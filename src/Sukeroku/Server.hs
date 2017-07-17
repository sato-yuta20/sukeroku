{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Sukeroku.Server where

import Control.Concurrent (yield)
import Control.Concurrent.MVar
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async (async, wait, race)
import Control.Exception (Exception, SomeException, throwIO, catch, bracket, finally)
import Control.Monad (join, forever, when, unless, replicateM)
import Control.Monad.STM (STM, orElse, atomically, retry)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)

import Data.Either (either)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List as List
import Data.Char (toLower)
import Data.Foldable (foldrM)
import Data.Maybe (mapMaybe, fromMaybe, fromJust)

import System.Random (newStdGen, randomRs)
import System.IO

import Sukeroku.Chess
import Sukeroku.Command


logger :: Server -> IO ()
logger server@Server{..} = do
    msg <- readChan server_logChan
    case msg of
      "STOP" -> do
        return ()
      _      -> do
        putStrLn msg
        logger server


logging :: Server -> String -> IO ()
logging server@Server{..} msg = do
    writeChan server_logChan msg


start :: Server -> Handle -> IO ()
start server handle = do
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    muser <- login server handle
    case muser of
      Nothing   -> return ()
      Just user -> runUser server user


runUser :: Server -> User -> IO ()
runUser server@Server{..} user@User{..} = do
    race msghandler receive
    return ()
  where
    receive = forever $ do
        outputPrompt user
        inp <- inputStr user
        logging server ("Received from " ++ user_name ++ ": " ++ inp)
        inpReplaced <- replaceAlias server user inp
        logging server ("Replaced: " ++ inpReplaced)
        case parseUserInput inpReplaced of
          Left err  -> do
            outputStrLn user err
          Right msg -> do
            atomically (sendEventToUser user (EventInput msg))
            yield

    msghandler = join $ atomically $ do
        svRooms <- readTVar server_rooms
        usRooms <- readTVar user_rooms
        msg <- List.foldr orElse retry $ map readTChan
                (user_sendChan:[room_sendChan room | room <- mapMaybe (`Map.lookup`svRooms) usRooms])
        return $ do
            continue <- handleEvent server user msg
            when continue msghandler


replaceAlias :: Server -> User -> String -> IO String
replaceAlias _ _ [] = return []
replaceAlias server@Server{..} user@User{..} inp
  | null ws' = return ""
  | otherwise = atomically $ do
      aliases <- readTVar user_aliases
      case Map.lookup w aliases of
        Nothing -> return inp
        Just a  -> return (subst a)
  where
    ws'     = words inp
    (w,ws)  = (head ws', tail ws')

    subst as =
      case as of
        []             -> []
        ('$':'$':xs)   -> '$' : subst xs
        ('$':'@':xs)   -> (unwords ws) ++ subst xs
        ('$':x:xs)
          | '1' <= x && x <= '9' && length ws >= d
                       -> (ws!!(d-1)) ++ subst xs where d = read[x]
        ('$':'-':x:xs)
          | '1' <= x && x <= '9' && length ws >= d
                       -> (unwords (take d ws)) ++ subst xs where d = read[x]
        ('$':x:'-':xs)
          | '1' <= x && x <= '9' && length ws >= d
                       -> (unwords (drop (d-1) ws)) ++ subst xs where d = read[x]
        (x:xs)         -> x : subst xs


handleEvent :: Server -> User -> ICSEvent -> IO Bool
handleEvent server@Server{..} user@User{..} ev =
    case ev of
      EventNotice s -> do
        outputStrLn user s
        return True
      EventShout name s -> do
        outputStrLn user (name ++ " shouts: " ++ s)
        return True
      EventTellUser name s -> do
        outputStrLn user (name ++ " tells you: " ++ s)
        return True
      EventTellRoom name roomname s -> do
        outputStrLn user (name ++ "(" ++ roomname ++ "): " ++ s)
        return True
      EventDisplayBoard table@Table{..} -> do
        tblGame <- readTVarIO table_game
        outputStrLn user (showBoard (game_board tblGame))
        return True
      EventInput (CommandQuit) -> do
        removeUser server user_name
        return False
      EventInput (CommandTellUser who what) -> do
        tellUser server user who what
        return True
      EventInput (CommandTellRoom num what) -> do
        tellRoom server user num what
        return True
      EventInput (CommandShout s) -> do
        shout server user s
        return True
      EventInput (CommandSetFlag var mval) -> do
        setFlag server user var mval
        return True
      EventInput (CommandAddRoom num) -> do
        addChannel server user num
        return True
      EventInput (CommandRemoveRoom num) -> do
        removeChannel server user num
        return True
      EventInput (CommandExamine) -> do
        examine server user
        return True
      EventInput (CommandUnexamine) -> do
        unexamine server user
        return True
      EventInput (CommandAlgNotation algnot) -> do
        algNotation server user algnot
        return True
      EventInput (CommandMatch name) -> do
        match server user name
        return True
      EventInput (CommandAccept mnum) -> do
        accept server user mnum
        return True
      EventInput (CommandPending) -> do
        pending server user
        return True
      _ -> do
        return True


login :: Server -> Handle -> IO (Maybe User)
login server@Server{..} handle = do
     hPutStr handle "login: "
     hFlush handle
     inp <- hGetLine handle
     case inp of
       ""   -> do
         hPutStrLn handle $
           "\n"
           ++ "If you are not a registered player, enter guest or a unique ID\n"
           ++ "(If your return key does not work, use cntrl-J)\n"
         login server handle
       _ -> do
         name <- if (map toLower inp)`elem`["g","guest"]
                      then newGuestName server else return inp
         svUsers <- readTVarIO server_users
         if Map.member name svUsers
           then do
             hPutStr handle $ "\nName \"" ++ name ++ "\" is already in use.\n"
             login server handle
           else do
             mvar <- newMVar False
             user@User{..} <- atomically $ do
                 user <- newUser name handle mvar
                 writeTVar server_users $ Map.insert (map toLower name) user svUsers
                 return user
             hPutStr handle $
               "\n"
               ++ "Logging you in as \"" ++ name ++ "\"; you may use this name to play unrated games.\n"
               ++ "(After logging in, do \"help register\" for more info on how to register.)\n"
               ++ "\n"
               ++ "Press return to enter the server as \"" ++ name ++ "\": "
             hFlush handle
             enter <- hGetLine handle
             return (Just user)


newGuestName :: Server -> IO String
newGuestName server@Server{..} = do
    svUsers <- readTVarIO server_users
    svReserved <- takeMVar server_reservedNames
    g <- newStdGen
    randomName svUsers svReserved g
  where
    randomName names reserved gen = do
        let name = "Guest" ++ take 4 (randomRs ('A','Z') gen)
        if (Map.member name names) || (Set.member name reserved)
          then randomName names reserved gen
          else do
            putMVar server_reservedNames (Set.insert name reserved)
            return name


removeUser :: Server -> UserName -> IO ()
removeUser server@Server{..} name = atomically $ do
    modifyTVar' server_users $ Map.delete name


sendEventToUser :: User -> ICSEvent -> STM ()
sendEventToUser user@User{..} ev =
    writeTChan user_sendChan ev


sendEventToRoom :: Room -> ICSEvent -> STM ()
sendEventToRoom room@Room{..} ev =
    writeTChan room_sendChan ev


sendEventToUserName :: Server -> UserName -> ICSEvent -> ExceptT String STM ()
sendEventToUserName server@Server{..} name ev = do
    svUsers <- lift $ readTVar server_users
    user <- maybe (throwE $ name ++ " not logged in.") return (Map.lookup name svUsers)
    lift $ sendEventToUser user ev
    return ()


inputStr :: User -> IO String
inputStr user@User{..} = do
    inp <- hGetLine user_handle
    usPrompt <- takeMVar user_prompt
    putMVar user_prompt False
    if null inp
      then do
        outputPrompt user
        inputStr user
      else do
        return inp


outputStrLn :: User -> String -> IO ()
outputStrLn user@User{..} str = do
    usPrompt <- takeMVar user_prompt
    when usPrompt (hPutStrLn user_handle "")
    hPutStrLn user_handle str
    putMVar user_prompt False


outputPrompt :: User -> IO ()
outputPrompt user@User{..} = do
    usPrompt <- takeMVar user_prompt
    unless usPrompt $ do
        hPutStr user_handle "jics% "
        hFlush user_handle
    putMVar user_prompt True


tellUser :: Server -> User -> UserName -> String -> IO ()
tellUser server@Server{..} user@User{..} who msg = do
    r <- atomically $ runExceptT $ do
        svUsers <- lift $ readTVar server_users
        her@User{user_name=her_name}
            <- maybe (throwE $ who ++ " is not logged in.") return (Map.lookup who svUsers)
        herTellOn <- lift $ readUserVar VariableTell her
        when (user_isGuest && herTellOn)
            (throwE $ "Player \"" ++ user_name ++ "\" isn't listening to unregistered user's tells.")
        lift $ sendEventToUser her (EventTellUser user_name msg)
        return $"(told " ++ her_name ++ ")"
    either (outputStrLn user) (outputStrLn user) r


tellRoom :: Server -> User -> RoomName -> String -> IO ()
tellRoom server@Server{..} user@User{..} roomname msg = do
    r <- atomically $ runExceptT $ do
        svRooms <- lift $ readTVar server_rooms
        room@Room{..} <- maybe (throwE $ "channel " ++ roomname ++ " not found.") return (Map.lookup roomname svRooms)
        lift $ sendEventToRoom room (EventTellRoom user_name roomname msg)
        rmUsers <- lift $ readTVar room_users
        return ("(told " ++ show (Set.size rmUsers) ++ " users in channel " ++ room_name ++ ")")
    either (outputStrLn user) (outputStrLn user) r


shout :: Server -> User -> String -> IO ()
shout server@Server{..} user@User{..} msg = do
    r <- atomically $ runExceptT $ do
        svUsers <- lift $ readTVar server_users
        nshouted <- lift $ foldrM f 0 svUsers
        return ("(shouted to " ++ show nshouted ++ " players)")
    either (outputStrLn user) (outputStrLn user) r
  where
    f user@User{user_name = name} count
      | name == user_name = return count
      | otherwise        = sendEventToUser user (EventShout user_name msg) >> return (count+1)


addChannel :: Server -> User -> RoomName -> IO ()
addChannel server@Server{..} user@User{..} name = do
    r <- atomically $ runExceptT $ do
        svRooms <- lift $ readTVar server_rooms
        room@Room{..} <- maybe (throwE $ "") return (Map.lookup name svRooms)
        rmUsers <- lift $ readTVar room_users
        when (Set.member user_name rmUsers)
            (throwE $ "[" ++ room_name ++ "] is already on your channel list.")
        lift $ writeTVar room_users (Set.insert user_name rmUsers)
        userRooms <- lift $ readTVar user_rooms
        lift $ writeTVar user_rooms (room_name:userRooms)
        return ("[" ++ room_name ++ "] added to your channel.")
    either (outputStrLn user) (outputStrLn user) r


removeChannel :: Server -> User -> String -> IO ()
removeChannel server@Server{..} user@User{..} name = do
    r <- atomically $ runExceptT $ do
        svRooms <- lift $ readTVar server_rooms
        room@Room{..} <- maybe (throwE $ "") return (Map.lookup name svRooms)
        rmUsers <- lift $ readTVar room_users
        when (Set.notMember user_name rmUsers)
            (throwE $ "[" ++ room_name ++ "] is not in your channel list.")
        lift $ writeTVar room_users (Set.delete user_name rmUsers)
        userRooms <- lift $ readTVar user_rooms
        lift $ writeTVar user_rooms (List.delete room_name userRooms)
        return ("[" ++ room_name ++ "] removed from your channel list.")
    either (outputStrLn user) (outputStrLn user) r


setFlag :: Server -> User -> Variable -> Maybe Bool -> IO ()
setFlag server@Server{..} user@User{..} var mval = do
    r <- atomically $ runExceptT $ do
        userVariables <- lift $ readTVar user_variables
        when (Map.member var userVariables)
            (throwE $ "no such variable \"" ++ show var ++ "\".")
        lift $ writeTVar user_variables (Map.adjust (show . change mval) var userVariables)
        val' <- lift $ readUserVar var user
        return $ msg var val'
    either (outputStrLn user) (outputStrLn user) r
  where
    change (Just True)  = const True
    change (Just False) = const False
    change Nothing      = not.read

    msg VariableTell  True   = "You will now hear direct tells from all users."
    msg VariableTell  False  = "You will not hear direct tells from unregistered users."
    msg VariableShout True   = "You will now hear shouts."
    msg VariableShout False  = "You will not hear shouts."

examine :: Server -> User -> IO ()
examine server@Server{..} user@User{..} = do
    r <- atomically $ runExceptT $ do
        svTables <- lift $ readTVar server_tables
        svTabnums <- lift $ readTVar server_nextTableNums
        let tabnum = if Set.null svTabnums
                       then fst (Map.findMax svTables) + 1
                       else Set.findMin svTabnums
        table@Table{..} <- lift $ newTable tabnum user_name user_name
        lift $ writeTVar server_tables (Map.insert tabnum table svTables)
        lift $ writeTVar server_nextTableNums (Set.delete tabnum svTabnums)
        userStatus <- lift $ readTVar user_status
        when (userStatus /= UserStateNothing)
            (throwE $ "already playing or examining.")
        lift $ writeTVar user_playing (Just tabnum)
        lift $ writeTVar user_status UserStateExamining
        lift $ sendEventToUser user (EventDisplayBoard table)
        return $ "table #" ++ show table_number ++ " created."
    either (outputStrLn user) (outputStrLn user) r


unexamine :: Server -> User -> IO ()
unexamine server@Server{..} user@User{..} = do
    r <- atomically $ runExceptT $ do
        -- TODO handling a case of Nothing
        userStatus <- lift $ readTVar user_status
        when (userStatus /= UserStateExamining)
            (throwE $ "not examining.")
        muserPlaying <- lift $ readTVar user_playing
        userPlaying <- maybe (throwE $ "system error?") return muserPlaying
        svTables <- lift $ readTVar server_tables
        table@Table{..} <- maybe (throwE $ "not found table #" ++ show userPlaying)
                                 return (Map.lookup userPlaying svTables)
        svTabnums <- lift $ readTVar server_nextTableNums
        lift $ writeTVar server_tables (Map.delete table_number svTables)
        lift $ writeTVar server_nextTableNums (Set.insert table_number svTabnums)
        lift $ writeTVar user_playing Nothing
        lift $ writeTVar user_status UserStateNothing
        return $ "table #" ++ show table_number ++ " deleted."
    either (outputStrLn user) (outputStrLn user) r


algNotation :: Server -> User -> AlgNotation -> IO ()
algNotation server@Server{..} user@User{..} algnot = do
    r <- atomically $ runExceptT $ do
        muserPlaying <- lift $ readTVar user_playing
        userPlaying <- maybe (throwE $ "not playing.") return muserPlaying
        svTables <- lift $ readTVar server_tables
        table@Table{..} <- maybe (throwE $ "system error?") return (Map.lookup userPlaying svTables)
        game@Game{..} <- lift $ readTVar table_game
        wplayer <- lift $ readTVar table_whitePlayer
        bplayer <- lift $ readTVar table_blackPlayer
        let player = if game_nextToMove==White then wplayer else bplayer
        let oppname= if bplayer == user_name then wplayer else bplayer
        when (player /= user_name) (throwE $ "not in your turn.")
        move <- maybe (throwE $ "illegal move.") return (algToMove game_nextToMove algnot game_board)
        let game' = stepForward move game
        lift $ writeTVar table_game game'
        lift $ sendEventToUser user (EventDisplayBoard table)
        sendEventToUserName server oppname (EventDisplayBoard table)
        return ()
    either (outputStrLn user) return r


match :: Server -> User -> UserName -> IO ()
match server@Server{..} user@User{..} name = do
    r <- atomically $ runExceptT $ do
        userStatus <- lift $ readTVar user_status
        when (userStatus /= UserStateNothing) (throwE $ "already plyaing or examining.")
        svUsers <- lift $ readTVar server_users
        opp@User{user_playing=oppPlaying, user_status=oppStatus, user_name=oppName} <-
                maybe (throwE $ name ++ " not logged in.") return (Map.lookup name svUsers)
        svOffers <- lift $ readTVar server_offers
        svOffnums <- lift $ readTVar server_nextOfferNums
        let offnum = if Set.null svOffnums
                         then fst (Map.findMax svOffers) + 1
                         else Set.findMin svOffnums
        let offer = newOffer offnum user_name oppName
        lift $ writeTVar server_offers (Map.insert offnum offer svOffers)
        lift $ writeTVar server_nextOfferNums (Set.delete offnum svOffnums)
        return $ "offer #" ++ show offnum ++ " created."
    either (outputStrLn user) (outputStrLn user) r


accept :: Server -> User -> Maybe OfferNum -> IO ()
accept server@Server{..} user@User{..} moffnum = do
    r <- atomically $ runExceptT $ do
        svOffers <- lift $ readTVar server_offers
        let offered = filter (\x-> user_name==offer_to x) (Map.elems svOffers)
        offnum <- case moffnum of
                    Nothing | length offered == 1 -> return $ offer_number (head offered)
                            | otherwise           -> throwE $ "no pendings."
                    Just n                        -> return n
        offer@Offer{..} <- maybe (throwE $ "not found offer #" ++ show offnum)
                           return (List.find (\x ->offnum==offer_number x) offered)
        svUsers <- lift $ readTVar server_users
        opp@User{user_playing=oppPlaying, user_status=oppStatus, user_name=oppName} <-
                maybe (throwE $ offer_from ++ " not logged in.") return (Map.lookup offer_from svUsers)
        svTables <- lift $ readTVar server_tables
        svTabnums <- lift $ readTVar server_nextTableNums
        svOffnums <- lift $ readTVar server_nextOfferNums
        let tabnum = if Set.null svTabnums
                       then fst (Map.findMax svTables) + 1
                       else Set.findMin svTabnums
        table@Table{..} <- lift $ newTable tabnum user_name oppName
        lift $ writeTVar server_tables (Map.insert tabnum table svTables)
        lift $ writeTVar server_nextTableNums (Set.delete tabnum svTabnums)
        lift $ writeTVar server_offers (Map.delete offnum svOffers)
        lift $ writeTVar server_nextOfferNums (Set.insert offnum svOffnums)
        -- TODO check whether already playing
        lift $ writeTVar user_playing (Just tabnum)
        lift $ writeTVar user_status UserStatePlaying
        lift $ writeTVar oppPlaying (Just tabnum)
        lift $ writeTVar oppStatus UserStatePlaying
        lift $ sendEventToUser user (EventDisplayBoard table)
        lift $ sendEventToUser opp (EventDisplayBoard table)
        return $ "table #" ++ show table_number ++ " created."
    either (outputStrLn user) (outputStrLn user) r


pending :: Server -> User -> IO ()
pending server@Server{..} user@User{..} = do
    svOffers <- readTVarIO server_offers
    outputStrLn user "Offers to other players:"
    mapM_ f (filter ((==user_name).offer_from) (Map.elems svOffers))
    outputStrLn user "Offers from other players:"
    mapM_ f (filter ((==user_name).offer_to) (Map.elems svOffers))
  where
    f offer@Offer{..} = outputStrLn user
            ("#" ++ show offer_number ++ " challenge: " ++ offer_from ++ " " ++ offer_to)


data ICSEvent
    = EventNotice String
    | EventShout UserName String
    | EventTellUser UserName String
    | EventTellRoom UserName RoomName String
    | EventDisplayBoard Table
    | EventInput Command
    | EventPending Offer


data Room = Room
  { room_name     :: RoomName
  , room_users  :: TVar (Set.Set UserName)
  , room_sendChan :: TChan ICSEvent
  }

newRoom :: RoomName -> IO Room
newRoom name = do
    sendChan <- newTChanIO
    users    <- newTVarIO Set.empty
    return Room
      { room_name      = name
      , room_users     = users
      , room_sendChan  = sendChan
      }


data Table = Table
  { table_number       :: TableNum
  , table_whitePlayer  :: TVar UserName
  , table_blackPlayer  :: TVar UserName
  , table_observers    :: TVar (Set.Set UserName)
  , table_game         :: TVar Game
  }

newTable :: TableNum -> UserName -> UserName -> STM Table
newTable num wplayer bplayer = do
    whitePlayer  <- newTVar wplayer
    blackPlayer  <- newTVar bplayer
    observers    <- newTVar (Set.empty)
    game         <- newTVar newInitialGame
    return Table
      { table_number            = num
      , table_whitePlayer       = whitePlayer
      , table_blackPlayer       = blackPlayer
      , table_observers         = observers
      , table_game              = game
      }


data Offer = Offer
    { offer_number         :: OfferNum
    , offer_type           :: OfferType
    , offer_from           :: UserName
    , offer_to             :: UserName
    }
  deriving (Show)

newOffer :: OfferNum -> UserName -> UserName -> Offer
newOffer num from to = Offer
  { offer_number = num
  , offer_type   = OfferChallenge from to
  , offer_from   = from
  , offer_to     = to
  }

data User = User
  { user_name          :: UserName
  , user_handle        :: Handle
  , user_prompt        :: MVar Bool
  , user_sendChan      :: TChan ICSEvent
  , user_status        :: TVar UserStatus
  , user_isGuest       :: Bool
  , user_rooms         :: TVar [RoomName]
  , user_tables        :: TVar [TableNum]
  , user_playing       :: TVar (Maybe TableNum)
  , user_variables     :: TVar (Map.Map Variable String)
  , user_aliases       :: TVar (Map.Map String String)
  }

newUser :: UserName -> Handle -> MVar Bool -> STM User
newUser name handle mvar = do
    sendChan  <- newTChan
    status    <- newTVar UserStateNothing
    rooms     <- newTVar []
    tables    <- newTVar []
    playing   <- newTVar Nothing
    variables <- newTVar (Map.fromList [(VariableTell,show True),(VariableShout,show True)])
    aliases   <- newTVar (Map.fromList [("aa", "unko 1=$1 2=$2")])
    return User
       { user_name      = name
       , user_handle    = handle
       , user_prompt    = mvar
       , user_sendChan  = sendChan
       , user_status    = status
       , user_isGuest   = True
       , user_rooms     = rooms
       , user_tables    = tables
       , user_playing   = playing
       , user_variables = variables
       , user_aliases   = aliases
       }

readUserVar :: Read a => Variable -> User -> STM a
readUserVar var user@User{..} = do
    usVariables <- readTVar user_variables
    return $ read $ fromJust $ Map.lookup var usVariables


data Server = Server
  { server_users              :: TVar (Map.Map UserName User)
  , server_rooms              :: TVar (Map.Map RoomName Room)
  , server_tables             :: TVar (Map.Map TableNum Table)
  , server_nextTableNums      :: TVar (Set.Set TableNum)
  , server_offers             :: TVar (Map.Map OfferNum Offer)
  , server_nextOfferNums      :: TVar (Set.Set OfferNum)
  , server_reservedNames      :: MVar (Set.Set UserName)
  , server_logChan            :: Chan String
  }


newServer :: IO Server
newServer = do
    users    <- newTVarIO Map.empty
    rooms'     <- initialRooms
    rooms      <- newTVarIO (Map.fromList rooms')
    tables     <- newTVarIO Map.empty
    tablenums  <- newTVarIO (Set.fromList [0..100])
    offers     <- newTVarIO Map.empty
    offernums  <- newTVarIO (Set.fromList [0..100])
    reserved   <- newMVar Set.empty
    logChan    <- newChan
    return Server
      { server_users              = users
      , server_rooms              = rooms
      , server_tables             = tables
      , server_nextTableNums      = tablenums
      , server_offers             = offers
      , server_nextOfferNums      = offernums
      , server_reservedNames      = reserved
      , server_logChan            = logChan
      }
  where
    initialRoomNames :: [RoomName]
    initialRoomNames = map show [0..255]

    initialRooms :: IO [(RoomName,Room)]
    initialRooms = do
      rs <- mapM newRoom initialRoomNames
      return (zip initialRoomNames rs)
