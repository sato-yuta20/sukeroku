module Sukeroku.Command where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.Char (isSpace, isAlpha, isDigit, toLower)
import Data.Function (on)
import Text.Parsec
import Text.ParserCombinators.ReadPrec (readPrec_to_S, readP_to_Prec)
import Sukeroku.Chess


type RoomName   = String
type TableNum   = Int
type OfferNum   = Int
type UserName   = String

data UserStatus
    = UserStateNothing
    | UserStatePlaying
    | UserStateExamining
  deriving (Eq, Show)

data Variable
    = VariableTell   --(Proxy Bool)
    | VariableShout  --(Proxy Bool)
  deriving (Eq, Ord)

instance Show Variable where
  show VariableTell  = "tell"
  show VariableShout = "shout"

instance Read Variable where
  readsPrec = undefined
{--
  readsPrec = readPrec_to_S $ readP_to_Prec f
    where
      f i = choice
        [ string' "tell" >> eof >> return VariableTell
        , string' "shout" >> eof >> return VariableShout
        ]
--}

data OfferType
    = OfferChallenge
        { offertype_from :: UserName
        , offertype_to  :: UserName
        }
  deriving (Show)

data Command
    = CommandTellUser UserName String
    | CommandTellRoom RoomName String
    | CommandAddRoom RoomName
    | CommandRemoveRoom RoomName
    | CommandSetFlag Variable (Maybe Bool)
    | COmmandSetNum Variable (Maybe Int)
    | CommandSetString Variable (Maybe String)
    | CommandQuit
    | CommandShout String
    | CommandExamine
    | CommandUnexamine
    | CommandAlgNotation AlgNotation
    | CommandMatch UserName
    | CommandAccept (Maybe OfferNum)
    | CommandPending
  deriving (Show)


type ParserE a = ParsecT String () (Either String) a


parseUserInput :: String -> Either String Command
parseUserInput s =
    case runParserT allCommandP () "" s of
      Left  e -> Left e -- (head $ [msg | Message msg <-errorMessages e]++["error"])
      Right c -> case c of
                   Left e2  -> Left (show e2)
                   Right c2 -> Right c2


allCommandP :: ParserE Command
allCommandP = do
    try (choice $ map try
            [ quitP
            , tellP
            , addRoomP, removeRoomP
            , shoutP
            , examineP, unexamineP
            , setP
            , matchP
            , algNotationP
            , acceptP, pendingP
            ]
        )
      <|>
      unknownP

help "tell"  = "help tell"
help "shout" = "help shout"


-- stop parsing with error
throwP :: String -> ParserE a
throwP = lift . Left

-- case-insensitive string
string' :: String -> ParserE String
string' s =
    sequence [satisfy (((==)`on`toLower) c) | c <- s]

checkRoomNumberRange :: String -> Either String ()
checkRoomNumberRange num =
    case reads num of
      (n,_):_ | 0 <= n && n <= 255 -> Right ()
      _                            -> Left "The range of channels is 0 to 255."

checkArgcP :: String -> ParserE ()
checkArgcP msg =
    optional $ try $ spaces >> eof >> throwP msg

unknownP :: ParserE Command
unknownP = do
    spaces
    word <- many1 (satisfy (not.isSpace))
    throwP $ word ++ ": Command not found."

quitP :: ParserE Command
quitP = do
    spaces
    choice (map (try.string') ["quit", "bye", "exit"])
    spaces
    eof
    return CommandQuit

tellP :: ParserE Command
tellP = do
    skipMany space
    choice (map (try.string') ["tell", "t"])
    checkArgcP (help "tell")
    skipMany1 space
    do{ try(do name <- many1 letter
               checkArgcP (help "tell")
               skipMany1 space
               msg <- manyTill anyChar eof
               return (CommandTellUser (map toLower name) msg)
           )
        <|>
        try(do num <- many1 digit
               checkArgcP (help "tell")
               skipMany1 space
               msg <- manyTill anyChar eof
               lift $ checkRoomNumberRange num
               return (CommandTellRoom num msg)
           )
        <|>
        try(do str <- many1 (satisfy (not.isSpace))
               throwP $ "'" ++ str ++ "' is not a valid handle."
           )
        <|>
        throwP (help "tell")
      }


addRoomP :: ParserE Command
addRoomP = do
    spaces
    string' "+channel"
    checkArgcP (help "+channel")
    skipMany1 space
    do{ try(do num <- many1 digit
               lift $ checkRoomNumberRange num
               spaces
               eof
               return (CommandAddRoom num)
           )
        <|>
        try(do str <- many1 (satisfy (not.isSpace))
               throwP "The channel to add must be a number between 0 and 255."
           )
        <|>
        throwP (help "+channel")
      }

removeRoomP :: ParserE Command
removeRoomP = do
    spaces
    string' "-channel"
    checkArgcP (help "-channel")
    skipMany1 space
    do{ try(do num <- many1 digit
               lift $ checkRoomNumberRange num
               spaces
               eof
               return (CommandRemoveRoom num)
           )
        <|>
        try(do str <- many1 (satisfy (not.isSpace))
               throwP "The channel to add must be a number between 0 and 255."
           )
        <|>
        throwP (help "-channel")
      }

setP :: ParserE Command
setP = do
    spaces
    string' "set"
    checkArgcP (help "set")
    skipMany1 space
    do{ try(do var <- choice (map (try.string') ["tell", "shout"])
               skipMany1 space
               val <- choice
                      [ fmap (const $ Just True) (choice (map (try.string') ["true", "on", "1"]))
                      , fmap (const $ Just False) (choice (map (try.string') ["false", "off", "0"]))
                      , fmap (const Nothing) (spaces >> eof)
                      ]
               spaces
               eof
               return (CommandSetFlag (read var) val)
           )
        <|>
        try(do var <- oneOf "123456789"
               skipMany1 space
               val <- optionMaybe (manyTill anyChar eof)
               return (CommandSetString (read [var]) val)
           )
      }


shoutP :: ParserE Command
shoutP = do
    spaces
    choice (map (try.string') ["!", "shout"])
    checkArgcP (help "shout")
    skipMany1 space
    msg <- manyTill anyChar eof
    return $ CommandShout msg

examineP :: ParserE Command
examineP = do
    spaces
    string' "examine"
    spaces
    eof
    return CommandExamine

unexamineP :: ParserE Command
unexamineP = do
    spaces
    string' "unexamine"
    spaces
    eof
    return CommandUnexamine

algNotationP :: ParserE Command
algNotationP = do
    spaces
    move <- algNotP
    spaces
    eof
    return (CommandAlgNotation move)


matchP :: ParserE Command
matchP = do
    spaces
    string' "match"
    skipMany1 space
    name <- many1 letter
    spaces
    eof
    return (CommandMatch (map toLower name))


acceptP :: ParserE Command
acceptP = do
    spaces
    string' "accept"
    mnum <- option Nothing (fmap Just (skipMany1 space >> many1 digit))
    spaces
    eof
    return (CommandAccept (fmap read mnum))

pendingP :: ParserE Command
pendingP = do
    spaces
    string' "pending"
    spaces
    eof
    return CommandPending


pieceTypeP :: ParserE PieceType
pieceTypeP = do
    ty <- oneOf "KQRBNPkqrbnp"
    return $ case toLower ty of
             'k' -> King
             'q' -> Queen
             'r' -> Rook
             'b' -> Bishop
             'n' -> Knight
             'p' -> Pawn

algNotP :: ParserE AlgNotation
algNotP = do
    ty <- try pieceTypeP <|> return Pawn
    fromx <- option Nothing (fmap Just (satisfy isChessFile))
    fromy <- option Nothing (fmap Just (satisfy isChessRank))
    optional (string' "x")
    tox <- satisfy isChessFile
    toy <- satisfy isChessRank
    return AlgNotation
      { algnot_pieceType = ty
      , algnot_mFromFile = fmap chessFileToInt fromx
      , algnot_mFromRank = fmap chessRankToInt fromy
      , algnot_to        = (chessFileToInt tox, chessRankToInt toy)
      }

algShortCastNotP :: ParserE AlgNotation
algShortCastNotP = do
    choice (map string' ["0-0", "o-o"])
    return undefined -- TODO

algLongCastNotP :: ParserE AlgNotation
algLongCastNotP = do
    choice (map string' ["0-0-0", "o-o-o"])
    return undefined -- TODO


