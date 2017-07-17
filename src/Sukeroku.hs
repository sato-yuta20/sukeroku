{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

import Network.Socket hiding (recv)

import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (async, wait, race)
import Control.Monad (forever)
import Control.Exception (Exception, SomeException, throwIO, catch, bracket, finally)

import System.IO
import System.Environment
import System.Console.GetOpt

import Sukeroku.Server hiding (accept)


main :: IO ()
main = do
    eopts <- fmap parseOptions getArgs
    case eopts of
      Left errs  -> do
        mapM putStrLn errs
        putStrLn (usageInfo "Usage" commandOptions)
      Right opts -> do
        -- TODO read config file
        --
        withSocketsDo $ do
          server <- newServer
          stoplogger <- async (logger server)
          addrinfos <- getAddrInfo
                       (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                       Nothing (Just "5000")
          let serveraddr = head addrinfos
          bracket
            (socket (addrFamily serveraddr) Stream defaultProtocol)
            (\sock -> do
                close sock
                logging server "socket closed"
                logging server "STOP"
                wait stoplogger
            )
            (\sock -> do
                bind sock (addrAddress serveraddr)
                listen sock 1
                forever $ do
                    bracket
                      (accept sock)
                      (\_ -> return ())
                      (\(conn, sockaddr) -> do
                          handle <- socketToHandle conn ReadWriteMode
                          logging server ("Accepted connection from " ++ show sockaddr)
                          forkFinally
                            (start server handle `catch` (\e -> print (e :: SomeException) >> throwIO e))
                            (\_ -> hClose handle)
                      )
            )


data Options = Options
  { optVersion     :: Bool
  , optConfigFile  :: Maybe FilePath
  , optLogFile     :: Maybe FilePath
  }

commandOptions :: [OptDescr (Options -> Options)]
commandOptions =
  [ Option ['v'] ["version"] (NoArg (\opts -> opts {optVersion = True})) "show version"
  , Option ['c'] ["config"] (ReqArg (\f opts -> opts {optConfigFile = Just f}) "FILE") "config file path"
  , Option ['l'] ["log"] (ReqArg (\f opts -> opts {optLogFile = Just f}) "FILE") "log file path"
  ]

defaultOptions :: Options
defaultOptions = Options
  { optVersion = False
  , optConfigFile = Nothing
  , optLogFile = Nothing
  }

parseOptions :: [String] -> Either [String] Options
parseOptions args =
  case getOpt Permute commandOptions args of
    (os,ns,[]) -> Right (foldl (flip id) defaultOptions os)
    (_,_,errs) -> Left errs


