module Log where

import System.Log.Logger
import System.Log.Handler hiding (setLevel)
import System.Log.Handler.Simple
import System.Log.Formatter

import System.IO

import System.Environment

import Parse
import Text.Parsec
import Data.Maybe

setup :: IO ()
setup = do
  env <- fromMaybe "" <$> lookupEnv "LOG"
  l <- case parse logline "<env>" env of
    Left e -> error $ show e 
    Right l -> return $ (rootLoggerName, INFO) : l
    
  h <- streamHandler stderr DEBUG >>= \lh -> return $ setFormatter lh $
    simpleLogFormatter "[$tid : $loggername] $msg"
    
  updateGlobalLogger rootLoggerName (setHandlers [h])
  mapM_ (\(n,p) -> updateGlobalLogger  n (setLevel p)) l
