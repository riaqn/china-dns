module Log where

import System.Log.Logger
import System.Log.Handler hiding (setLevel)
import System.Log.Handler.Simple
import System.Log.Formatter

import System.IO

setup :: [(String, Priority)] -> IO ()
setup l = do
  h <- streamHandler stderr DEBUG >>= \lh -> return $ setFormatter lh $
    simpleLogFormatter "[$tid : $loggername] $msg"
    
  updateGlobalLogger rootLoggerName (setHandlers [h])
  mapM_ (\(n,p) -> updateGlobalLogger  n (setLevel p)) l
