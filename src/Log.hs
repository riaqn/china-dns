module Log where

import System.Log.Logger
import System.Log.Handler hiding (setLevel)
import System.Log.Handler.Simple
import System.Log.Formatter

import System.IO

setup :: IO ()
setup = do
  h <- streamHandler stderr DEBUG >>= \lh -> return $ setFormatter lh $
    simpleLogFormatter "[$tid : $loggername] $msg"
    
  updateGlobalLogger rootLoggerName ((setLevel INFO) . setHandlers [h])
