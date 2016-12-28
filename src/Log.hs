module Log where

import System.Log.Logger
import System.Log.Handler hiding (setLevel)
import System.Log.Handler.Simple
import System.Log.Formatter

import System.IO

import System.Environment
import Text.Read

import Control.Monad.Trans.Maybe

setup :: IO ()
setup = do

  m <- runMaybeT $ do
    x <- MaybeT $ lookupEnv "LOG"
    MaybeT $ return $ readMaybe x
  let l = maybe INFO (id) m
  h <- streamHandler stderr l >>= \lh -> return $ setFormatter lh $
    simpleLogFormatter "[$tid : $loggername] $msg"
    
  updateGlobalLogger rootLoggerName ((setLevel l) . setHandlers [h])
