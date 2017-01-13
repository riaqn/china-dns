module ZhinaDNS where

import qualified Resolve.Types as R
import Resolve.DNS.Types hiding (Query, Response)
import Resolve.DNS.Lookup hiding (Config) 
import qualified IPSet
import qualified NameSet

import Control.Monad
import Control.Monad.Trans.Except
import Control.Concurrent
import Control.Exception
import Control.Monad.Trans.Class

import System.Log.Logger

nameM = "ZhinaDNS"

data Config = Config { china :: R.Resolve Query Response
                     , world :: R.Resolve Query Response
                     , chinaIP :: IPSet.IPSet IPv4
                     , worldName :: NameSet.NameSet 
                     }


resolve :: Config -> R.Resolve Query Response
resolve c a = do
  let nameF = nameM ++ ".resolve"
  m_china <- newEmptyMVar
  m_world <- newEmptyMVar

  let isForeignName = NameSet.test (worldName c) (qname $ qquestion a)

  bracket
    (do
      t_china <- forkIO $ when (not isForeignName) $ putMVar m_china =<< try (china c a)
      t_world <- forkIO $ putMVar m_world =<< try (world c a)
      return (t_china, t_world)
    )
    (\(t_china, t_world) -> do
      killThread t_china
      killThread t_world
    )
    (\_ -> either id id <$> (runExceptT $ 
              do
                b_china' <- if isForeignName then do
                  lift $ debugM nameF "foreign name detected, using only foreign DNS"
                  return Nothing
                  else do 
                  b_china' <- lift $ takeMVar m_china
                  b_china <- case b_china' of
                    Left e -> do
                      lift $ debugM nameF $ "zhina: " ++ show (e :: SomeException)
                      lift $ throwIO e
                    Right b' -> return b'

                  let isForeignIP rdata' = case rdata' of
                        RR_A ip -> not $ IPSet.test (chinaIP c) ip
                        _ -> False

                  if any (\rr -> isForeignIP (rdata rr)) (ranswer b_china) then do
                    lift $ debugM nameF "foreign results detected, waiting for foreign DNS"
                    return Nothing
                    else return $ Just b_china

                case b_china' of
                  Nothing -> do 
                    b_world' <- lift $ takeMVar m_world
                    case b_world' of
                      Left e -> do
                        lift $ debugM nameF $ "world: " ++ show (e :: SomeException)
                        lift $ throwIO e 
                      Right b' -> return b'
                  Just b_china -> return b_china
    ))
