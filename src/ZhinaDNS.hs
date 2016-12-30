module ZhinaDNS where

import qualified Resolve.Types as T
import Resolve.DNS.Types
import qualified Resolve.DNS.Channel as C
import IPSet

import Control.Monad
import Control.Monad.Trans.Except
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Trans.Class
import Data.Either

import System.Log.Logger

nameM = "ChinaDNS"

data Config = Config { china :: T.Resolve Message Message
                     , world :: T.Resolve Message Message
                     , chinaIP :: IPSet IPv4
                     }


resolve :: Config -> T.Resolve Message Message
resolve c a = do
  let nameF = nameM ++ ".resolve"
  m_china <- newEmptyMVar
  m_world <- newEmptyMVar

  let errRep a = Message { header = (header a)  { qr = Response
                                                , tc = False
                                                , ra = True
                                                , zero = 0
                                                , rcode = ServFail
                                                }
                         , question = question a
                         , answer = []
                         , authority = []
                         , additional = []
                         }

  bracket
    (do 
      t_china <- forkIO $ putMVar m_china =<< try (china c a)
      t_world <- forkIO $ putMVar m_world =<< try (world c a)
      return (t_china, t_world)
    )
    (\(t_china, t_world) -> do
      killThread t_china
      killThread t_world
    )
    (\_ -> let left e = case e of
                 Left a -> a
           in left <$> (runExceptT $
              do 
                b_china <- lift $ takeMVar m_china
                b_china' <- case b_china of
                  Left e -> do
                    let x = e :: SomeException
                    lift $ errorM nameF $ show e
                    throwE $ errRep a
                  Right b' -> return b'

                when ((rcode $ header b_china') /= NoErr) $ throwE $ errRep a

                let isForeign rdata = case rdata of
                      RR_A ip -> not $ test (chinaIP c) ip
                      _ -> False
                b_final <- if any (\rr -> isForeign (rdata rr)) (answer b_china') then do
                  lift $ debugM nameF "foreign results detected, waiting for foreign DNS"
                  b_world <- lift $ takeMVar m_world
                  b_world' <- case b_world of
                    Left e -> do
                      let x = e :: SomeException
                      lift $ errorM nameF $ show e
                      throwE $ errRep a
                    Right b' -> return b'
                  when ((rcode $ header b_world')  /= NoErr) $ throwE $ errRep a

                  return b_world'
                  else
                  return b_china'

                throwE $ b_final {header = (header b_final) {ident = ident $ header $ a}}
    ))
