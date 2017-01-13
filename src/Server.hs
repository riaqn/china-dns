module Server where

import Resolve.Types
import Resolve.DNS.Types
import qualified Resolve.DNS.Lookup as L

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

import qualified Resolve.DNS.Encode as E
import qualified Resolve.DNS.Decode as D

import Control.Monad
import Control.Exception

import Data.Word
import Data.Typeable

import System.Log.Logger

data Error = WierdQuery String
           | ResponseTooLong
  deriving (Typeable, Show)

instance Exception Error

nameM = "Server"

data Config = Config { back :: Resolve L.Query L.Response
                     , is_udp :: Bool
                     }

server :: Config -> Resolve ByteString ByteString
server c bs_a = do 
  m_a <- case D.decodeMessage (BSL.toStrict bs_a)  of
    Left e -> throwIO $ D.Error e
    Right m -> return m

  let h = header m_a
  when (qr h /= Query ||
        opcode h /= STD ||
        aa h == True ||
        tc h == True ||
        ra h == True ||
        rcode h /= NoErr 
       ) $ do
    throwIO $ WierdQuery "Some fields are wierd"

  
  when ((not $ null $ answer m_a) ||
        (not $ null $ authority m_a) ||
        (not $ null $ additional m_a) ) $ do
    throwIO $ WierdQuery "some sections should be empty"

  q <- case question m_a of
    [] -> throwIO $ WierdQuery "zero question"
    [q] -> return q
    _ -> throwIO $ WierdQuery "multiple questions"

  let a = L.Query { L.qquestion = q
                  , L.qopt = [] 
                  }
          
  b' <- try (back c a)
  m_b <- case b' of
    Left e -> do
      debugM nameM $ show (e :: SomeException)
      return $ Message { header = (header m_a) { qr = Response
                                               , rcode = ServFail
                                               , ra = True
                                               , zero = 0
                                               }
                    , question = question m_a
                    , answer = []
                    , authority = []
                    , additional = []
                    , opt = Nothing
                    }
    Right b -> return $ Message { header = (header m_a) { qr = Response
                                            , aa = False
                                            , ra = True
                                            , zero = 0
                                            }
                    , question = question m_a
                    , answer = L.ranswer b
                    , authority = L.rauthority b
                    , additional = L.radditional b
                    , opt = Nothing
                    }

  bs_b <- case E.encode E.message m_b of
    Left e -> throwIO e
    Right bs -> return bs
  
  bs_b' <- runExceptT $ do
    this <- case is_udp c of
      False -> throwE bs_b
      True -> return (512 :: Word16)
    lift $ debugM nameM $ "payload on this side is " ++ (show this)
    when (BSL.null (BSL.drop (fromIntegral this) bs_b)) $ throwE bs_b
    lift $ debugM nameM "response too long, setting TC bit.."
    let m_b' = m_b { header = (header m_b) { tc = True}
                   , answer = []
                   , authority = []
                   , additional = []
                   , opt = Nothing
                   }
    bs_b' <- case E.encode E.message m_b' of
      Left e -> lift $ throwIO e
      Right bs -> return bs
    when (BSL.null $ BSL.drop (fromIntegral this) bs_b') $ throwE bs_b'

  case bs_b' of
    Left bs_b -> return bs_b
    Right _ -> throwIO $ ResponseTooLong
