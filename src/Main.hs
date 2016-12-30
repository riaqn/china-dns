module Main where

import qualified ZhinaDNS as ZDNS
import IPSet
import Text.Parsec
import Parse
import qualified Log

import System.IO
import System.Log.Logger

import Network.Socket hiding (recv, recvFrom, send, sendTo)
import Network.Socket.ByteString 
import qualified Network.Socket.ByteString.Lazy as SL


import qualified Resolve.Types as R
import qualified Resolve.DNS.Channel as C


import qualified Resolve.DNS.Helper.DNS as DNS
import qualified Resolve.DNS.Helper.UDP as UDP
import qualified Resolve.DNS.Helper.LiveTCP as TCP

import Resolve.Timeout
import Resolve.Retry
import qualified Resolve.DNS.Types as T
import qualified Resolve.DNS.Encode as E
import qualified Resolve.DNS.Decode as D
import qualified Resolve.DNS.Truncation as Truncation

import qualified Resolve.DNS.Server.UDP as SUDP
import qualified Resolve.DNS.Server.TCP as STCP

import qualified  Data.Attoparsec.ByteString as AP
import Data.Attoparsec.Binary
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.Maybe

import qualified Data.ByteString as BS

import Control.Monad
import Control.Monad.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

import System.Environment

import Control.Exception


nameM = "Main"

readChinaIP :: Handle -> IO (Either String [IPRange IPv4])
readChinaIP h = helper 0 []
  where helper i t = do
          done <- hIsEOF h
          if done then do
            return $ Right t
            else do
            l <- hGetLine h
            case parse (line i) "" l of
              Left e -> return $ Left $ show e
              Right ip' -> helper (i +1) (maybe t (\ip -> ip : t) ip')


main :: IO ()
main = do
  Log.setup
  
  let nameF = nameM ++ ".main"

  host <- lookupEnv "HOST"
  port <- lookupEnv "PORT"
  zhina_host <- lookupEnv "ZHINA_HOST"
  zhina_port <- lookupEnv "ZHINA_PORT"
  world_host <- lookupEnv "WORLD_HOST"
  world_port <- lookupEnv "WORLD_PORT"
  
  zhina_udp_timeout <- lookupEnv "ZHINA_UDP_TIMEOUT"
  zhina_tcp_timeout <- lookupEnv "ZHINA_TCP_TIMEOUT"
  world_tcp_timeout <- lookupEnv "WORLD_TCP_TIMEOUT"
  

  let host' = fromMaybe "127.0.0.1" host
  let port' = fromMaybe "5300" port
  let zhina_host' = fromMaybe "114.114.114.114" zhina_host
  let zhina_port' = fromMaybe "53" zhina_port
  let world_host' = fromMaybe "8.8.8.8" world_host
  let world_port' = fromMaybe "53" world_port


  let zhina_udp_timeout' = maybe 100000 read zhina_udp_timeout
  let zhina_tcp_timeout' = maybe 1000000 read zhina_tcp_timeout 
  let world_tcp_timeout' = maybe 5000000 read world_tcp_timeout 


  l' <- readChinaIP stdin
  l <- case l' of
    Left e -> error e
    Right l -> return l
  
  let ips = foldl (\a b -> add a b) create l
  infoM nameF $ (show $ size ips) ++  " china subnets loaded"


  bracket
    (do
        let c_china_udp = UDP.Config {UDP.host = zhina_host', UDP.port = zhina_port'}
        r_china_udp <- UDP.new $ c_china_udp
        infoM nameF $ "created UDP client to " ++ (show c_china_udp)

        let c_china_tcp =  TCP.Config {TCP.host = zhina_host', TCP.port = zhina_port'}
        r_china_tcp <- TCP.new $ c_china_tcp
        infoM nameF $ "created TCP client to " ++ (show c_china_tcp)

        let c_world_tcp =  TCP.Config {TCP.host = world_host', TCP.port = world_port'}
        r_world <- TCP.new $ c_world_tcp
        infoM nameF $ "created TCP client to " ++ (show c_world_tcp)

        return (r_china_udp, r_china_tcp, r_world)
    )
    (\(r_china_udp, r_china_tcp, r_world) -> do
        R.delete r_china_udp
        R.delete r_china_tcp
        R.delete r_world
    )
    (\(r_china_udp, r_china_tcp, r_world) -> do
        let r_china_udp' = timeout zhina_udp_timeout' $ R.resolve r_china_udp
        let r_china_tcp' = timeout zhina_tcp_timeout' $ R.resolve r_china_tcp
        let r_world' = timeout world_tcp_timeout' $ R.resolve r_world
        let r_udp = ZDNS.resolve $ ZDNS.Config
                { ZDNS.china = r_china_udp'
                , ZDNS.world = r_world'
                , ZDNS.chinaIP = ips
                }
            r_tcp = ZDNS.resolve $ ZDNS.Config { ZDNS.china = r_china_tcp'
                                               , ZDNS.world = r_world'
                                               , ZDNS.chinaIP = ips
                                               }
        bracket
          (do
              t_udp <- forkIO $ udp $ Config { resolve = SUDP.resolve $ r_udp
                                             , host = host'
                                             , port = port'
                                             }
              t_tcp <- forkIO $ tcp_listen $ Config { resolve = STCP.resolve $ r_tcp
                                                    , host = host'
                                                    , port = port'
                                                    }
              return (t_udp, t_tcp))
          (\(t_udp, t_tcp) -> do
              killThread t_udp
              killThread t_tcp)
          (\_ -> forever $ threadDelay 1000000)
    )
data Config a b = Config { resolve :: R.Resolve a b
                         , host :: String
                         , port :: String
                       }
    
    
udp :: Config ByteString ByteString -> IO ()
udp c = do
  let nameF = nameM ++ ".udp"
  let maxLength = 512 -- 512B is max length of UDP message
                      -- due ot rfc1035
  
  infoM nameF $ "starting UDP server"
  let hints = defaultHints { addrSocketType = Datagram, addrFlags = [AI_ADDRCONFIG, AI_PASSIVE]}
  addr:_ <- getAddrInfo (Just hints) (Just $ host c) (Just $ port c)
  bracket 
    (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
    close
    (\sock -> do
        bind sock (addrAddress addr)
        infoM nameF $ "bound to " ++ (show $ addrAddress addr)
        forever $ do
          (a, sa) <- recvFrom sock maxLength
          forkIO $ do 
            b <- resolve c a
            void $ sendTo sock b sa
    )
    
tcp_listen :: Config BSL.ByteString BSL.ByteString -> IO ()
tcp_listen c = do
  let nameF = nameM ++ ".tcp"
  infoM nameF "starting TCP server"
  let hints = defaultHints { addrSocketType = Stream, addrFlags = [AI_ADDRCONFIG, AI_PASSIVE]}
  addr:_ <- getAddrInfo (Just hints) (Just $ host c) (Just $ port c)
  bracket
    (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
    close
    (\sock -> do 
        bind sock (addrAddress addr)
        infoM nameF $ "bound to " ++ (show $ addrAddress addr)
        listen sock 5
        forever $ do
          bracketOnError
            (accept sock)
            (\(sock', sa) -> close sock')
            (\(sock', sa) -> do 
                let nameConn = nameF ++ "." ++ (show sa)
                forkFinally (tcp sock' nameConn (resolve c)) (\e -> close sock'))
    )


tcp sock' nameConn r = do
  qi <- newEmptyTMVarIO
  qo <- newEmptyTMVarIO
  si <- newTVarIO False
  so <- newTVarIO False

  bracket 
    (do
        -- thread receiving messages to qi
        ti <- forkFinally
          (do
              let nameRecv = nameConn ++ ".recv"
              let recvAll' n = do  
                    bs <- SL.recv sock' n
                    when (BSL.length bs == 0) $ throwIO ThreadKilled
                    mappend (lazyByteString bs) <$> (recvAll' $ n - (BSL.length bs))
                  recvAll n = toLazyByteString <$> recvAll' n

              forever $ runMaybeT $ do
                n <- lift $ recvAll 2
                n' <- case AP.parseOnly anyWord16be (BSL.toStrict n) of 
                  Left e -> error "How is it possible?"
                  Right n' -> return n'
                lift $ do 
                  bs <- recvAll $ fromIntegral n'
                  atomically $ putTMVar qi bs)
          (\_ -> atomically $ writeTVar si True)

        -- thread sending messages from qo
        to <- forkFinally
          (do
              let nameSend = nameConn ++ ".send"
              let sendAll bs = if BSL.null bs  then
                                 return ()
                               else do
                    n <- SL.send sock' bs
                    sendAll (BSL.drop n bs)
              forever $ do
                bs <- atomically $ takeTMVar qo
                sendAll $ toLazyByteString $ word16BE $ fromIntegral $ BSL.length bs
                sendAll bs)
          (\_ -> atomically $ writeTVar so True)
        return (ti, to)
    )
    (\(ti, to) -> do
        killThread ti
        killThread to
    )
    (\_ -> forever $ do
        a <- atomically $ do
          x <- readTVar si
          if x then tryTakeTMVar qi
            else Just <$> takeTMVar qi
        case a of
          Nothing -> throwIO ThreadKilled
          Just a' -> forkIO $ do 
            b <- r a'
            atomically $ do
              x <- readTVar so
              when (not x) $ putTMVar qo b
    )

