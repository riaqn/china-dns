module Main where

import qualified ChinaDNS as CDNS
import IPSet
import qualified Log

import Data.IP

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

import Data.Attoparsec.ByteString
import Data.Attoparsec.Binary
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString (ByteString)
import Data.ByteString.Builder

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


import Control.Exception


nameM = "Main"

readChinaIP :: Handle -> IO [AddrRange IPv4]
readChinaIP h = helper []
  where helper t = do
          done <- hIsEOF h
          if done then do
            return t
            else do
            l <- hGetLine h
            helper (read l : t)


main :: IO ()
main = do
  let nameF = nameM ++ ".main"
  
  Log.setup

  l <- readChinaIP stdin
  
  let ips = foldl (\a b -> add a b) create l
  infoM nameF $ (show $ size ips) ++  " china subnets loaded"


  bracket
    (do 
        r_china_udp <- UDP.new $ UDP.Config {UDP.host = "114.114.114.114", UDP.port = "53"}
        r_china_tcp <- TCP.new $ TCP.Config {TCP.host = "223.5.5.5", TCP.port = "53"}
        r_world <- TCP.new $ TCP.Config {TCP.host = "8.8.8.8", TCP.port = "53"}
        
        return (r_china_udp, r_china_tcp, r_world)
    )
    (\(r_china_udp, r_china_tcp, r_world) -> do
        R.delete r_china_udp
        R.delete r_china_tcp
        R.delete r_world
    )
    (\(r_china_udp, r_china_tcp, r_world) -> do
        let r_china_udp' = timeout 500000 $ R.resolve r_china_udp
        let r_china_tcp' = timeout 1000000 $ R.resolve r_china_tcp
        let r_world' = timeout 5000000 $ R.resolve r_world
        let r_udp = CDNS.resolve $ CDNS.Config
                { CDNS.china = r_china_udp'
                , CDNS.world = r_world'
                , CDNS.chinaIP = ips
                }
            r_tcp = CDNS.resolve $ CDNS.Config { CDNS.china = r_china_tcp'
                                               , CDNS.world = r_world'
                                               , CDNS.chinaIP = ips
                                               }
        bracket
          (do
              t_udp <- forkIO $ udp $ SUDP.resolve $ r_udp
              t_tcp <- forkIO $ tcp_listen $ STCP.resolve $ r_tcp
              return (t_udp, t_tcp))
          (\(t_udp, t_tcp) -> do
              killThread t_udp
              killThread t_tcp)
          (\_ -> forever $ threadDelay 1000000)
    )
    
udp :: R.Resolve ByteString ByteString -> IO ()
udp r = do
  let nameF = nameM ++ ".udp"
  let maxLength = 512 -- 512B is max length of UDP message
                      -- due ot rfc1035
  
  infoM nameF $ "starting UDP server"
  let hints = defaultHints { addrSocketType = Datagram, addrFlags = [AI_ADDRCONFIG, AI_PASSIVE]}
  addr:_ <- getAddrInfo (Just hints) Nothing (Just "5300")
  bracket 
    (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
    close
    (\sock -> do
        bind sock (addrAddress addr)
        infoM nameF $ "bound to " ++ (show $ addrAddress addr)
        forever $ do
          (a, sa) <- recvFrom sock maxLength
          forkIO $ do 
            b <- r a
            void $ sendTo sock b sa
    )
    
tcp_listen :: R.Resolve BSL.ByteString BSL.ByteString -> IO ()
tcp_listen r = do
  let nameF = nameM ++ ".tcp"
  infoM nameF "starting TCP server"
  let hints = defaultHints { addrSocketType = Stream, addrFlags = [AI_ADDRCONFIG, AI_PASSIVE]}
  addr:_ <- getAddrInfo (Just hints) Nothing (Just "5300")
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
                forkFinally (tcp sock' nameConn r) (\e -> close sock'))
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
                n' <- case parseOnly anyWord16be (BSL.toStrict n) of 
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

