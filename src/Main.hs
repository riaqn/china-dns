module Main where

import qualified ZhinaDNS as ZDNS
import qualified IPSet
import qualified NameSet
import Text.Parsec (parse, Line)
import Parse
import qualified Server as S
import qualified Log
import Options.Applicative


import System.IO
import System.Log.Logger

import Network.Socket hiding (recv, recvFrom, send, sendTo)
import Network.Socket.ByteString 

import qualified Resolve.Types as R
import qualified Resolve.DNS.Transport.Helper.UDP as UDP
import qualified Resolve.DNS.Transport.Helper.LiveTCP as TCP
import qualified Resolve.DNS.Transport.Dumb as D

import qualified Resolve.DNS.Lookup as L
import Resolve.DNS.Utils

import Resolve.Timeout

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import Data.ByteString.Builder

import Control.Monad
import Control.Monad.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

import Control.Exception


nameM = "Main"

readLines :: Handle -> (Line -> String -> Either e (Maybe a)) -> IO (Either e [a])
readLines h p = helper 0 []
  where helper i t = do
          done <- hIsEOF h
          if done then do
            return $ Right t
            else do
            l <- hGetLine h
            case p i l of
              Left e -> return $ Left $ e
              Right ip' -> helper (i +1) (maybe t (\ip -> ip : t) ip')

data Config = Config { host :: Maybe String
                     , port :: String
                     , zhina_host :: String
                     , zhina_port :: String
                     , world_host :: String
                     , world_port :: String
                     , zhina_timeout :: Int
                     , world_timeout :: Int
                     , log_level :: [(String, Priority)]
                     , zhina_ip :: String
                     , world_name :: String
                     }

config :: Parser Config
config = Config <$>
         option (Just <$> str) ( long "host" <>
                       metavar "HOST" <>
                       value Nothing <>
                       showDefault <>
                       help "hostname to bind" ) <*>
         strOption ( long "port" <>
                     metavar "PORT" <>
                     value "5300" <>
                     showDefault <>
                     help "port number to bind" ) <*>
         strOption ( long "zhina_host" <>
                     metavar "HOST" <>
                     value "114.114.114.114" <>
                     showDefault <>
                     help "upstream Chinese server host" ) <*>
         strOption ( long "zhina_port" <>
                     metavar "PORT" <>
                     value "53" <>
                     showDefault <>
                     help "upstream Chinese server port" ) <*>
         strOption ( long "world_host" <>
                     metavar "HOST" <>
                     value "8.8.8.8" <>
                     showDefault <>
                     help "upstream foreign server host" ) <*>
         strOption ( long "world_port" <>
                     metavar "PORT" <>
                     value "53" <>
                     showDefault <>
                     help "upstream foreign server port" ) <*>
         option auto ( long "zhina_timeout" <>
                       metavar "MICROSEC" <>
                       value 1000000 <>
                       showDefault <>
                       help "timeout for Chinese upstream, UDP and TCP combined ") <*>
         option auto ( long "world_timeout" <>
                       metavar "MICROSEC" <>
                       value 5000000 <>
                       showDefault <>
                       help "timeout for foreign upstream, UDP and TCP combined") <*>
         option (eitherReader $ \s -> case parse log_line "" s of
                    Left e -> Left $ show e
                    Right b -> Right b ) ( long "log_level" <>
                       metavar "SPEC" <>
                       value [(rootLoggerName, INFO)] <>
                       showDefault <>
                       help "spec for logging") <*>
         strOption ( long "zhina_ip" <>
                     metavar "PATH" <>
                     showDefault <>
                     help "file containing Chinese IP ranges") <*>
         strOption ( long "world_name" <>
                     metavar "PATH" <>
                     showDefault <>
                     help "file containing foreign domain names")

main = execParser opts >>= main'
  where opts  = info (helper <*> config)
                ( fullDesc <>
                  progDesc "a DNS proxy for people in Zhina" 
                )

main' :: Config -> IO ()
main' c = do
  Log.setup (log_level c)
  
  let nameF = nameM ++ ".main"

  let readLines' f p = withFile (f c) ReadMode (\h -> do
                                                   r <- readLines h (\i l -> parse (p i) (f c) l)
                                                   case r of 
                                                     Left e -> error $ show e
                                                     Right b -> return b
                                                   )

  zhina_ip' <- readLines' zhina_ip ip_line
  world_name' <- readLines' world_name name_line
  
  let ips = foldl (\a b -> IPSet.add a b) IPSet.create zhina_ip'
  infoM nameF $ (show $ length zhina_ip') ++  " Chinese IP ranges loaded"
  let names = foldl (\a b -> NameSet.add a b) NameSet.create world_name'
  infoM nameF $ (show $ length world_name') ++ " foreign domains loaded"
  
  let c_china_udp = UDP.Config {UDP.host = zhina_host c, UDP.port = zhina_port c, UDP.p_max = 4096}
  t_china_udp <- UDP.new $ c_china_udp
  infoM nameF $ "created client: " ++ (show c_china_udp)

  let c_china_tcp = TCP.Config {TCP.host = zhina_host c, TCP.port = zhina_port c, TCP.passive = True}
  t_china_tcp <- TCP.new $ c_china_tcp
  infoM nameF $ "created client: " ++ (show c_china_tcp)

  let c_world_tcp = TCP.Config {TCP.host = world_host c, TCP.port = world_port c, TCP.passive = True}
  t_world_tcp <- TCP.new $ c_world_tcp
  infoM nameF $ "created client: " ++ (show c_world_tcp)

  l_china <- L.new $ L.Config { L.udp = (t_china_udp, (return 1024))
                              , L.tcp = (t_china_tcp)
                              }
             
  l_world <- L.new $ L.Config { L.udp = (D.dumb, (return 0))
                              , L.tcp = t_world_tcp}

  let r = ZDNS.resolve $ ZDNS.Config
        { ZDNS.china = timeout (zhina_timeout c) $ R.resolve l_china
        , ZDNS.world = timeout (world_timeout c) $ R.resolve l_world
        , ZDNS.chinaIP = ips
        , ZDNS.worldName = names
        }
              
  void $ forkIO $ udp $ ServerConfig { resolve = S.server $ S.Config { S.back = r
                                                               , S.is_udp = True
                                                               }
                               , server_host = host c
                               , server_port = port c
                               }
           
  void $ forkIO $ tcp_listen $ ServerConfig { resolve = S.server $ S.Config { S.back = r
                                                                      , S.is_udp = False
                                                                      }
                                      , server_host = host c
                                      , server_port = port c
                                      }
  forever $ threadDelay 1000000

data ServerConfig = ServerConfig { resolve :: R.Resolve BSL.ByteString BSL.ByteString
                                 , server_host :: Maybe String
                                 , server_port :: String
                                 }
    
udp :: ServerConfig  -> IO ()
udp c = do
  let nameF = nameM ++ ".udp"
  let maxLength = 512 -- 512B is max length of UDP message
                      -- due ot rfc1035
  
  infoM nameF $ "starting UDP server"
  let hints = defaultHints { addrSocketType = Datagram, addrFlags = [AI_ADDRCONFIG, AI_PASSIVE]}
  addr:_ <- getAddrInfo (Just hints) (server_host c) (Just $ server_port c)
  bracket 
    (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
    close
    (\sock -> do
        bind sock (addrAddress addr)
        infoM nameF $ "bound to " ++ (show $ addrAddress addr)
        forever $ do
          (a, sa) <- recvFrom sock maxLength
          forkIO $ do 
            b <- resolve c (BSL.fromStrict a)
            void $ sendTo sock (BSL.toStrict b) sa
    )
    
tcp_listen :: ServerConfig -> IO ()
tcp_listen c = do
  let nameF = nameM ++ ".tcp"
  infoM nameF "starting TCP server"
  let hints = defaultHints { addrSocketType = Stream, addrFlags = [AI_ADDRCONFIG, AI_PASSIVE]}
  addr:_ <- getAddrInfo (Just hints) (server_host c) (Just $ server_port c)
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
            (\(sock', _) -> close sock')
            (\(sock', sa) -> do 
                let nameConn = nameF ++ "." ++ (show sa)
                forkFinally (tcp sock' nameConn (resolve c)) (\_ -> debugM nameF "closing the socket" >> close sock'))
    )


tcp sock' _ r = do
  let nameF = nameM ++ ".tcp"
  qi <- newEmptyTMVarIO
  qo <- newEmptyTMVarIO
  si <- newTVarIO False
  so <- newTVarIO False

  bracket 
    (do
        -- thread receiving messages to qi
        ti <- forkFinally
          (do
              let recvAll' l n = if n == 0 then return l
                    else do 
                    bs <- recv sock' n
                    debugM nameF $ "recv: " ++ (show $BS.length bs) ++ "B = " ++ (show bs)
                    when (BS.null bs) $ throwIO ThreadKilled
                    recvAll' (mappend l $ byteString bs) (n - (BS.length bs))
                  recvAll n = toLazyByteString <$> recvAll' mempty n

              forever $ runMaybeT $ do
                n <- lift $ recvAll 2
                let n' = toWord16 (BSL.toStrict n)
                lift $ do 
                  bs <- recvAll $ fromIntegral n'
                  atomically $ putTMVar qi bs)
          (\x -> do
              debugM nameF $ "recv exited: " ++ either (\e -> show (e :: SomeException)) (\_ -> " elegantly") x
              atomically $ writeTVar si True)

        -- thread sending messages from qo
        to <- forkFinally
          (do
              let sendAll bs = if BS.null bs  then
                                 return ()
                               else do
                    n <- send sock' bs
                    sendAll (BS.drop n bs)
              forever $ do
                bs <- atomically $ takeTMVar qo
                case safeFromIntegral $ BSL.length bs of
                  Nothing -> return ()
                  Just n -> do 
                    sendAll $ fromWord16 n
                    sendAll (BSL.toStrict bs))
          (\x -> do
              debugM nameF $ "send exited: " ++ either (\e -> show (e :: SomeException)) (\_ -> " elegantly") x
              atomically $ writeTVar so True)
        return (ti, to)
    )
    (\(ti, to) -> uninterruptibleMask_ $ do
        killThread ti
        killThread to
    )
    (\_ -> forever $ do
        a <- atomically $ do
          x <- readTVar si
          if x then tryTakeTMVar qi
            else Just <$> takeTMVar qi
        case a of
          Nothing -> do
            debugM nameF "my friend is dead, I want to kill myself"
            throwIO ThreadKilled
          Just a' -> forkIO $ do 
            b <- r a'
            atomically $ do
              x <- readTVar so
              when (not x) $ putTMVar qo b
    )

