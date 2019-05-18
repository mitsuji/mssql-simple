{-# LANGUAGE OverloadedStrings #-}

module Database.MSSQLServer.Connection ( ConnectInfo(..)
                                       , Connection(..)
                                       , connect
                                       , connectWithoutEncription
                                       , close
                                       , ProtocolError(..)
                                       , AuthError(..)
                                       ) where

import qualified Network.Socket as Socket
import Network.Socket (AddrInfo(..),SocketType(..),Socket(..))
import Network.Socket.ByteString (recv)
import Network.Socket.ByteString.Lazy (sendAll)

import Data.Monoid ((<>))

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import Data.Binary (Binary(..),encode)
import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get

import Control.Monad (when)
import Control.Exception (Exception(..),throwIO)

import qualified Network.TLS as TLS
import Network.HostName (getHostName)

import Database.Tds.Message
import Database.Tds.Transport (contextNew)

import Data.Word (Word8)


data ProtocolError = ProtocolError String
                   deriving (Show)
instance Exception ProtocolError

data AuthError = AuthError !Info
               deriving (Show)
instance Exception AuthError




data ConnectInfo = ConnectInfo { connectHost :: String
                               , connectPort :: String
                               , connectDatabase :: String
                               , connectUser :: String
                               , connectPassword :: String
                               }
                   
newtype Connection = Connection Socket


connect :: ConnectInfo -> IO Connection
connect ci@(ConnectInfo host port database user pass) = do
  addr <- resolve host port
  sock <- connect' addr
  
  Prelogin plResOpts <- performPrelogin sock 0x00 -- ENCRYPT_OFF (Encrypt login packet only)

  [PLOEncription modeEnc]  <- case filter isPLOEncription plResOpts of
                                [] -> throwIO $ ProtocolError "connect: PLOEncription is necessary"
                                xs -> return xs
  [PLOMars modeMars] <- case filter isPLOMars plResOpts of
                          [] -> throwIO $ ProtocolError "connect: PLOMars is necessary"
                          xs -> return xs
  when (modeEnc/=0x00)  $ throwIO $ ProtocolError "connect: Server reported unsupported encription mode"
  when (modeMars/=0) $ throwIO $ ProtocolError "connect: Server reported unsupported mars mode"

  login7 <- newLogin7 ci

  ---
  --- TLS handshake
  ---
  tlsContext <- contextNew sock host
  TLS.handshake tlsContext

  --- 
  --- Login with encripted packet
  --- 
  TLS.sendData tlsContext $ encode $ CMLogin7 login7
  ServerMessage tss <- readMessage sock $ Get.runGetIncremental get

  --- 
  --- Verify Ack
  --- 
  validLoginAck login7 tss
  
  return $ Connection sock



connectWithoutEncription :: ConnectInfo -> IO Connection
connectWithoutEncription ci@(ConnectInfo host port database user pass) = do
  addr <- resolve host port
  sock <- connect' addr
  
  Prelogin plResOpts <- performPrelogin sock 0x02 -- ENCRYPT_NOT_SUP (No encription)

  [PLOEncription modeEnc]  <- case filter isPLOEncription plResOpts of
                                [] -> throwIO $ ProtocolError "connectWithoutEncription: PLOEncription is necessary"
                                xs -> return xs
  [PLOMars modeMars] <- case filter isPLOMars plResOpts of
                          [] -> throwIO $ ProtocolError "connectWithoutEncription: PLOMars is necessary"
                          xs -> return xs
  when (modeEnc/=0x02)  $ throwIO $ ProtocolError "connectWithoutEncription: Server reported unsupported encription mode"
  when (modeMars/=0) $ throwIO $ ProtocolError "connectWithoutEncription: Server reported unsupported mars mode"

  login7 <- newLogin7 ci
  
  --- 
  --- Login without encripted packet
  --- 
  sendAll sock $ encode $ CMLogin7 login7
  ServerMessage tss <- readMessage sock $ Get.runGetIncremental get

  --- 
  --- Verify Ack
  --- 
  validLoginAck login7 tss
  
  return $ Connection sock



close :: Connection -> IO ()
close (Connection sock) = Socket.close sock




performPrelogin :: Socket -> Word8 -> IO Prelogin
performPrelogin sock enc = do
  -- https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/60f56408-0188-4cd5-8b90-25c6f2423868
  --
  -- Prelogin
  --
  -- [TODO] Threadid support
  -- [TODO] Mars support
  let clientPrelogin = Prelogin [ PLOVersion 9 0 0 0
                                , PLOEncription enc
                                , PLOInstopt "MSSQLServer"
                                , PLOThreadid (Just 1000) -- [TODO]
                                , PLOMars 0 -- [TODO]
                                ]
  sendAll sock $ encode $ CMPrelogin clientPrelogin
  ServerMessage serverPrelogin <- readMessage sock $ Get.runGetIncremental get
  
  return serverPrelogin


  
newLogin7 :: ConnectInfo -> IO Login7
newLogin7 (ConnectInfo host port database user pass) = do
  ---
  --- Login7
  ---
  -- [TODO] Improve default params
  -- [TODO] process ID support
  -- [TODO] MAC address support
  hostname <- getHostName
  let login7 = defaultLogin7 { l7ClientPID = 1 -- [TODO]
                             , l7ClientMacAddr = B.pack [0x00,0x00,0x00,0x00,0x00,0x00] -- [TODO]
                             , l7ClientHostName = (T.pack hostname)
                             , l7AppName = "mssql-simple" -- [TODO] more nice name
                             , l7ServerName = (T.pack host)
                             , l7UserName = (T.pack user)
                             , l7Password = (T.pack pass)
                             , l7Database = (T.pack database)
                             }
  return login7



validLoginAck :: Login7 -> TokenStreams -> IO ()
validLoginAck login7 (TokenStreams loginResTokenStreams) = do
  
  let loginAcks   = filter isTSLoginAck loginResTokenStreams
  when (null loginAcks) $ do
    [TSError info] <- case filter isTSError loginResTokenStreams of
                        [] -> throwIO $ ProtocolError "validLoginAck: TSError is necessary"
                        xs -> return xs
    throwIO $ AuthError info

  let [TSLoginAck _ tdsVersion _ _] = loginAcks
  when (l7TdsVersion login7 /= tdsVersion) $ throwIO $ ProtocolError "validLoginAck: Server reported unsupported tds version"

  return ()
  where

    isTSLoginAck :: TokenStream -> Bool
    isTSLoginAck (TSLoginAck{}) = True
    isTSLoginAck _ = False

    isTSError :: TokenStream -> Bool
    isTSError (TSError{}) = True
    isTSError _ = False

    printEnvChange :: TokenStream -> IO ()
    printEnvChange (TSEnvChange t o n) = do
      putStr "TSEnvChange: "
      case t of
        1 -> T.putStr $ "Database: "   <> T.decodeUtf16LE o <> " -> " <> T.decodeUtf16LE n
        2 -> T.putStr $ "Language: "   <> T.decodeUtf16LE o <> " -> " <> T.decodeUtf16LE n
        3 -> T.putStr $ "Charset: "    <> T.decodeUtf16LE o <> " -> " <> T.decodeUtf16LE n
        4 -> T.putStr $ "PacketSize: " <> T.decodeUtf16LE o <> " -> " <> T.decodeUtf16LE n
        5 -> T.putStr $ "DSLID:      " <> T.decodeUtf16LE n
        6 -> T.putStr $ "DSCFlags: "   <> T.decodeUtf16LE n
        7 -> putStr $ "Collaction: " <> show o <> " -> " <> show n
        8 -> putStr $ "BeginTran: " <> show n
        9 -> putStr $ "CommitTran: " <> show o
        10 -> putStr $ "RollbackTran: " <> show o
        11 -> putStr $ "EnlistDTCTran: " <> show o
        12 -> putStr $ "DefactTran: " <> show n
        13 -> T.putStr $ "MirrorPartner: " <> T.decodeUtf16LE n
        15 -> putStr $ "PromoteTran: " <> show n
        16 -> putStr $ "TranManAddr: " <> show n
        17 -> putStr $ "TranEndedr: " <> show o
        18 -> putStr $ "ResetAck: "
        19 -> T.putStr $ "SendsBackInfo: " <> T.decodeUtf16LE n
        20 -> putStr $ "Routing: " <> show n
      putStrLn mempty



isPLOEncription :: PreloginOption -> Bool
isPLOEncription (PLOEncription{}) = True
isPLOEncription _ = False

isPLOMars :: PreloginOption -> Bool
isPLOMars (PLOMars{}) = True
isPLOMars _ = False

resolve host port = do
  let hints = Socket.defaultHints { addrSocketType = Stream }
  addr:_ <- Socket.getAddrInfo (Just hints) (Just host) (Just port)
  return addr

connect' addr = do
  sock <- Socket.socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  Socket.connect sock $ addrAddress addr
  return sock

readMessage :: Socket -> Get.Decoder a -> IO a
readMessage sock decoder = do
  bs <- recv sock 512 -- [TODO] optimize
  case Get.pushChunk decoder bs of
    Get.Done _ _ msg -> return msg
    decoder' -> readMessage sock decoder'




