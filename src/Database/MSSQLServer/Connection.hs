{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- SQL Server client library implemented in Haskell
--
-- [Usage Example](https://github.com/mitsuji/mssql-simple-example/blob/master/app/Main.hs)


module Database.MSSQLServer.Connection
  (
    -- * Connect with the SQL Server
    -- $use

      ConnectInfo(..)
    , defaultConnectInfo
    , Connection(..)
    , connect
    , connectWithoutEncryption
    , close
    , ProtocolError(..)
    , AuthError(..)
    ) where

import qualified Network.Socket as Socket
import Network.Socket (AddrInfo(..),SocketType(..),Socket(..))
import Network.Socket.ByteString (recv)
import Network.Socket.ByteString.Lazy (sendAll)

import Data.Monoid ((<>),mempty)

import qualified Data.ByteString as B

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get

import Control.Monad (when)
import Control.Exception (Exception(..),throwIO)

import qualified Network.TLS as TLS
import Network.HostName (getHostName)

import Database.Tds.Message
import Database.Tds.Transport (contextNew)

import Data.Word (Word8,Word32)
import Data.Int (Int32)
import Data.Typeable(Typeable)

data ProtocolError = ProtocolError String
                   deriving (Show,Typeable)
instance Exception ProtocolError

data AuthError = AuthError !Info
               deriving (Show,Typeable)
instance Exception AuthError




data ConnectInfo = ConnectInfo { connectHost :: !String
                               , connectPort :: !String
                               , connectDatabase :: !String
                               , connectUser :: !String
                               , connectPassword :: !String
                               , connectEncryption :: !Word8
                               , connectPacketSize :: !Word32
                               , connectOptionFlags1 :: !Word8
                               , connectOptionFlags2 :: !Word8
                               , connectOptionFlags3 :: !Word8
                               , connectTypeFlags :: !Word8
                               , connectTimeZone :: !Int32
                               , connectCollation :: !Collation32
                               , connectLanguage :: !String
                               , connectAppName :: !String
                               , connectServerName :: !String
                               }

defaultConnectInfo :: ConnectInfo
defaultConnectInfo =
  let
    l7 = defaultLogin7
  in ConnectInfo { connectHost = mempty
                 , connectPort = mempty
                 , connectDatabase = T.unpack $ l7Database l7
                 , connectUser = T.unpack $ l7UserName l7
                 , connectPassword = T.unpack $ l7Password l7
                 , connectEncryption = 0x00 -- 0x00: ENCRYPT_OFF (Encrypt login packet only), 0x02: ENCRYPT_NOT_SUP (No encryption)
                 , connectPacketSize = l7PacketSize l7
                 , connectOptionFlags1 = l7OptionFlags1 l7
                 , connectOptionFlags2 = l7OptionFlags2 l7
                 , connectOptionFlags3 = l7OptionFlags3 l7
                 , connectTypeFlags = l7TypeFlags l7
                 , connectTimeZone = l7TimeZone l7
                 , connectCollation = l7Collation l7
                 , connectLanguage = T.unpack $ l7Language l7
                 , connectAppName = T.unpack $ l7AppName l7
                 , connectServerName = T.unpack $ l7ServerName l7
                 }

                   
data Connection = Connection Socket Word32


connect :: ConnectInfo -> IO Connection
connect ci@(ConnectInfo host port _ _ _ encrypt ps _ _ _ _ _ _ _ _ _) = do
  addr <- resolve host port
  sock <- connect' addr
  
  Prelogin plResOpts <- performPrelogin sock ps encrypt

  PLOEncryption modeEnc:_  <- case filter isPLOEncryption plResOpts of
                                [] -> throwIO $ ProtocolError "connect: PLOEncryption is necessary"
                                xs -> return xs
  PLOMars modeMars:_ <- case filter isPLOMars plResOpts of
                          [] -> throwIO $ ProtocolError "connect: PLOMars is necessary"
                          xs -> return xs
  when (modeEnc/=encrypt)  $ throwIO $ ProtocolError "connect: Server reported unsupported encryption mode"
  when (modeMars/=0) $ throwIO $ ProtocolError "connect: Server reported unsupported mars mode"

  login7 <- newLogin7 ci

  tss <- case encrypt of
    0x00 -> do
      ---
      --- TLS handshake
      ---
      tlsContext <- contextNew sock host
      TLS.handshake tlsContext

      --- 
      --- Login with encrypted packet
      --- 
      TLS.sendData tlsContext $ Put.runPut $ putClientMessage ps $ CMLogin7 login7
      readMessage sock $ Get.runGetIncremental getServerMessage
    0x02 -> do
      --- 
      --- Login without encryipted packet
      --- 
      sendAll sock $ Put.runPut $ putClientMessage ps $ CMLogin7 login7
      readMessage sock $ Get.runGetIncremental getServerMessage
      
  --- 
  --- Verify Ack
  --- 
  validLoginAck login7 tss
  
  return $ Connection sock ps



connectWithoutEncryption :: ConnectInfo -> IO Connection
connectWithoutEncryption ci = connect $ ci {connectEncryption = 0x02}


close :: Connection -> IO ()
close (Connection sock _ ) = Socket.close sock




performPrelogin :: Socket -> Word32 -> Word8 -> IO Prelogin
performPrelogin sock ps enc = do
  -- https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/60f56408-0188-4cd5-8b90-25c6f2423868
  --
  -- Prelogin
  --
  -- [TODO] Threadid support
  -- [TODO] Mars support
  let clientPrelogin = Prelogin [ PLOVersion 8 0 341 0
                                , PLOEncryption enc
                                , PLOInstopt "MSSQLServer"
                                , PLOThreadid (Just 1000) -- [TODO]
                                , PLOMars 0 -- [TODO]
                                ]
  sendAll sock $ Put.runPut $ putClientMessage ps $ CMPrelogin clientPrelogin
  serverPrelogin <- readMessage sock $ Get.runGetIncremental getServerMessage
  
  return serverPrelogin


  
newLogin7 :: ConnectInfo -> IO Login7
newLogin7 (ConnectInfo _ _ database user pass _ _ optf1 optf2 optf3 typef tz coll lang app serv) = do
  ---
  --- Login7
  ---
  -- [TODO] process ID support
  -- [TODO] MAC address support
  hostname <- getHostName
  let login7 = defaultLogin7 { l7ClientProgVer = 1
                             , l7OptionFlags1 = optf1
                             , l7OptionFlags2 = optf2
                             , l7OptionFlags3 = optf3
                             , l7TypeFlags = typef
                             , l7TimeZone = tz
                             , l7Collation = coll
                             , l7CltIntName = T.pack "mssql-simple"
                             , l7Language = T.pack lang
                             , l7ClientPID = 1 -- [TODO]
                             , l7ClientMacAddr = B.pack [0x00,0x00,0x00,0x00,0x00,0x00] -- [TODO]
                             , l7ClientHostName = T.pack hostname
                             , l7AppName = T.pack app
                             , l7ServerName = T.pack serv
                             , l7UserName = T.pack user
                             , l7Password = T.pack pass
                             , l7Database = T.pack database
                             }
  return login7



validLoginAck :: Login7 -> TokenStreams -> IO ()
validLoginAck login7 (TokenStreams loginResTokenStreams) = do

  let loginAcks   = filter isTSLoginAck loginResTokenStreams
  when (null loginAcks) $ do
    TSError info:_ <- case filter isTSError loginResTokenStreams of
                        [] -> throwIO $ ProtocolError "validLoginAck: TSError is necessary"
                        xs -> return xs
    throwIO $ AuthError info

  let (TSLoginAck _ tdsVersion' _ _):_ = loginAcks
  when (tdsVersion /= tdsVersion') $ throwIO $ ProtocolError "validLoginAck: Server reported unsupported tds version"

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



isPLOEncryption :: PreloginOption -> Bool
isPLOEncryption (PLOEncryption{}) = True
isPLOEncryption _ = False

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




-- $use
-- 'connect' and 'close' function could be used as follows.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main where
-- >
-- > import Network.Socket (withSocketsDo)
-- > import Control.Exception (bracket)
-- >
-- > import Database.MSSQLServer.Connection
-- > import Database.MSSQLServer.Query
-- >
-- > main :: IO ()
-- > main = do
-- >   let info = defaultConnectInfo { connectHost = "192.168.0.1"
-- >                                 , connectPort = "1433"
-- >                                 , connectDatabase = "some_database"
-- >                                 , connectUser = "some_user"
-- >                                 , connectPassword = "some_password"
-- >                                 }
-- >   withSocketsDo $
-- >     bracket (connect info) close $ \conn -> do
-- >     rs <- sql conn "SELECT 2 + 2" :: IO [Only Int]
-- >     print rs
