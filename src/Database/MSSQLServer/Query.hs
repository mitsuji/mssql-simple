{-# LANGUAGE DeriveDataTypeable #-}

module Database.MSSQLServer.Query ( -- * SQL Text Query
                                    sql
                                    
                                  -- ** ResultSet
                                  , ResultSet (..)
                                  , Row (..)
                                  
                                  -- * RPC Query
                                  , rpc
                                  
                                  -- ** RpcResultSet
                                  , RpcResultSet (..)
                                  , RpcResult (..)
                                  , RpcOutputSet (..)
                                  
                                  -- ** RpcQuerySet
                                  , RpcQuerySet (..)
                                  , RpcQuery (..)
                                  , RpcQueryId (..)
                                  , StoredProcedure (..)
                                  , RpcParamSet (..)
                                  , RpcParam (..)
                                  , RpcParamName
                                  , nvarcharVal
                                  , ntextVal
                                  , varcharVal
                                  , textVal
                                  , Only (..)
                                  
                                  -- * Exceptions
                                  , withTransaction
                                  , QueryError (..)
                                  ) where

import Data.Monoid ((<>))
import Data.Typeable(Typeable)

import Network.Socket (Socket)
import Network.Socket.ByteString (recv)
import Network.Socket.ByteString.Lazy (sendAll)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import qualified Data.Text as T

import Data.Binary (Binary(..),encode)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put

import Control.Monad (when)
import Control.Exception (Exception(..),throwIO,onException)

import Database.Tds.Message

import Database.MSSQLServer.Connection
import Database.MSSQLServer.Query.Only
import Database.MSSQLServer.Query.ResultSet
import Database.MSSQLServer.Query.RpcResultSet
import Database.MSSQLServer.Query.RpcQuerySet


data QueryError = QueryError !Info
                deriving (Show,Typeable)

instance Exception QueryError


sql :: ResultSet a => Connection -> T.Text -> IO a
sql (Connection sock ps) query = do
  sendAll sock $ Put.runPut $ putClientMessage ps $ CMSqlBatch $ SqlBatch query
  TokenStreams tss <- readMessage sock $ Get.runGetIncremental getServerMessage

  case filter isTSError tss of
    [] -> return $ fromTokenStreams tss
    TSError info :_ -> throwIO $ QueryError info



rpc :: (RpcQuerySet a, RpcResultSet b) => Connection -> a -> IO b
rpc (Connection sock ps) queries = do
  sendAll sock $ Put.runPut $ putClientMessage ps $ CMRpcRequest $ toRpcRequest queries
  TokenStreams tss <- readMessage sock $ Get.runGetIncremental getServerMessage

  case filter isTSError tss of
    [] -> return $ fromListOfTokenStreams $ splitBy isTSDoneProc tss
    TSError info :_ -> throwIO $ QueryError info
  
  where
    isTSDoneProc :: TokenStream -> Bool
    isTSDoneProc (TSDoneProc{}) = True
    isTSDoneProc _ = False


    spanBy :: (a -> Bool) -> [a] -> ([a],[a])
    spanBy q xs = case span (not . q) xs of
      t@(ys,z:zs) | (q z) -> (ys <> [z], zs)
                  | otherwise -> t
      t -> t

    splitBy :: (a -> Bool) -> [a] ->[[a]]
    splitBy q xs = case spanBy q xs of
      (ys,[]) -> [ys]
      (ys,zs) -> ys:splitBy q zs



nvarcharVal :: RpcParamName -> T.Text -> RpcParam T.Text
nvarcharVal name ts = RpcParamVal name (TINVarChar (fromIntegral $ (T.length ts) * 2) (Collation 0x00000000 0x00)) ts

ntextVal :: RpcParamName -> T.Text -> RpcParam T.Text
ntextVal name ts = RpcParamVal name (TINText (fromIntegral $ (T.length ts) * 2) (Collation 0x00000000 0x00)) ts

varcharVal :: RpcParamName -> B.ByteString -> RpcParam B.ByteString
varcharVal name bs = RpcParamVal name (TIBigVarChar (fromIntegral $ B.length bs) (Collation 0x00000000 0x00)) bs

textVal :: RpcParamName -> B.ByteString -> RpcParam B.ByteString
textVal name bs = RpcParamVal name (TIText (fromIntegral $ B.length bs) (Collation 0x00000000 0x00)) bs



withTransaction :: Connection -> IO a -> IO a
withTransaction conn act = do
  begin
  r <- act `onException` rollback
  commit
  return r
    where
      begin = sql conn $ T.pack "BEGIN TRANSACTION" :: IO ()
      commit = sql conn $ T.pack "COMMIT TRANSACTION" :: IO ()
      rollback = sql conn $ T.pack "ROLLBACK TRANSACTION":: IO ()



isTSError :: TokenStream -> Bool
isTSError (TSError{}) = True
isTSError _ = False

readMessage :: Socket -> Get.Decoder a -> IO a
readMessage sock decoder = do
  bs <- recv sock 512 -- [TODO] optimize
  case Get.pushChunk decoder bs of
    Get.Done _ _ msg -> return msg
    decoder' -> readMessage sock decoder'


