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
                                  , Only (..)
                                  
                                  -- * Exceptions
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

import Control.Monad (when)
import Control.Exception (Exception(..),throwIO)

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
sql (Connection sock) query = do
  sendAll sock $ encode $ CMSqlBatch $ SqlBatch query
  ServerMessage (TokenStreams tss) <- readMessage sock $ Get.runGetIncremental get

  case filter isTSError tss of
    [] -> return $ fromTokenStreams tss
    TSError info :_ -> throwIO $ QueryError info



rpc :: (RpcQuerySet a, RpcResultSet b) => Connection -> a -> IO b
rpc (Connection sock) queries = do
  sendAll sock $ encode $ CMRpcRequest $ toRpcRequest queries
  ServerMessage (TokenStreams tss) <- readMessage sock $ Get.runGetIncremental get

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



isTSError :: TokenStream -> Bool
isTSError (TSError{}) = True
isTSError _ = False

readMessage :: Socket -> Get.Decoder a -> IO a
readMessage sock decoder = do
  bs <- recv sock 512 -- [TODO] optimize
  case Get.pushChunk decoder bs of
    Get.Done _ _ msg -> return msg
    decoder' -> readMessage sock decoder'


