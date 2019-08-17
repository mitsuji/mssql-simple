{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Database.MSSQLServer.Query ( -- * SQL Text Query
                                    sql
                                    
                                  -- ** ResultSet
                                  , ResultSet (..)
                                  , Result (..)
                                  , Row (..)
                                  , Only (..)
                                  , RowCount (..)
                                  , ReturnStatus (..)
                                  
                                  -- * RPC Query
                                  , rpc
                                  
                                  -- ** RpcResponseSet
                                  , RpcResponseSet (..)
                                  , RpcResponse (..)
                                  , RpcOutputSet (..)
                                  
                                  -- ** RpcResultSet
                                  , RpcResultSet (..)
                                  , RpcResult (..)

                                  -- ** RpcQuerySet
                                  , RpcQuerySet (..)
                                  , RpcQuery (..)
                                  , RpcQueryId (..)
                                  , StoredProcedure (..)
                                  , RpcParamSet (..)
                                  , RpcParam (..)
                                  , RpcParamName

                                  , bitVal
                                  , tinyintVal
                                  , smallintVal
                                  , intVal
                                  , bigintVal
                                  , smallmoneyVal
                                  , moneyVal
                                  , smalldatetimeVal
                                  , datetimeVal
                                  , float24Val
                                  , realVal
                                  , float53Val
                                  , doubleVal
                                  , uniqueidentifierVal
                                  , decimalVal
                                  , numericVal
                                  , charVal
                                  , varcharVal
                                  , textVal
                                  , ncharVal
                                  , nvarcharVal
                                  , ntextVal
                                  , binaryVal
                                  , varbinaryVal
                                  , imageVal

                                  , bitRef
                                  , tinyintRef
                                  , smallintRef
                                  , intRef
                                  , bigintRef
                                  , smallmoneyRef
                                  , moneyRef
                                  , smalldatetimeRef
                                  , datetimeRef
                                  , float24Ref
                                  , realRef
                                  , float53Ref
                                  , doubleRef
                                  , uniqueidentifierRef
                                  , decimalRef
                                  , numericRef
                                  , charRef
                                  , varcharRef
                                  , textRef
                                  , ncharRef
                                  , nvarcharRef
                                  , ntextRef
                                  , binaryRef
                                  , varbinaryRef
                                  , imageRef

                                  , bitDefRef
                                  , tinyintDefRef
                                  , smallintDefRef
                                  , intDefRef
                                  , bigintDefRef
                                  , smallmoneyDefRef
                                  , moneyDefRef
                                  , smalldatetimeDefRef
                                  , datetimeDefRef
                                  , float24DefRef
                                  , realDefRef
                                  , float53DefRef
                                  , doubleDefRef
                                  , uniqueidentifierDefRef
                                  , decimalDefRef
                                  , numericDefRef
                                  , charDefRef
                                  , varcharDefRef
                                  , textDefRef
                                  , ncharDefRef
                                  , nvarcharDefRef
                                  , ntextDefRef
                                  , binaryDefRef
                                  , varbinaryDefRef
                                  , imageDefRef
                                  -- * Exceptions
                                  , withTransaction
                                  , QueryError (..)
                                  ) where

import Data.Typeable(Typeable)

import Network.Socket (Socket)
import Network.Socket.ByteString (recv)
import Network.Socket.ByteString.Lazy (sendAll)

import qualified Data.Text as T

import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put

import Control.Exception (Exception(..),throwIO,onException)

import Database.Tds.Message

import Database.MSSQLServer.Connection (Connection(..))
import Database.MSSQLServer.Query.Only
import Database.MSSQLServer.Query.Row
import Database.MSSQLServer.Query.ResultSet
import Database.MSSQLServer.Query.RpcResponseSet
import Database.MSSQLServer.Query.RpcQuerySet
import Database.MSSQLServer.Query.TokenStreamParser

#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except
#else
import Control.Monad.Error
runExceptT = runErrorT
#endif

data QueryError = QueryError !Info
                deriving (Show,Typeable)

instance Exception QueryError


sql :: ResultSet a => Connection -> T.Text -> IO a
sql (Connection sock ps) query = do
  sendAll sock $ Put.runPut $ putClientMessage ps $ CMSqlBatch $ SqlBatch query
  TokenStreams tss <- readMessage sock $ Get.runGetIncremental getServerMessage

  case parse responseParser tss of
    [] -> fail "sql: failed to parse token streams"
    (Left info,_):_ -> throwIO $ QueryError info
    (Right x,_):_ -> return x

  where
    responseParser :: (ResultSet a) => Parser (Either Info a)
    responseParser = runExceptT $ resultSetParser



rpc :: (RpcQuerySet a, RpcResponseSet b) => Connection -> a -> IO b
rpc (Connection sock ps) queries = do
  sendAll sock $ Put.runPut $ putClientMessage ps $ CMRpcRequest $ toRpcRequest queries
  TokenStreams tss <- readMessage sock $ Get.runGetIncremental getServerMessage

  case parse rpcResponseSetParser tss of
    [] -> fail "rpc: failed to parse token streams"
    (x,_):_ -> return x



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



readMessage :: Socket -> Get.Decoder a -> IO a
readMessage sock decoder = do
  bs <- recv sock 512 -- [TODO] optimize
  case Get.pushChunk decoder bs of
    Get.Done _ _ msg -> return msg
    decoder' -> readMessage sock decoder'


