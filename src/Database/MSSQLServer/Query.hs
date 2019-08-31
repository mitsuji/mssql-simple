{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

-- |
-- SQL Server client library implemented in Haskell
--
-- [Usage Example](https://github.com/mitsuji/mssql-simple-example/blob/master/app/Main.hs)


module Database.MSSQLServer.Query (
                                  -- * SQL Text Query
                                  -- $use
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




-- $use
-- A 'sql' function accepts valid 'Connection' and SQL text.
-- And the expression could be evaluated as a instance of 'ResultSet' type class.
--
-- A 'Row' represents a record included in a query result.
-- Also list of 'Row' is a instance of 'ResultSet'.
-- So the result of select query could be obtained as follows.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Database.MSSQLServer.Connection
-- > import Database.MSSQLServer.Query
-- >
-- > query_select1 :: Connection -> IO Int
-- > query_select1 conn = do
-- >     [Only i] <- sql conn "SELECT 2 + 2" :: IO [Only Int]
-- >     return i
--
-- In SQL , uncomputable expression could be evaluated as NULL.
-- In that case, 'Maybe' type could be used.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Database.MSSQLServer.Connection
-- > import Database.MSSQLServer.Query
-- >
-- > query_select2 :: Connection -> IO (Maybe Int)
-- > query_select2 conn = do
-- >     [Only mi] <- sql conn "SELECT 6 / 2" :: IO [Only (Maybe Int)]
-- >     return mi
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Database.MSSQLServer.Connection
-- > import Database.MSSQLServer.Query
-- >
-- > query_select3 :: Connection -> IO (Maybe Int)
-- > query_select3 conn = do
-- >     [Only mi] <- sql conn "SELECT 6 / 0" :: IO [Only (Maybe Int)]
-- >     return mi
--
-- The result of select query from SQL table could be obtained as follows.
-- A member of tuple must be a instance of 'Data' type class
-- and convertible with the SQL data type of a SQL table column.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Database.MSSQLServer.Connection
-- > import Database.MSSQLServer.Query
-- >
-- > import qualified Data.Text.Lazy as LT
-- > import Database.Tds.Message
-- > import Data.Time (UTCTime(..))
-- >
-- > query_select4 :: Connection -> IO [(Int,String,LT.Text,Money,UTCTime,Maybe UTCTime,Maybe UTCTime)]
-- > query_select4 conn = sql conn "SELECT * FROM TSome ORDER BY somePrice"
--
-- Any type could be a instance of 'Row' and used as the result of select query as follows.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE BangPatterns #-}
-- >
-- > import Database.MSSQLServer.Connection
-- > import Database.MSSQLServer.Query
-- >
-- > import qualified Data.Text.Lazy as LT
-- > import Database.Tds.Message
-- > import Data.Time (UTCTime(..))
-- >
-- > data Some = Some { someID :: Int
-- >                  , someTitle :: LT.Text
-- >                  , someContent :: LT.Text
-- >                  , somePrice :: Money
-- >                  , someCreated:: UTCTime
-- >                  , someModified:: Maybe UTCTime
-- >                  , someDeleted:: Maybe UTCTime
-- >                  }
-- >           deriving (Show)
-- >
-- > instance Row Some where
-- >   fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7] [b1,b2,b3,b4,b5,b6,b7] = Some d1 d2 d3 d4 d5 d6 d7
-- >     where
-- >       !d1 = fromRawBytes (mcdTypeInfo m1) b1
-- >       !d2 = fromRawBytes (mcdTypeInfo m2) b2
-- >       !d3 = fromRawBytes (mcdTypeInfo m3) b3
-- >       !d4 = fromRawBytes (mcdTypeInfo m4) b4
-- >       !d5 = fromRawBytes (mcdTypeInfo m5) b5
-- >       !d6 = fromRawBytes (mcdTypeInfo m6) b6
-- >       !d7 = fromRawBytes (mcdTypeInfo m7) b7
-- >
-- >       mcdTypeInfo :: MetaColumnData -> TypeInfo
-- >       mcdTypeInfo (MetaColumnData _ _ ti _ _) = ti
-- >
-- >   fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 7"
-- >
-- > query_select5 :: Connection -> IO [Some]
-- > query_select5 conn = sql conn "SELECT TOP 10 * FROM TSome ORDER BY somePrice DESC"
--
-- 'Row' is also a instance of 'Result' and tuple of 'Result' is a instance of 'ResultSet'.
-- So the result of multiple SQL query could be obtained simultaneously as follows.
--
-- > import Data.Monoid (mconcat)
-- >
-- > query_select6 :: Connection -> IO ([Some],[Some])
-- > query_select6 conn =
-- >   sql conn $ mconcat ["SELECT * FROM TSome WHERE someID < 8 ORDER BY someID;",
-- >                       "SELECT * FROM TSome WHERE 8 <= someID AND someID < 12 ORDER BY someID DESC;"]
--
-- 'RowCount' is a instance of 'ResultSet'.
-- So the count of SQL table record affected with the SQL query could be obtainde as follows.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Database.MSSQLServer.Connection
-- > import Database.MSSQLServer.Query
-- >
-- > query_count1 :: Connection -> IO Int
-- > query_count1 conn = do
-- >   RowCount rc <- sql conn "UPDATE TSome SET somePrice = somePrice + 100 WHERE someID < 5"
-- >   return rc
--
-- Also () is a instance of 'ResultSet'.
-- So the result of SQL query could be discarded as follows.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Database.MSSQLServer.Connection
-- > import Database.MSSQLServer.Query
-- >
-- > query_discard1 :: Connection -> IO ()
-- > query_discard1 conn = sql conn "UPDATE TSome SET somePrice = somePrice + 100 WHERE someID < 5"
--
-- 'ReturnStatus' is a instance of 'ResultSet'.
-- So when a stored procedure executed in SQL text, Return status of the execution could be obtained as follows.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Database.MSSQLServer.Connection
-- > import Database.MSSQLServer.Query
-- >
-- > query_status1 :: Connection -> IO Int
-- > query_status1 conn = do
-- >   ReturnStatus rets <- sql conn "EXEC SP_Input1 @Val1=3"
-- >   return rets
