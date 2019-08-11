{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Database.MSSQLServer.Query.RpcResponseSet ( RpcResponseSet (..)
                                                 , RpcResponse (..)
                                                 , RpcResultSet (..)
                                                 , RpcResult (..)
                                                 , RpcOutputSet (..)
                                                 ) where

import Control.Applicative((<$>))
import Database.Tds.Message
import Database.MSSQLServer.Query.Row
import Database.MSSQLServer.Query.Only
import Database.MSSQLServer.Query.TokenStreamParser
import Database.MSSQLServer.Query.Template

import Control.Monad(forM)
import Language.Haskell.TH (runIO,pprint)

#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except
#else
import Control.Monad.Error
runExceptT = runErrorT
#endif



listOfRow :: Row a => Parser' ([a])
listOfRow = do
  _ <- trySatisfyMany $ not . isTSColMetaData
  tsCmd <- trySatisfy isTSColMetaData
  _ <- trySatisfyMany $ not . isTSRow
  tsRows <- trySatisfyMany isTSRow
  _ <- trySatisfyMany  $ not . isTSDoneInProc -- necesarry ?
  _ <- trySatisfy isTSDoneInProc
  return $
    let
      (TSColMetaData (maybeCmd)) = tsCmd
      mcds = case (\(ColMetaData x) -> x) <$> maybeCmd of
               Nothing -> error "listOfRow: ColMetaData is necessary"
               Just mcds' -> mcds'
      rows = (\(TSRow row) -> getRawBytes <$> row) <$> tsRows
    in fromListOfRawBytes mcds <$> rows
  where

    isTSColMetaData :: TokenStream -> Bool
    isTSColMetaData (TSColMetaData{}) = True
    isTSColMetaData _ = False

    isTSRow :: TokenStream -> Bool
    isTSRow (TSRow{}) = True
    isTSRow _ = False

    isTSDoneInProc :: TokenStream -> Bool
    isTSDoneInProc (TSDoneInProc{}) = True
    isTSDoneInProc _ = False

    getRawBytes :: RowColumnData -> RawBytes
    getRawBytes (RCDOrdinal dt) = dt
    getRawBytes (RCDLarge _ _ dt) = dt




class RpcResultSet a where
  rpcResultSetParser :: Parser' a


instance RpcResultSet () where
  rpcResultSetParser = return ()


instance (Row a) => RpcResultSet [a] where
  rpcResultSetParser = listOfRow


-- [MEMO] using Template Haskell
forM [2..30] $ \n -> do
  dec <- rpcResultSetTupleQ n
--  runIO $ putStrLn $ pprint dec
  return dec
--instance (RpcResult a1, RpcResult a2) => RpcResultSet (a1, a2) where
--  rpcResultSetParser = do
--    !r1 <- rpcResultParser :: (RpcResult a1) => Parser' a1
--    !r2 <- rpcResultParser :: (RpcResult a2) => Parser' a2
--    return  (r1,r2)
--


class RpcResult a where
  rpcResultParser :: Parser' a

instance Row a => RpcResult [a] where
  rpcResultParser = listOfRow

  



rvTypeInfo :: ReturnValue -> TypeInfo
rvTypeInfo (ReturnValue _ _ _ _ _ ti _) = ti

rvRawBytes :: ReturnValue -> RawBytes
rvRawBytes (ReturnValue _ _ _ _ _ _ rb) = rb


class RpcOutputSet a where
  fromReturnValues :: [ReturnValue] -> a

instance RpcOutputSet () where
  fromReturnValues [] = ()
  fromReturnValues _ = error "fromReturnValues: List length must be 0"
  
instance (Data a) => RpcOutputSet (Only a) where
  fromReturnValues [r1] = Only d1
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
  fromReturnValues _ = error "fromReturnValues: List length must be 1"

-- [MEMO] using Template Haskell
forM [2..30] $ \n -> do
  dec <- rpcOutputSetTupleQ n
--  runIO $ putStrLn $ pprint dec
  return dec
--instance (Data a1, Data a2) => RpcOutputSet (a1,a2) where
--  fromReturnValues [r1,r2] = (d1,d2)
--    where
--      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
--      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
--  fromReturnValues _ = error "fromReturnValues: List length must be 2"
--



-- (RpcOutputSet a, RpcResultSet b) => 
data RpcResponse a b = RpcResponse !Int !a !b
                     | RpcResponseError !Info
                   deriving (Show)


rpcResponseParser :: (RpcOutputSet a, RpcResultSet b) => Parser (RpcResponse a b)
rpcResponseParser = do
  let rrParser = runExceptT $ do
        rrs <- rpcResultSetParser
        _ <- trySatisfyMany $ not . isTSReturnStatus
        TSReturnStatus ret <- trySatisfy isTSReturnStatus
        _ <- trySatisfyMany $ not . isTSReturnValue
        rvs <- trySatisfyMany isTSReturnValue
        _ <- trySatisfyMany $ not . isTSDoneProc
        _ <- trySatisfy isTSDoneProc

        let rvs' = (\(TSReturnValue rv) -> rv) <$>  rvs
        return $ RpcResponse (fromIntegral ret) (fromReturnValues rvs') rrs
  err <- rrParser
  case err of
    Left ei -> return $ RpcResponseError ei
    Right rr -> return rr

  where
    isTSReturnStatus :: TokenStream -> Bool
    isTSReturnStatus (TSReturnStatus{}) = True
    isTSReturnStatus _ = False

    isTSReturnValue :: TokenStream -> Bool
    isTSReturnValue (TSReturnValue{}) = True
    isTSReturnValue _ = False

    isTSDoneProc :: TokenStream -> Bool
    isTSDoneProc (TSDoneProc{}) = True
    isTSDoneProc _ = False





class RpcResponseSet a where
  rpcResponseSetParser :: Parser a

instance (RpcOutputSet a1, RpcResultSet b1) => RpcResponseSet (RpcResponse a1 b1) where
  rpcResponseSetParser = rpcResponseParser

-- [MEMO] using Template Haskell
forM [2..30] $ \n -> do
  dec <- rpcResponseSetTupleQ n
--  runIO $ putStrLn $ pprint dec
  return dec
--instance (RpcOutputSet a1, RpcResultSet b1, RpcOutputSet a2, RpcResultSet b2) => RpcResponseSet (RpcResponse a1 b1, RpcResponse a2 b2) where
--  rpcResponseSetParser = do
--    !r1 <- rpcResponseParser
--    !r2 <- rpcResponseParser
--    return (r1,r2)
--


