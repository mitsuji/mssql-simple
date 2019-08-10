{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}


module Database.MSSQLServer.Query.RpcQuerySet ( RpcQuerySet (..)
                                              , RpcQuery (..)
                                              , RpcQueryId (..)
                                              , StoredProcedure (..)
                                              , RpcParamSet (..)
                                              , RpcParam (..)
                                              , RpcParamName
                                              , rpcReqBatchParam
                                              ) where


import qualified Data.Text as T
import Data.Word (Word16(..))

import Database.Tds.Message
import Database.MSSQLServer.Query.Only
import Database.MSSQLServer.Query.Template

import Control.Monad(forM)
import Language.Haskell.TH (runIO,pprint)


data RpcQuery a b = RpcQuery a b
                  deriving (Show)

class RpcQueryId a where
  toRpcReqBatch :: (RpcParamSet b) => a -> b -> RpcReqBatch

instance RpcQueryId Word16 where
  toRpcReqBatch id ps = RpcReqBatchProcId id 0x0000 $ toRpcReqBatchParams ps
  
instance RpcQueryId T.Text where
  toRpcReqBatch name ps = RpcReqBatchProcName name 0x0000 $ toRpcReqBatchParams ps

data StoredProcedure = SP_Cursor 
                     | SP_CursorOpen
                     | SP_CursorPrepare
                     | SP_CursorExecute
                     | SP_CursorPrepExec
                     | SP_CursorUnprepare
                     | SP_CursorFetch
                     | SP_CursorOption
                     | SP_CursorClose
                     | SP_ExecuteSql
                     | SP_Prepare
                     | SP_Execute
                     | SP_PrepExec
                     | SP_PrepExecRpc
                     | SP_Unprepare
                     deriving (Show,Enum,Bounded)

instance RpcQueryId StoredProcedure where
  toRpcReqBatch sp ps = RpcReqBatchProcId (fromIntegral $ (fromEnum sp) +1) 0x0000 $ toRpcReqBatchParams ps


type RpcParamName  = T.Text

data RpcParam a = RpcParamVal RpcParamName TypeInfo a
                | RpcParamRef RpcParamName TypeInfo a
                | RpcParamDefVal RpcParamName TypeInfo a
                | RpcParamDefRef RpcParamName TypeInfo a
                deriving (Show)



rpcReqBatchParam :: (Data a) => RpcParam a -> RpcReqBatchParam
rpcReqBatchParam = f
  where
    f (RpcParamVal name ti dt) = RpcReqBatchParam name 0 ti (toRawBytes ti dt)
    f (RpcParamRef name ti dt) = RpcReqBatchParam name 1 ti (toRawBytes ti dt)
    f (RpcParamDefVal name ti dt) = RpcReqBatchParam name 2 ti (toRawBytes ti dt)
    f (RpcParamDefRef name ti dt) = RpcReqBatchParam name 3 ti (toRawBytes ti dt)



class RpcParamSet a where
  toRpcReqBatchParams :: a -> [RpcReqBatchParam]

instance RpcParamSet () where
  toRpcReqBatchParams _ = []

instance (Data a) => RpcParamSet (RpcParam a) where
  toRpcReqBatchParams v1 = [b1]
    where
      !b1 = rpcReqBatchParam v1

-- [MEMO] using Template Haskell
forM [2..30] $ \n -> do
  dec <- rpcParamSetTupleQ n
--  runIO $ putStrLn $ pprint dec
  return dec
--instance (Data a1, Data a2) => RpcParamSet (RpcParam a1, RpcParam a2) where
--  toRpcReqBatchParams (d1,d2) = [p1,bp]
--    where
--      !p1 = rpcReqBatchParam d1
--      !p2 = rpcReqBatchParam d2
--




class RpcQuerySet a where
  toRpcRequest :: a -> RpcRequest

instance (RpcQueryId a1, RpcParamSet b1) => RpcQuerySet (RpcQuery a1 b1) where
  toRpcRequest (RpcQuery a1 b1) = RpcRequest [r1]
    where
      !r1 = toRpcReqBatch a1 b1

-- [MEMO] using Template Haskell
forM [2..30] $ \n -> do
  dec <- rpcQuerySetTupleQ n
--  runIO $ putStrLn $ pprint dec
  return dec
--instance (RpcQueryId a1, RpcParamSet b1, RpcQueryId a2, RpcParamSet b2) => RpcQuerySet (RpcQuery a1 b1, RpcQuery a2 b2) where
--  toRpcRequest (RpcQuery a1 b1,RpcQuery a2 b2) = RpcRequest [r1,r2]
--    where
--      !r1 = toRpcReqBatch a1 b1
--      !r2 = toRpcReqBatch a2 b2
--
