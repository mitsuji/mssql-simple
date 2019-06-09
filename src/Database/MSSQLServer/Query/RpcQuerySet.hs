{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BinaryLiterals #-}


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



class RpcQuerySet a where
  toRpcRequest :: a -> RpcRequest

instance (RpcQueryId a1, RpcParamSet b1) => RpcQuerySet (RpcQuery a1 b1) where
  toRpcRequest (RpcQuery a1 b1) = RpcRequest [r1]
    where
      !r1 = toRpcReqBatch a1 b1

instance (RpcQueryId a1, RpcParamSet b1, RpcQueryId a2, RpcParamSet b2) => RpcQuerySet (RpcQuery a1 b1, RpcQuery a2 b2) where
  toRpcRequest (RpcQuery a1 b1,RpcQuery a2 b2) = RpcRequest [r1,r2]
    where
      !r1 = toRpcReqBatch a1 b1
      !r2 = toRpcReqBatch a2 b2

instance (RpcQueryId a1, RpcParamSet b1, RpcQueryId a2, RpcParamSet b2, RpcQueryId a3, RpcParamSet b3) => RpcQuerySet (RpcQuery a1 b1, RpcQuery a2 b2, RpcQuery a3 b3) where
  toRpcRequest (RpcQuery a1 b1,RpcQuery a2 b2,RpcQuery a3 b3) = RpcRequest [r1,r2,r3]
    where
      !r1 = toRpcReqBatch a1 b1
      !r2 = toRpcReqBatch a2 b2
      !r3 = toRpcReqBatch a3 b3

instance (RpcQueryId a1, RpcParamSet b1, RpcQueryId a2, RpcParamSet b2, RpcQueryId a3, RpcParamSet b3, RpcQueryId a4, RpcParamSet b4) =>
         RpcQuerySet (RpcQuery a1 b1, RpcQuery a2 b2, RpcQuery a3 b3, RpcQuery a4 b4) where
  toRpcRequest (RpcQuery a1 b1,RpcQuery a2 b2,RpcQuery a3 b3,RpcQuery a4 b4) = RpcRequest [r1,r2,r3,r4]
    where
      !r1 = toRpcReqBatch a1 b1
      !r2 = toRpcReqBatch a2 b2
      !r3 = toRpcReqBatch a3 b3
      !r4 = toRpcReqBatch a4 b4

instance (RpcQueryId a1, RpcParamSet b1, RpcQueryId a2, RpcParamSet b2, RpcQueryId a3, RpcParamSet b3, RpcQueryId a4, RpcParamSet b4, RpcQueryId a5, RpcParamSet b5) =>
         RpcQuerySet (RpcQuery a1 b1, RpcQuery a2 b2, RpcQuery a3 b3, RpcQuery a4 b4, RpcQuery a5 b5) where
  toRpcRequest (RpcQuery a1 b1,RpcQuery a2 b2,RpcQuery a3 b3,RpcQuery a4 b4,RpcQuery a5 b5) = RpcRequest [r1,r2,r3,r4,r5]
    where
      !r1 = toRpcReqBatch a1 b1
      !r2 = toRpcReqBatch a2 b2
      !r3 = toRpcReqBatch a3 b3
      !r4 = toRpcReqBatch a4 b4
      !r5 = toRpcReqBatch a5 b5



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


class RpcParamSet a where
  toRpcReqBatchParams :: a -> [RpcReqBatchParam]
  
instance RpcParamSet () where
  toRpcReqBatchParams _ = []

instance (Data a) => RpcParamSet (RpcParam a) where
  toRpcReqBatchParams v1 = [b1]
    where
      !b1 = rpcReqBatchParam v1

instance (Data a, Data b) => RpcParamSet (RpcParam a, RpcParam b) where
  toRpcReqBatchParams (v1,v2) = [b1,b2]
    where
      !b1 = rpcReqBatchParam v1
      !b2 = rpcReqBatchParam v2

instance (Data a, Data b, Data c) => RpcParamSet (RpcParam a, RpcParam b, RpcParam c) where
  toRpcReqBatchParams (v1,v2,v3) = [b1,b2,b3]
    where
      !b1 = rpcReqBatchParam v1
      !b2 = rpcReqBatchParam v2
      !b3 = rpcReqBatchParam v3

instance (Data a, Data b, Data c, Data d) => RpcParamSet (RpcParam a, RpcParam b, RpcParam c, RpcParam d) where
  toRpcReqBatchParams (v1,v2,v3,v4) = [b1,b2,b3,b4]
    where
      !b1 = rpcReqBatchParam v1
      !b2 = rpcReqBatchParam v2
      !b3 = rpcReqBatchParam v3
      !b4 = rpcReqBatchParam v4

instance (Data a, Data b, Data c, Data d, Data e) => RpcParamSet (RpcParam a, RpcParam b, RpcParam c, RpcParam d, RpcParam e) where
  toRpcReqBatchParams (v1,v2,v3,v4,v5) = [b1,b2,b3,b4,b5]
    where
      !b1 = rpcReqBatchParam v1
      !b2 = rpcReqBatchParam v2
      !b3 = rpcReqBatchParam v3
      !b4 = rpcReqBatchParam v4
      !b5 = rpcReqBatchParam v5

instance (Data a, Data b, Data c, Data d, Data e, Data f) => RpcParamSet (RpcParam a, RpcParam b, RpcParam c, RpcParam d, RpcParam e, RpcParam f) where
  toRpcReqBatchParams (v1,v2,v3,v4,v5,v6) = [b1,b2,b3,b4,b5,b6]
    where
      !b1 = rpcReqBatchParam v1
      !b2 = rpcReqBatchParam v2
      !b3 = rpcReqBatchParam v3
      !b4 = rpcReqBatchParam v4
      !b5 = rpcReqBatchParam v5
      !b6 = rpcReqBatchParam v6

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g) => RpcParamSet (RpcParam a, RpcParam b, RpcParam c, RpcParam d, RpcParam e, RpcParam f, RpcParam g) where
  toRpcReqBatchParams (v1,v2,v3,v4,v5,v6,v7) = [b1,b2,b3,b4,b5,b6,b7]
    where
      !b1 = rpcReqBatchParam v1
      !b2 = rpcReqBatchParam v2
      !b3 = rpcReqBatchParam v3
      !b4 = rpcReqBatchParam v4
      !b5 = rpcReqBatchParam v5
      !b6 = rpcReqBatchParam v6
      !b7 = rpcReqBatchParam v7

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h) => RpcParamSet (RpcParam a, RpcParam b, RpcParam c, RpcParam d, RpcParam e, RpcParam f, RpcParam g, RpcParam h) where
  toRpcReqBatchParams (v1,v2,v3,v4,v5,v6,v7,v8) = [b1,b2,b3,b4,b5,b6,b7,b8]
    where
      !b1 = rpcReqBatchParam v1
      !b2 = rpcReqBatchParam v2
      !b3 = rpcReqBatchParam v3
      !b4 = rpcReqBatchParam v4
      !b5 = rpcReqBatchParam v5
      !b6 = rpcReqBatchParam v6
      !b7 = rpcReqBatchParam v7
      !b8 = rpcReqBatchParam v8

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i) => RpcParamSet (RpcParam a, RpcParam b, RpcParam c, RpcParam d, RpcParam e, RpcParam f, RpcParam g, RpcParam h, RpcParam i) where
  toRpcReqBatchParams (v1,v2,v3,v4,v5,v6,v7,v8,v9) = [b1,b2,b3,b4,b5,b6,b7,b8,b9]
    where
      !b1 = rpcReqBatchParam v1
      !b2 = rpcReqBatchParam v2
      !b3 = rpcReqBatchParam v3
      !b4 = rpcReqBatchParam v4
      !b5 = rpcReqBatchParam v5
      !b6 = rpcReqBatchParam v6
      !b7 = rpcReqBatchParam v7
      !b8 = rpcReqBatchParam v8
      !b9 = rpcReqBatchParam v9

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j) => RpcParamSet (RpcParam a, RpcParam b, RpcParam c, RpcParam d, RpcParam e, RpcParam f, RpcParam g, RpcParam h, RpcParam i, RpcParam j) where
  toRpcReqBatchParams (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10) = [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10]
    where
      !b1 = rpcReqBatchParam v1
      !b2 = rpcReqBatchParam v2
      !b3 = rpcReqBatchParam v3
      !b4 = rpcReqBatchParam v4
      !b5 = rpcReqBatchParam v5
      !b6 = rpcReqBatchParam v6
      !b7 = rpcReqBatchParam v7
      !b8 = rpcReqBatchParam v8
      !b9 = rpcReqBatchParam v9
      !b10 = rpcReqBatchParam v10



type RpcParamName  = T.Text

data RpcParam a = RpcParamVal RpcParamName TypeInfo a
                | RpcParamRef RpcParamName TypeInfo a
                | RpcParamDefVal RpcParamName TypeInfo a
                | RpcParamDefRef RpcParamName TypeInfo a
                deriving (Show)



rpcReqBatchParam :: (Data a) => RpcParam a -> RpcReqBatchParam
rpcReqBatchParam = f
  where
    f (RpcParamVal name ti dt) = RpcReqBatchParam name 0b00 ti (toRawBytes ti dt)
    f (RpcParamRef name ti dt) = RpcReqBatchParam name 0b01 ti (toRawBytes ti dt)
    f (RpcParamDefVal name ti dt) = RpcReqBatchParam name 0b10 ti (toRawBytes ti dt)
    f (RpcParamDefRef name ti dt) = RpcReqBatchParam name 0b11 ti (toRawBytes ti dt)


