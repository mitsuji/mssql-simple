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
                                              ) where


import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Word (Word16(..))

import Database.Tds.Message
import Database.MSSQLServer.Query.Only
import Database.MSSQLServer.Query.Template

import Control.Monad(forM)
import Language.Haskell.TH (runIO,pprint)

import Data.Time (UTCTime(..))
import Data.UUID.Types (UUID)
import Data.Fixed (Fixed(..),HasResolution(..))


data RpcQuery a b = RpcQuery !a !b
                  deriving (Show)

-- | There several ways provided for specify stored procedures.
-- See ProcID section of [\[MS-TDS\] 2.2.6.6 RPC Request](https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-tds/619c43b6-9495-4a58-9e49-a4950db245b3).
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

data RpcParam a = RpcParamVal !RpcParamName !TypeInfo !a
                | RpcParamRef !RpcParamName !TypeInfo !a
                | RpcParamDefVal !RpcParamName !TypeInfo !a
                | RpcParamDefRef !RpcParamName !TypeInfo !a
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
forM [2..60] $ \n -> do
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


decimalScale :: (HasResolution a) => Fixed a -> Scale
decimalScale = digits . resolution
  where
    digits :: Integer -> Scale
    digits = truncate . (logBase 10.0) . fromInteger


bitVal :: (Integral a) => RpcParamName -> (Maybe a) -> RpcParam (Maybe a)
bitVal name n = RpcParamVal name TIBitN n

tinyintVal :: (Integral a) => RpcParamName -> (Maybe a) -> RpcParam (Maybe a)
tinyintVal name n = RpcParamVal name TIIntN1 n

smallintVal :: (Integral a) => RpcParamName -> (Maybe a) -> RpcParam (Maybe a)
smallintVal name n = RpcParamVal name TIIntN2 n

intVal :: (Integral a) => RpcParamName -> (Maybe a) -> RpcParam (Maybe a)
intVal name n = RpcParamVal name TIIntN4 n

bigintVal :: (Integral a) => RpcParamName -> (Maybe a) -> RpcParam (Maybe a)
bigintVal name n = RpcParamVal name TIIntN8 n

smallmoneyVal :: RpcParamName -> (Maybe Money) -> RpcParam (Maybe Money)
smallmoneyVal name m = RpcParamVal name TIMoneyN4 m

moneyVal :: RpcParamName -> (Maybe Money) -> RpcParam (Maybe Money)
moneyVal name m = RpcParamVal name TIMoneyN8 m

smalldatetimeVal :: RpcParamName -> (Maybe UTCTime) -> RpcParam (Maybe UTCTime)
smalldatetimeVal name dt = RpcParamVal name TIDateTimeN4 dt

datetimeVal :: RpcParamName -> (Maybe UTCTime) -> RpcParam (Maybe UTCTime)
datetimeVal name dt = RpcParamVal name TIDateTimeN8 dt

float24Val :: (Fractional a) => RpcParamName -> (Maybe a) -> RpcParam (Maybe a)
float24Val name n = RpcParamVal name TIFltN4 n

realVal :: (Fractional a) => RpcParamName -> (Maybe a) -> RpcParam (Maybe a)
realVal = float24Val

float53Val :: (Fractional a) => RpcParamName -> (Maybe a) -> RpcParam (Maybe a)
float53Val name n = RpcParamVal name TIFltN8 n

doubleVal :: (Fractional a) => RpcParamName -> (Maybe a) -> RpcParam (Maybe a)
doubleVal = float53Val

uniqueidentifierVal :: RpcParamName -> (Maybe UUID) -> RpcParam (Maybe UUID)
uniqueidentifierVal name uuid = RpcParamVal name TIGUID uuid

decimalVal :: (HasResolution a) => RpcParamName -> Precision -> (Either Scale (Fixed a)) -> RpcParam (Maybe (Fixed a))
decimalVal name p (Left s) = RpcParamVal name (TIDecimalN p s) Nothing
decimalVal name p (Right f) = RpcParamVal name (TIDecimalN p (decimalScale f)) (Just f)

numericVal :: (HasResolution a) => RpcParamName -> Precision -> (Either Scale (Fixed a)) -> RpcParam (Maybe (Fixed a))
numericVal name p (Left s) = RpcParamVal name (TINumericN p s) Nothing
numericVal name p (Right f) = RpcParamVal name (TINumericN p (decimalScale f)) (Just f)

charVal :: RpcParamName -> (Maybe B.ByteString) -> RpcParam (Maybe B.ByteString)
charVal name Nothing = RpcParamVal name (TIBigChar 0 (Collation 0x00000000 0x00)) Nothing
charVal name (Just bs) = RpcParamVal name (TIBigChar (fromIntegral $ B.length bs) (Collation 0x00000000 0x00)) (Just bs)

varcharVal :: RpcParamName -> (Maybe B.ByteString) -> RpcParam (Maybe B.ByteString)
varcharVal name Nothing = RpcParamVal name (TIBigVarChar 0 (Collation 0x00000000 0x00)) Nothing
varcharVal name (Just bs) = RpcParamVal name (TIBigVarChar (fromIntegral $ B.length bs) (Collation 0x00000000 0x00)) (Just bs)

textVal :: RpcParamName -> (Maybe B.ByteString) -> RpcParam (Maybe B.ByteString)
textVal name Nothing = RpcParamVal name (TIText 0 (Collation 0x00000000 0x00)) Nothing
textVal name (Just bs) = RpcParamVal name (TIText (fromIntegral $ B.length bs) (Collation 0x00000000 0x00)) (Just bs)

ncharVal :: RpcParamName -> (Maybe T.Text) -> RpcParam (Maybe T.Text)
ncharVal name Nothing = RpcParamVal name (TINChar 0 (Collation 0x00000000 0x00)) Nothing
ncharVal name (Just ts) = RpcParamVal name (TINChar (fromIntegral $ (T.length ts) * 2) (Collation 0x00000000 0x00)) (Just ts)

nvarcharVal :: RpcParamName -> (Maybe T.Text) -> RpcParam (Maybe T.Text)
nvarcharVal name Nothing = RpcParamVal name (TINVarChar 0 (Collation 0x00000000 0x00)) Nothing
nvarcharVal name (Just ts) = RpcParamVal name (TINVarChar (fromIntegral $ (T.length ts) * 2) (Collation 0x00000000 0x00)) (Just ts)

ntextVal :: RpcParamName -> (Maybe T.Text) -> RpcParam (Maybe T.Text)
ntextVal name Nothing = RpcParamVal name (TINText 0 (Collation 0x00000000 0x00)) Nothing
ntextVal name (Just ts) = RpcParamVal name (TINText (fromIntegral $ (T.length ts) * 2) (Collation 0x00000000 0x00)) (Just ts)

binaryVal :: RpcParamName -> (Maybe B.ByteString) -> RpcParam (Maybe B.ByteString)
binaryVal name Nothing = RpcParamVal name (TIBigBinary 0) Nothing
binaryVal name (Just bs) = RpcParamVal name (TIBigBinary (fromIntegral $ B.length bs)) (Just bs)

varbinaryVal :: RpcParamName -> (Maybe B.ByteString) -> RpcParam (Maybe B.ByteString)
varbinaryVal name Nothing = RpcParamVal name (TIBigVarBinary 0) Nothing
varbinaryVal name (Just bs) = RpcParamVal name (TIBigVarBinary (fromIntegral $ B.length bs)) (Just bs)

imageVal :: RpcParamName -> (Maybe B.ByteString) -> RpcParam (Maybe B.ByteString)
imageVal name Nothing = RpcParamVal name (TIImage 0) Nothing
imageVal name (Just bs) = RpcParamVal name (TIImage (fromIntegral $ B.length bs)) (Just bs)



bitRef :: (Integral a) => RpcParamName -> (Maybe a) -> RpcParam (Maybe a)
bitRef name n = RpcParamRef name TIBitN n

tinyintRef :: (Integral a)  => RpcParamName -> (Maybe a) -> RpcParam (Maybe a)
tinyintRef name n = RpcParamRef name TIIntN1 n

smallintRef :: (Integral a) => RpcParamName -> (Maybe a) -> RpcParam (Maybe a)
smallintRef name n = RpcParamRef name TIIntN2 n

intRef :: (Integral a) => RpcParamName -> (Maybe a) -> RpcParam (Maybe a)
intRef name n = RpcParamRef name TIIntN4 n

bigintRef :: (Integral a) => RpcParamName -> (Maybe a) -> RpcParam (Maybe a)
bigintRef name n = RpcParamRef name TIIntN8 n

smallmoneyRef :: RpcParamName -> (Maybe Money) -> RpcParam (Maybe Money)
smallmoneyRef name m = RpcParamRef name TIMoneyN4 m

moneyRef :: RpcParamName -> (Maybe Money) -> RpcParam (Maybe Money)
moneyRef name m = RpcParamRef name TIMoneyN8 m

smalldatetimeRef :: RpcParamName -> (Maybe UTCTime) -> RpcParam (Maybe UTCTime)
smalldatetimeRef name dt = RpcParamRef name TIDateTimeN4 dt

datetimeRef :: RpcParamName -> (Maybe UTCTime) -> RpcParam (Maybe UTCTime)
datetimeRef name dt = RpcParamRef name TIDateTimeN8 dt

float24Ref :: (Fractional a) => RpcParamName -> (Maybe a) -> RpcParam (Maybe a)
float24Ref name n = RpcParamRef name TIFltN4 n

realRef :: (Fractional a) => RpcParamName -> (Maybe a) -> RpcParam (Maybe a)
realRef = float24Ref

float53Ref :: (Fractional a) => RpcParamName -> (Maybe a) -> RpcParam (Maybe a)
float53Ref name n = RpcParamRef name TIFltN8 n

doubleRef :: (Fractional a) => RpcParamName -> (Maybe a) -> RpcParam (Maybe a)
doubleRef = float53Ref

uniqueidentifierRef :: RpcParamName -> (Maybe UUID) -> RpcParam (Maybe UUID)
uniqueidentifierRef name uuid = RpcParamRef name TIGUID uuid

decimalRef :: (HasResolution a) => RpcParamName -> Precision -> (Either Scale (Fixed a)) -> RpcParam (Maybe (Fixed a))
decimalRef name p (Left s) = RpcParamRef name (TIDecimalN p s) Nothing
decimalRef name p (Right f) = RpcParamRef name (TIDecimalN p (decimalScale f)) (Just f)

numericRef :: (HasResolution a) => RpcParamName -> Precision -> (Either Scale (Fixed a)) -> RpcParam (Maybe (Fixed a))
numericRef name p (Left s) = RpcParamRef name (TINumericN p s) Nothing
numericRef name p (Right f) = RpcParamRef name (TINumericN p (decimalScale f)) (Just f)

charRef :: RpcParamName -> Int -> (Maybe B.ByteString) -> RpcParam (Maybe B.ByteString)
charRef name len bs = RpcParamRef name (TIBigChar (fromIntegral len) (Collation 0x00000000 0x00)) bs

varcharRef :: RpcParamName -> Int -> (Maybe B.ByteString) -> RpcParam (Maybe B.ByteString)
varcharRef name len bs = RpcParamRef name (TIBigVarChar (fromIntegral len) (Collation 0x00000000 0x00)) bs

textRef :: RpcParamName -> Int -> (Maybe B.ByteString) -> RpcParam (Maybe B.ByteString)
textRef name len bs = RpcParamRef name (TIText (fromIntegral len) (Collation 0x00000000 0x00)) bs

ncharRef :: RpcParamName -> Int -> (Maybe T.Text) -> RpcParam (Maybe T.Text)
ncharRef name len ts = RpcParamRef name (TINChar (fromIntegral $ len *2) (Collation 0x00000000 0x00)) ts

nvarcharRef :: RpcParamName -> Int -> (Maybe T.Text) -> RpcParam (Maybe T.Text)
nvarcharRef name len ts = RpcParamRef name (TINVarChar (fromIntegral $ len *2) (Collation 0x00000000 0x00)) ts

ntextRef :: RpcParamName -> Int -> (Maybe T.Text) -> RpcParam (Maybe T.Text)
ntextRef name len ts = RpcParamRef name (TINText (fromIntegral $ len *2) (Collation 0x00000000 0x00)) ts

binaryRef :: RpcParamName -> Int -> (Maybe B.ByteString) -> RpcParam (Maybe B.ByteString)
binaryRef name len bs = RpcParamRef name (TIBigBinary (fromIntegral len)) bs

varbinaryRef :: RpcParamName -> Int -> (Maybe B.ByteString) -> RpcParam (Maybe B.ByteString)
varbinaryRef name len bs = RpcParamRef name (TIBigVarBinary (fromIntegral len)) bs

imageRef :: RpcParamName -> Int -> (Maybe B.ByteString) -> RpcParam (Maybe B.ByteString)
imageRef name len bs = RpcParamRef name (TIImage (fromIntegral len)) bs



bitDefRef :: (Integral a) => RpcParamName -> RpcParam (Maybe a)
bitDefRef name = RpcParamDefRef name TIBitN Nothing

tinyintDefRef :: (Integral a)  => RpcParamName -> RpcParam (Maybe a)
tinyintDefRef name = RpcParamDefRef name TIIntN1 Nothing

smallintDefRef :: (Integral a) => RpcParamName -> RpcParam (Maybe a)
smallintDefRef name = RpcParamDefRef name TIIntN2 Nothing

intDefRef :: (Integral a) => RpcParamName -> RpcParam (Maybe a)
intDefRef name = RpcParamDefRef name TIIntN4 Nothing

bigintDefRef :: (Integral a) => RpcParamName -> RpcParam (Maybe a)
bigintDefRef name = RpcParamDefRef name TIIntN8 Nothing

smallmoneyDefRef :: RpcParamName -> RpcParam (Maybe Money)
smallmoneyDefRef name = RpcParamDefRef name TIMoneyN4 Nothing

moneyDefRef :: RpcParamName -> RpcParam (Maybe Money)
moneyDefRef name = RpcParamDefRef name TIMoneyN8 Nothing

smalldatetimeDefRef :: RpcParamName -> RpcParam (Maybe UTCTime)
smalldatetimeDefRef name = RpcParamDefRef name TIDateTimeN4 Nothing

datetimeDefRef :: RpcParamName -> RpcParam (Maybe UTCTime)
datetimeDefRef name = RpcParamDefRef name TIDateTimeN8 Nothing

float24DefRef :: (Fractional a) => RpcParamName -> RpcParam (Maybe a)
float24DefRef name = RpcParamDefRef name TIFltN4 Nothing

realDefRef :: (Fractional a) => RpcParamName -> RpcParam (Maybe a)
realDefRef = float24DefRef

float53DefRef :: (Fractional a) => RpcParamName -> RpcParam (Maybe a)
float53DefRef name = RpcParamDefRef name TIFltN8 Nothing

doubleDefRef :: (Fractional a) => RpcParamName -> RpcParam (Maybe a)
doubleDefRef = float53DefRef

uniqueidentifierDefRef :: RpcParamName -> RpcParam (Maybe UUID)
uniqueidentifierDefRef name = RpcParamDefRef name TIGUID Nothing

decimalDefRef :: (HasResolution a) => RpcParamName -> Precision -> Scale -> RpcParam (Maybe (Fixed a))
decimalDefRef name p s = RpcParamDefRef name (TIDecimalN p s) Nothing

numericDefRef :: (HasResolution a) => RpcParamName -> Precision -> Scale -> RpcParam (Maybe (Fixed a))
numericDefRef name p s = RpcParamDefRef name (TINumericN p s) Nothing

charDefRef :: RpcParamName -> Int -> RpcParam (Maybe B.ByteString)
charDefRef name len = RpcParamDefRef name (TIBigChar (fromIntegral len) (Collation 0x00000000 0x00)) Nothing

varcharDefRef :: RpcParamName -> Int -> RpcParam (Maybe B.ByteString)
varcharDefRef name len = RpcParamDefRef name (TIBigVarChar (fromIntegral len) (Collation 0x00000000 0x00)) Nothing

textDefRef :: RpcParamName -> Int -> RpcParam (Maybe B.ByteString)
textDefRef name len = RpcParamDefRef name (TIText (fromIntegral len) (Collation 0x00000000 0x00)) Nothing

ncharDefRef :: RpcParamName -> Int -> RpcParam (Maybe T.Text)
ncharDefRef name len = RpcParamDefRef name (TINChar (fromIntegral $ len *2) (Collation 0x00000000 0x00)) Nothing

nvarcharDefRef :: RpcParamName -> Int -> RpcParam (Maybe T.Text)
nvarcharDefRef name len = RpcParamDefRef name (TINVarChar (fromIntegral $ len *2) (Collation 0x00000000 0x00)) Nothing

ntextDefRef :: RpcParamName -> Int -> RpcParam (Maybe T.Text)
ntextDefRef name len = RpcParamDefRef name (TINText (fromIntegral $ len *2) (Collation 0x00000000 0x00)) Nothing

binaryDefRef :: RpcParamName -> Int -> RpcParam (Maybe B.ByteString)
binaryDefRef name len = RpcParamDefRef name (TIBigBinary (fromIntegral len)) Nothing

varbinaryDefRef :: RpcParamName -> Int -> RpcParam (Maybe B.ByteString)
varbinaryDefRef name len = RpcParamDefRef name (TIBigVarBinary (fromIntegral len)) Nothing

imageDefRef :: RpcParamName -> Int -> RpcParam (Maybe B.ByteString)
imageDefRef name len = RpcParamDefRef name (TIImage (fromIntegral len)) Nothing


