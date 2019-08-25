{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}


module Database.MSSQLServer.Query.ResultSet ( ResultSet (..)
                                            , Result (..)
                                            ) where


import Control.Applicative ((<$>))
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
#endif



noResult :: Parser' ()
noResult = do
  _ <- trySatisfyMany $ not . isTSDoneOrDoneProc
  _ <- trySatisfy isTSDoneOrDoneProc
  return ()
  where
    isTSDoneOrDoneProc :: TokenStream -> Bool
    isTSDoneOrDoneProc (TSDone{}) = True
    isTSDoneOrDoneProc (TSDoneProc{}) = True
    isTSDoneOrDoneProc _ = False


returnStatus :: Parser' ReturnStatus
returnStatus = do
  _ <- trySatisfyMany $ not . isTSReturnStatus
  TSReturnStatus rets <- trySatisfy isTSReturnStatus
  _ <- trySatisfy isTSDoneProc
  return $ ReturnStatus $ fromIntegral rets
  where
    isTSReturnStatus :: TokenStream -> Bool
    isTSReturnStatus (TSReturnStatus{}) = True
    isTSReturnStatus _ = False

    isTSDoneProc :: TokenStream -> Bool
    isTSDoneProc (TSDoneProc{}) = True
    isTSDoneProc _ = False


rowCount :: Parser' RowCount
rowCount = do
  _ <- trySatisfyMany $ not . isTSDone
  TSDone (Done _ _ rc) <- trySatisfy isTSDone
  return $ RowCount $ fromIntegral rc


listOfRow :: Row a => Parser' ([a])
listOfRow = do
  tsCmd <- trySatisfy isTSColMetaData
  _ <- trySatisfyMany $ not . isTSRow
  tsRows <- trySatisfyMany isTSRow
  _ <- trySatisfyMany $ not . isTSDone -- necesarry ?
  _ <- trySatisfy $ isTSDone
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

    getRawBytes :: RowColumnData -> RawBytes
    getRawBytes (RCDOrdinal dt) = dt
    getRawBytes (RCDLarge _ _ dt) = dt



isTSDone :: TokenStream -> Bool
isTSDone (TSDone{}) = True
isTSDone _ = False







class ResultSet a where
  resultSetParser :: Parser' a


instance ResultSet () where
  resultSetParser = noResult

instance ResultSet RowCount where
  resultSetParser = rowCount

instance ResultSet ReturnStatus where
  resultSetParser = returnStatus

instance (Row a) => ResultSet [a] where
  resultSetParser = listOfRow



-- [MEMO] using Template Haskell
forM [2..30] $ \n -> do
  dec <- resultSetTupleQ n
--  runIO $ putStrLn $ pprint dec
  return dec
--instance (Result a1, Result a2) => ResultSet (a1, a2) where
--  resultSetParser = do
--    !r1 <- resultParser :: (Result a1) => Parser' a1
--    !r2 <- resultParser :: (Result a2) => Parser' a2
--    return  (r1,r2)
--


class Result a where
  resultParser :: Parser' a

instance Result () where
  resultParser = noResult

instance Result RowCount where
  resultParser = rowCount

instance Result ReturnStatus where
  resultParser = returnStatus

instance Row a => Result [a] where
  resultParser = listOfRow


