{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.MSSQLServer.Query.Row ( Row (..)
                                      , RowCount (..)
                                      , ReturnStatus (..)
                                      ) where

import Database.Tds.Message
import Database.MSSQLServer.Query.Only
import Database.MSSQLServer.Query.Template

import Control.Monad(forM)
import Language.Haskell.TH (runIO,pprint)


newtype RowCount = RowCount Int
newtype ReturnStatus = ReturnStatus Int

mcdTypeInfo :: MetaColumnData -> TypeInfo
mcdTypeInfo (MetaColumnData _ _ ti _ _) = ti


class Row a where
  fromListOfRawBytes :: [MetaColumnData] -> [RawBytes] -> a

instance (Data a) => Row (Only a) where
  fromListOfRawBytes [m1] [b1] = Only d1
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 1"

-- [MEMO] using Template Haskell
forM [2..30] $ \n -> do
  dec <- rowTupleQ n
--  runIO $ putStrLn $ pprint dec
  return dec
--instance (Data a1, Data a2) => Row (a1,a2) where
--  fromListOfRawBytes [m1,m2] [b1,b2] = (d1,d2)
--    where
--      !d1 = fromRawBytes (mcdTypeInfo m1) b1
--      !d2 = fromRawBytes (mcdTypeInfo m2) b2
--  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 2"
--
