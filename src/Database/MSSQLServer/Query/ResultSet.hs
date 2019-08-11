{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}


module Database.MSSQLServer.Query.ResultSet ( ResultSet (..)
                                            , Result (..)
                                            ) where


import Database.Tds.Message
import Database.MSSQLServer.Query.Row
import Database.MSSQLServer.Query.Only
import Database.MSSQLServer.Query.TokenStreamParser
import Database.MSSQLServer.Query.Template

import Control.Monad(forM)
import Language.Haskell.TH (runIO,pprint)



class ResultSet a where
  resultSetParser :: Parser a


instance ResultSet () where
  resultSetParser = noResult

instance (Row a) => ResultSet [a] where
  resultSetParser = listOfRow

instance ResultSet Int where
  resultSetParser = rowCount


-- [MEMO] using Template Haskell
forM [2..30] $ \n -> do
  dec <- resultSetTupleQ n
--  runIO $ putStrLn $ pprint dec
  return dec
--instance (Result a, Result b) => ResultSet (a, b) where
--  resultSetParser = do
--    !r1 <- resultParser :: (Result a) => Parser a
--    !r2 <- resultParser :: (Result b) => Parser b
--    return  (r1,r2)
--


class Result a where
  resultParser :: Parser a

instance Result () where
  resultParser = noResult

instance Row a => Result [a] where
  resultParser = listOfRow

instance Result Int where
  resultParser = rowCount

