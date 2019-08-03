{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}

module Database.MSSQLServer.Query.ResultSet ( ResultSet (..)
                                            , Result (..)
                                            ) where


import Database.Tds.Message
import Database.MSSQLServer.Query.Row
import Database.MSSQLServer.Query.Only
import Database.MSSQLServer.Query.TokenStreamParser



class ResultSet a where
  resultSetParser :: Parser a


instance ResultSet () where
  resultSetParser = noResult

instance (Row a) => ResultSet [a] where
  resultSetParser = listOfRow

instance ResultSet Int where
  resultSetParser = rowCount


-- [TODO] use Template Haskell
instance (Result a, Result b) => ResultSet (a, b) where
  resultSetParser = p
    where
      p :: (Result a, Result b) => Parser (a, b)
      p = do
        !r1 <- resultParser :: (Result a) => Parser a
        !r2 <- resultParser :: (Result b) => Parser b
        return  (r1,r2)

instance (Result a, Result b, Result c) => ResultSet (a, b, c) where
  resultSetParser = p
    where
      p :: (Result a, Result b, Result c) => Parser (a, b, c)
      p = do
        !r1 <- resultParser :: (Result a) => Parser a
        !r2 <- resultParser :: (Result b) => Parser b
        !r3 <- resultParser :: (Result c) => Parser c
        return  (r1,r2,r3)

instance (Result a, Result b, Result c, Result d) => ResultSet (a, b, c, d) where
  resultSetParser = p
    where
      p :: (Result a, Result b, Result c, Result d) => Parser (a, b, c, d)
      p = do
        !r1 <- resultParser :: (Result a) => Parser a
        !r2 <- resultParser :: (Result b) => Parser b
        !r3 <- resultParser :: (Result c) => Parser c
        !r4 <- resultParser :: (Result d) => Parser d
        return  (r1,r2,r3,r4)

instance (Result a, Result b, Result c, Result d, Result e) => ResultSet (a, b, c, d, e) where
  resultSetParser = p
    where
      p :: (Result a, Result b, Result c, Result d, Result e) => Parser (a, b, c, d, e)
      p = do
        !r1 <- resultParser :: (Result a) => Parser a
        !r2 <- resultParser :: (Result b) => Parser b
        !r3 <- resultParser :: (Result c) => Parser c
        !r4 <- resultParser :: (Result d) => Parser d
        !r5 <- resultParser :: (Result e) => Parser e
        return  (r1,r2,r3,r4,r5)



class Result a where
  resultParser :: Parser a

instance Result () where
  resultParser = noResult

instance Row a => Result [a] where
  resultParser = listOfRow

instance Result Int where
  resultParser = rowCount

