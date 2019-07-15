{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}

module Database.MSSQLServer.Query.ResultSet ( ResultSet (..)
                                            , Result
                                            ) where


import Control.Applicative((<$>),Applicative((<*>)),Alternative(empty))
import Database.Tds.Message
import Database.MSSQLServer.Query.Row
import Database.MSSQLServer.Query.Only
import Database.MSSQLServer.Query.TokenStreamParser



class ResultSet a where
  fromTokenStreams :: [TokenStream] -> a


instance ResultSet () where
  fromTokenStreams xs = case parse noResult xs of
                          [] -> error "fromTokenStreams(ResultSet ()): failed to parse"
                          (x,_):_ -> x

instance (Row a) => ResultSet [a] where
  fromTokenStreams xs = case parse listOfRow xs of
                          [] -> error "fromTokenStreams(ResultSet [Row a]): failed to parse"
                          (x,_):_ -> x

instance ResultSet Int where
  fromTokenStreams xs = case parse rowCount xs of
                          [] -> error "fromTokenStreams(ResultSet Int): failed to parse"
                          (x,_):_ -> x


-- [TODO] use Template Haskell
instance (Result a, Result b) => ResultSet (a, b) where
  fromTokenStreams xs = case parse p xs of
                          [] -> error "fromTokenStreams(ResultSet (Result a, Result b)): failed to parse"
                          (x,_):_ -> x
    where
      p :: (Result a, Result b) => Parser (a, b)
      p = do
        !r1 <- resultParser :: (Result a) => Parser a
        !r2 <- resultParser :: (Result b) => Parser b
        return  (r1,r2)

instance (Result a, Result b, Result c) => ResultSet (a, b, c) where
  fromTokenStreams xs = case parse p xs of
                          [] -> error "fromTokenStreams(ResultSet (Result a, Result b, Result c)): failed to parse"
                          (x,_):_ -> x
    where
      p :: (Result a, Result b, Result c) => Parser (a, b, c)
      p = do
        !r1 <- resultParser :: (Result a) => Parser a
        !r2 <- resultParser :: (Result b) => Parser b
        !r3 <- resultParser :: (Result c) => Parser c
        return  (r1,r2,r3)

instance (Result a, Result b, Result c, Result d) => ResultSet (a, b, c, d) where
  fromTokenStreams xs = case parse p xs of
                          [] -> error "fromTokenStreams(ResultSet (Result a, Result b, Result c, Result d)): failed to parse"
                          (x,_):_ -> x
    where
      p :: (Result a, Result b, Result c, Result d) => Parser (a, b, c, d)
      p = do
        !r1 <- resultParser :: (Result a) => Parser a
        !r2 <- resultParser :: (Result b) => Parser b
        !r3 <- resultParser :: (Result c) => Parser c
        !r4 <- resultParser :: (Result d) => Parser d
        return  (r1,r2,r3,r4)

instance (Result a, Result b, Result c, Result d, Result e) => ResultSet (a, b, c, d, e) where
  fromTokenStreams xs = case parse p xs of
                          [] -> error "fromTokenStreams(ResultSet (Result a, Result b, Result c, Result d, Result e)): failed to parse"
                          (x,_):_ -> x
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

