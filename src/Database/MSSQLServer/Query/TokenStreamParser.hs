{-# LANGUAGE CPP #-}

module Database.MSSQLServer.Query.TokenStreamParser ( Parser(..)
                                                    , parse
                                                    , item
                                                    , satisfy
                                                    , many
                                                    , many1
                                                    , oneOf
                                                    , noneOf
                                                    , noResult
                                                    , listOfRow
                                                    , rowCount
                                                    ) where


import Control.Applicative((<$>))
import Control.Applicative(Applicative((<*>),pure),Alternative((<|>),empty))
import Control.Monad(Monad(..),MonadPlus(..),ap)
#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail(MonadFail(..))
#endif
import Data.Monoid (mconcat,(<>),All(..),Any(..))

import Database.Tds.Message
import Database.MSSQLServer.Query.Row



data Parser a = Parser ([TokenStream] -> [(a,[TokenStream])])

parse :: Parser a -> [TokenStream] -> [(a,[TokenStream])]
parse (Parser p) = p


instance Functor Parser where
  fmap f p = Parser $ \xs -> [(f x,xs') | (x,xs') <- parse p xs]

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

instance Monad Parser where
  return x = Parser $ \xs -> [(x,xs)]
  p >>= f  = Parser $ \ts -> mconcat [parse (f t) ts' | (t,ts') <- parse p ts]

instance MonadPlus Parser where
  mzero = Parser $ \_ -> []
  mplus p q = Parser $ \xs -> parse p xs <> parse q xs

#if MIN_VERSION_base(4,9,0)
instance MonadFail Parser where
  fail _ = mzero
#endif


item :: Parser TokenStream
item = Parser $ \xs -> case xs of
                         [] -> []
                         (x:xs') -> [(x,xs')]

satisfy :: (TokenStream -> Bool) -> Parser TokenStream
satisfy f = do x <- item
               if f x
                 then return x
                 else empty

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do a <- p
             as <- many p
             return $ a:as

oneOf :: [TokenStream -> Bool] -> Parser TokenStream
oneOf xs = satisfy $ \x -> getAny $ mconcat $ (\f -> Any $ f x) <$> xs

noneOf :: [TokenStream -> Bool] -> Parser TokenStream
noneOf xs = satisfy $ \x -> getAll $ mconcat $ (\f -> All $ not $ f x) <$> xs


noResult :: Parser ()
noResult = do
  _ <- many $ satisfy $ not . isTSDone
  _ <- satisfy isTSDone -- [MEMO] just parse here
  return ()
    where

      -- [TODO] check Status contains 0x10
      isTSDone :: TokenStream -> Bool
      isTSDone (TSDone{}) = True
      isTSDone (TSDoneInProc{}) = True
      isTSDone _ = False
  

listOfRow :: Row a => Parser ([a])
listOfRow = do
  _ <- many $ satisfy $ not . isTSColMetaData
  tsCmd <- satisfy isTSColMetaData
  _ <- many $ satisfy $ not . isTSRow
  tsRows <- many $ satisfy isTSRow
  _ <- many $ satisfy $ not . isTSDone
  _ <- satisfy isTSDone -- [MEMO] just parse here
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

      -- [TODO] check Status contains 0x10
      isTSDone :: TokenStream -> Bool
      isTSDone (TSDone{}) = True
      isTSDone (TSDoneInProc{}) = True
      isTSDone _ = False

      getRawBytes :: RowColumnData -> RawBytes
      getRawBytes (RCDOrdinal dt) = dt
      getRawBytes (RCDLarge _ _ dt) = dt


rowCount :: Parser Int
rowCount = do
  _ <- many $ satisfy $ not . isTSDone
  tsDone <- satisfy isTSDone
  return $
    let
      Done _ _ rc = getDone tsDone
    in fromIntegral rc
    where

      -- [TODO] check Status contains 0x10
      isTSDone :: TokenStream -> Bool
      isTSDone (TSDone{}) = True
      isTSDone (TSDoneInProc{}) = True
      isTSDone _ = False

      getDone :: TokenStream -> Done
      getDone (TSDone x) = x
      getDone (TSDoneInProc x) = x
      getDone _ = error "rowCount: TSDone and TSDoneInProc are only possible here"

