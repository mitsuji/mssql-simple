{-# LANGUAGE CPP #-}

module Database.MSSQLServer.Query.TokenStreamParser ( Parser(..)
                                                    , parse
                                                    , item
                                                    , satisfy
                                                    , satisfyNotError
                                                    , Parser'(..)
                                                    , isTSError
                                                    , isTSDoneOrDoneProc
                                                    , isTSDone
                                                    , isTSDoneProc
                                                    , isFinalTSDoneOrDoneProc
                                                    , isFinalTSDone
                                                    , isFinalTSDoneProc
                                                    ) where


import Control.Applicative(Applicative((<*>),pure),Alternative((<|>),empty),(<$>))
import Control.Monad(Monad(..),MonadPlus(..),ap)
import Data.Monoid ((<>),mconcat)
#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail(MonadFail(..))
#endif

import Database.Tds.Message
import Database.MSSQLServer.Query.Row

import Data.Bits ((.&.))


#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except
#else
import Control.Monad.Error
import qualified Data.Text as T
#endif

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

satisfyNotError :: (TokenStream -> Bool) -> Parser TokenStream
satisfyNotError f = satisfy (\x -> f x && (not . isTSError) x)



#if MIN_VERSION_mtl(2,2,1)
type Parser' = ExceptT Info Parser
#else
type Parser' = ErrorT Info Parser
instance Error Info where
  noMsg = Info 0 0 0 (T.pack "") (T.pack "") (T.pack "") 0
#endif


isTSError :: TokenStream -> Bool
isTSError (TSError{}) = True
isTSError _ = False

isTSDoneOrDoneProc :: TokenStream -> Bool
isTSDoneOrDoneProc (TSDone{}) = True
isTSDoneOrDoneProc (TSDoneProc{}) = True
isTSDoneOrDoneProc _ = False

isTSDone :: TokenStream -> Bool
isTSDone (TSDone{}) = True
isTSDone _ = False

isTSDoneProc :: TokenStream -> Bool
isTSDoneProc (TSDoneProc{}) = True
isTSDoneProc _ = False


isFinalTSDoneOrDoneProc :: TokenStream -> Bool
isFinalTSDoneOrDoneProc = (||) <$> isFinalTSDone <*> isFinalTSDoneProc

isFinalTSDone :: TokenStream -> Bool
isFinalTSDone = f
  where
    f :: TokenStream -> Bool
    f (TSDone x) = isFinalDone x
    f _ = False

isFinalTSDoneProc :: TokenStream -> Bool
isFinalTSDoneProc = f
  where
    f :: TokenStream -> Bool
    f (TSDoneProc x) = isFinalDone x
    f _ = False

isFinalDone :: Done -> Bool
isFinalDone (Done st _ _) = not $ st .&. 0x01 == 0x01 -- [MEMO] 0x01 more bit



