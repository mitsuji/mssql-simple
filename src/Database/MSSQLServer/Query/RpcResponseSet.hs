{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.MSSQLServer.Query.RpcResponseSet ( RpcResponseSet (..)
                                                 , RpcResponse (..)
                                                 , RpcResultSet
                                                 , RpcResult
                                                 , RpcOutputSet (..)
                                                 ) where

import Control.Applicative((<$>),(<*>))
import Database.Tds.Message
import Database.MSSQLServer.Query.Row
import Database.MSSQLServer.Query.Only

import Database.MSSQLServer.Query.TokenStreamParser




class RpcResponseSet a where
  fromListOfTokenStreams :: [[TokenStream]] -> a

-- [TODO] use Template Haskell
instance (RpcOutputSet a1, RpcResultSet b1) => RpcResponseSet (RpcResponse a1 b1) where
  fromListOfTokenStreams [v1] = b1
    where
      !b1 = rpcResponse v1
  fromListOfTokenStreams _ = error "fromListOfTokenStreams: List length must be 1"
  
instance (RpcOutputSet a1, RpcResultSet b1, RpcOutputSet a2, RpcResultSet b2) => RpcResponseSet (RpcResponse a1 b1, RpcResponse a2 b2) where
  fromListOfTokenStreams [v1,v2] =  (b1, b2)
    where
      !b1 = rpcResponse v1
      !b2 = rpcResponse v2
  fromListOfTokenStreams _ = error "fromListOfTokenStreams: List length must be 2"

instance (RpcOutputSet a1, RpcResultSet b1, RpcOutputSet a2, RpcResultSet b2, RpcOutputSet a3, RpcResultSet b3) => RpcResponseSet (RpcResponse a1 b1, RpcResponse a2 b2, RpcResponse a3 b3) where
  fromListOfTokenStreams [v1,v2,v3] =  (b1, b2, b3)
    where
      !b1 = rpcResponse v1
      !b2 = rpcResponse v2
      !b3 = rpcResponse v3
  fromListOfTokenStreams _ = error "fromListOfTokenStreams: List length must be 3"

instance (RpcOutputSet a1, RpcResultSet b1, RpcOutputSet a2, RpcResultSet b2, RpcOutputSet a3, RpcResultSet b3, RpcOutputSet a4, RpcResultSet b4) => RpcResponseSet (RpcResponse a1 b1, RpcResponse a2 b2, RpcResponse a3 b3, RpcResponse a4 b4) where
  fromListOfTokenStreams [v1,v2,v3,v4] =  (b1, b2, b3, b4)
    where
      !b1 = rpcResponse v1
      !b2 = rpcResponse v2
      !b3 = rpcResponse v3
      !b4 = rpcResponse v4
  fromListOfTokenStreams _ = error "fromListOfTokenStreams: List length must be 4"

instance (RpcOutputSet a1, RpcResultSet b1, RpcOutputSet a2, RpcResultSet b2, RpcOutputSet a3, RpcResultSet b3, RpcOutputSet a4, RpcResultSet b4, RpcOutputSet a5, RpcResultSet b5) => RpcResponseSet (RpcResponse a1 b1, RpcResponse a2 b2, RpcResponse a3 b3, RpcResponse a4 b4, RpcResponse a5 b5) where
  fromListOfTokenStreams [v1,v2,v3,v4,v5] =  (b1, b2, b3, b4, b5)
    where
      !b1 = rpcResponse v1
      !b2 = rpcResponse v2
      !b3 = rpcResponse v3
      !b4 = rpcResponse v4
      !b5 = rpcResponse v5
  fromListOfTokenStreams _ = error "fromListOfTokenStreams: List length must be 5"




-- (RpcOutputSet a, RpcResultSet b) => 
data RpcResponse a b = RpcResponse Int a b
                   deriving (Show)

rpcResponse :: (RpcOutputSet a, RpcResultSet b) => [TokenStream] -> RpcResponse a b
rpcResponse tss =
  -- [TODO] performance tuning (avoid filter all)
  let [TSReturnStatus ret] = case filter isTSReturnStatus tss of
                               [] -> error "rpcResponse: TSReturnStatus is necessary"
                               xs -> xs
      rvs = (\(TSReturnValue rv) -> rv) <$> filter isTSReturnValue tss
      rss = fromTokenStreams tss

  in RpcResponse (fromIntegral ret) (fromReturnValues rvs) rss

  where
    isTSReturnStatus :: TokenStream -> Bool
    isTSReturnStatus (TSReturnStatus{}) = True
    isTSReturnStatus _ = False
    
    isTSReturnValue :: TokenStream -> Bool
    isTSReturnValue (TSReturnValue{}) = True
    isTSReturnValue _ = False



class RpcResultSet a where
  fromTokenStreams :: [TokenStream] -> a


instance RpcResultSet () where
  fromTokenStreams xs = case parse noResult xs of
                          [] -> error "fromTokenStreams(RpcResultSet ()): failed to parse"
                          (x,_):_ -> x

instance (Row a) => RpcResultSet [a] where
  fromTokenStreams xs = case parse listOfRow xs of
                          [] -> error "fromTokenStreams(RpcResultSet [Row a]): failed to parse"
                          (x,_):_ -> x


-- [TODO] use Template Haskell
instance (RpcResult a, RpcResult b) => RpcResultSet (a, b) where
  fromTokenStreams xs = case parse p xs of
                          [] -> error "fromTokenStreams(RpcResultSet (RpcResult a, RpcResult b)): failed to parse"
                          (x,_):_ -> x
    where
      p :: (RpcResult a, RpcResult b) => Parser (a, b)
      p = do
        !r1 <- resultParser :: (RpcResult a) => Parser a
        !r2 <- resultParser :: (RpcResult b) => Parser b
        return  (r1,r2)

instance (RpcResult a, RpcResult b, RpcResult c) => RpcResultSet (a, b, c) where
  fromTokenStreams xs = case parse p xs of
                          [] -> error "fromTokenStreams(RpcResultSet (RpcResult a, RpcResult b, RpcResult c)): failed to parse"
                          (x,_):_ -> x
    where
      p :: (RpcResult a, RpcResult b, RpcResult c) => Parser (a, b, c)
      p = do
        !r1 <- resultParser :: (RpcResult a) => Parser a
        !r2 <- resultParser :: (RpcResult b) => Parser b
        !r3 <- resultParser :: (RpcResult c) => Parser c
        return  (r1,r2,r3)

instance (RpcResult a, RpcResult b, RpcResult c, RpcResult d) => RpcResultSet (a, b, c, d) where
  fromTokenStreams xs = case parse p xs of
                          [] -> error "fromTokenStreams(RpcResultSet (RpcResult a, RpcResult b, RpcResult c, RpcResult d)): failed to parse"
                          (x,_):_ -> x
    where
      p :: (RpcResult a, RpcResult b, RpcResult c, RpcResult d) => Parser (a, b, c, d)
      p = do
        !r1 <- resultParser :: (RpcResult a) => Parser a
        !r2 <- resultParser :: (RpcResult b) => Parser b
        !r3 <- resultParser :: (RpcResult c) => Parser c
        !r4 <- resultParser :: (RpcResult d) => Parser d
        return  (r1,r2,r3,r4)

instance (RpcResult a, RpcResult b, RpcResult c, RpcResult d, RpcResult e) => RpcResultSet (a, b, c, d, e) where
  fromTokenStreams xs = case parse p xs of
                          [] -> error "fromTokenStreams(RpcResultSet (RpcResult a, RpcResult b, RpcResult c, RpcResult d, RpcResult e)): failed to parse"
                          (x,_):_ -> x
    where
      p :: (RpcResult a, RpcResult b, RpcResult c, RpcResult d, RpcResult e) => Parser (a, b, c, d, e)
      p = do
        !r1 <- resultParser :: (RpcResult a) => Parser a
        !r2 <- resultParser :: (RpcResult b) => Parser b
        !r3 <- resultParser :: (RpcResult c) => Parser c
        !r4 <- resultParser :: (RpcResult d) => Parser d
        !r5 <- resultParser :: (RpcResult e) => Parser e
        return  (r1,r2,r3,r4,r5)



class RpcResult a where
  resultParser :: Parser a

instance RpcResult () where
  resultParser = noResult

instance Row a => RpcResult [a] where
  resultParser = listOfRow

  



rvTypeInfo :: ReturnValue -> TypeInfo
rvTypeInfo (ReturnValue _ _ _ _ _ ti _) = ti

rvRawBytes :: ReturnValue -> RawBytes
rvRawBytes (ReturnValue _ _ _ _ _ _ rb) = rb


class RpcOutputSet a where
  fromReturnValues :: [ReturnValue] -> a

instance RpcOutputSet () where
  fromReturnValues [] = ()
  fromReturnValues _ = error "fromReturnValues: List length must be 0"
  
-- [TODO] use Template Haskell
instance (Data a) => RpcOutputSet (Only a) where
  fromReturnValues [r1] = Only d1
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
  fromReturnValues _ = error "fromReturnValues: List length must be 1"

instance (Data a, Data b) => RpcOutputSet (a,b) where
  fromReturnValues [r1,r2] = (d1,d2)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
  fromReturnValues _ = error "fromReturnValues: List length must be 2"

instance (Data a, Data b, Data c) => RpcOutputSet (a,b,c) where
  fromReturnValues [r1,r2,r3] = (d1,d2,d3)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
  fromReturnValues _ = error "fromReturnValues: List length must be 3"

instance (Data a, Data b, Data c, Data d) => RpcOutputSet (a,b,c,d) where
  fromReturnValues [r1,r2,r3,r4] = (d1,d2,d3,d4)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
  fromReturnValues _ = error "fromReturnValues: List length must be 4"

instance (Data a, Data b, Data c, Data d, Data e) => RpcOutputSet (a,b,c,d,e) where
  fromReturnValues [r1,r2,r3,r4,r5] = (d1,d2,d3,d4,d5)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
  fromReturnValues _ = error "fromReturnValues: List length must be 5"

instance (Data a, Data b, Data c, Data d, Data e, Data f) => RpcOutputSet (a,b,c,d,e,f) where
  fromReturnValues [r1,r2,r3,r4,r5,r6] = (d1,d2,d3,d4,d5,d6)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
  fromReturnValues _ = error "fromReturnValues: List length must be 6"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g) => RpcOutputSet (a,b,c,d,e,f,g) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7] = (d1,d2,d3,d4,d5,d6,d7)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
  fromReturnValues _ = error "fromReturnValues: List length must be 7"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h) => RpcOutputSet (a,b,c,d,e,f,g,h) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7,r8] = (d1,d2,d3,d4,d5,d6,d7,d8)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
      !d8 = fromRawBytes (rvTypeInfo r8) (rvRawBytes r8)
  fromReturnValues _ = error "fromReturnValues: List length must be 8"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i) => RpcOutputSet (a,b,c,d,e,f,g,h,i) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7,r8,r9] = (d1,d2,d3,d4,d5,d6,d7,d8,d9)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
      !d8 = fromRawBytes (rvTypeInfo r8) (rvRawBytes r8)
      !d9 = fromRawBytes (rvTypeInfo r9) (rvRawBytes r9)
  fromReturnValues _ = error "fromReturnValues: List length must be 9"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j) => RpcOutputSet (a,b,c,d,e,f,g,h,i,j) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10] = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
      !d8 = fromRawBytes (rvTypeInfo r8) (rvRawBytes r8)
      !d9 = fromRawBytes (rvTypeInfo r9) (rvRawBytes r9)
      !d10 = fromRawBytes (rvTypeInfo r10) (rvRawBytes r10)
  fromReturnValues _ = error "fromReturnValues: List length must be 10"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k) => RpcOutputSet (a,b,c,d,e,f,g,h,i,j,k) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11] = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
      !d8 = fromRawBytes (rvTypeInfo r8) (rvRawBytes r8)
      !d9 = fromRawBytes (rvTypeInfo r9) (rvRawBytes r9)
      !d10 = fromRawBytes (rvTypeInfo r10) (rvRawBytes r10)
      !d11 = fromRawBytes (rvTypeInfo r11) (rvRawBytes r11)
  fromReturnValues _ = error "fromReturnValues: List length must be 11"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l) => RpcOutputSet (a,b,c,d,e,f,g,h,i,j,k,l) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12] = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
      !d8 = fromRawBytes (rvTypeInfo r8) (rvRawBytes r8)
      !d9 = fromRawBytes (rvTypeInfo r9) (rvRawBytes r9)
      !d10 = fromRawBytes (rvTypeInfo r10) (rvRawBytes r10)
      !d11 = fromRawBytes (rvTypeInfo r11) (rvRawBytes r11)
      !d12 = fromRawBytes (rvTypeInfo r12) (rvRawBytes r12)
  fromReturnValues _ = error "fromReturnValues: List length must be 12"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m) => RpcOutputSet (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13] = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
      !d8 = fromRawBytes (rvTypeInfo r8) (rvRawBytes r8)
      !d9 = fromRawBytes (rvTypeInfo r9) (rvRawBytes r9)
      !d10 = fromRawBytes (rvTypeInfo r10) (rvRawBytes r10)
      !d11 = fromRawBytes (rvTypeInfo r11) (rvRawBytes r11)
      !d12 = fromRawBytes (rvTypeInfo r12) (rvRawBytes r12)
      !d13 = fromRawBytes (rvTypeInfo r13) (rvRawBytes r13)
  fromReturnValues _ = error "fromReturnValues: List length must be 13"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n) => RpcOutputSet (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14] = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
      !d8 = fromRawBytes (rvTypeInfo r8) (rvRawBytes r8)
      !d9 = fromRawBytes (rvTypeInfo r9) (rvRawBytes r9)
      !d10 = fromRawBytes (rvTypeInfo r10) (rvRawBytes r10)
      !d11 = fromRawBytes (rvTypeInfo r11) (rvRawBytes r11)
      !d12 = fromRawBytes (rvTypeInfo r12) (rvRawBytes r12)
      !d13 = fromRawBytes (rvTypeInfo r13) (rvRawBytes r13)
      !d14 = fromRawBytes (rvTypeInfo r14) (rvRawBytes r14)
  fromReturnValues _ = error "fromReturnValues: List length must be 14"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o) => RpcOutputSet (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15] = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
      !d8 = fromRawBytes (rvTypeInfo r8) (rvRawBytes r8)
      !d9 = fromRawBytes (rvTypeInfo r9) (rvRawBytes r9)
      !d10 = fromRawBytes (rvTypeInfo r10) (rvRawBytes r10)
      !d11 = fromRawBytes (rvTypeInfo r11) (rvRawBytes r11)
      !d12 = fromRawBytes (rvTypeInfo r12) (rvRawBytes r12)
      !d13 = fromRawBytes (rvTypeInfo r13) (rvRawBytes r13)
      !d14 = fromRawBytes (rvTypeInfo r14) (rvRawBytes r14)
      !d15 = fromRawBytes (rvTypeInfo r15) (rvRawBytes r15)
  fromReturnValues _ = error "fromReturnValues: List length must be 15"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p) => RpcOutputSet (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
      !d8 = fromRawBytes (rvTypeInfo r8) (rvRawBytes r8)
      !d9 = fromRawBytes (rvTypeInfo r9) (rvRawBytes r9)
      !d10 = fromRawBytes (rvTypeInfo r10) (rvRawBytes r10)
      !d11 = fromRawBytes (rvTypeInfo r11) (rvRawBytes r11)
      !d12 = fromRawBytes (rvTypeInfo r12) (rvRawBytes r12)
      !d13 = fromRawBytes (rvTypeInfo r13) (rvRawBytes r13)
      !d14 = fromRawBytes (rvTypeInfo r14) (rvRawBytes r14)
      !d15 = fromRawBytes (rvTypeInfo r15) (rvRawBytes r15)
      !d16 = fromRawBytes (rvTypeInfo r16) (rvRawBytes r16)
  fromReturnValues _ = error "fromReturnValues: List length must be 16"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q) => RpcOutputSet (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
      !d8 = fromRawBytes (rvTypeInfo r8) (rvRawBytes r8)
      !d9 = fromRawBytes (rvTypeInfo r9) (rvRawBytes r9)
      !d10 = fromRawBytes (rvTypeInfo r10) (rvRawBytes r10)
      !d11 = fromRawBytes (rvTypeInfo r11) (rvRawBytes r11)
      !d12 = fromRawBytes (rvTypeInfo r12) (rvRawBytes r12)
      !d13 = fromRawBytes (rvTypeInfo r13) (rvRawBytes r13)
      !d14 = fromRawBytes (rvTypeInfo r14) (rvRawBytes r14)
      !d15 = fromRawBytes (rvTypeInfo r15) (rvRawBytes r15)
      !d16 = fromRawBytes (rvTypeInfo r16) (rvRawBytes r16)
      !d17 = fromRawBytes (rvTypeInfo r17) (rvRawBytes r17)
  fromReturnValues _ = error "fromReturnValues: List length must be 17"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q, Data r) => RpcOutputSet (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
      !d8 = fromRawBytes (rvTypeInfo r8) (rvRawBytes r8)
      !d9 = fromRawBytes (rvTypeInfo r9) (rvRawBytes r9)
      !d10 = fromRawBytes (rvTypeInfo r10) (rvRawBytes r10)
      !d11 = fromRawBytes (rvTypeInfo r11) (rvRawBytes r11)
      !d12 = fromRawBytes (rvTypeInfo r12) (rvRawBytes r12)
      !d13 = fromRawBytes (rvTypeInfo r13) (rvRawBytes r13)
      !d14 = fromRawBytes (rvTypeInfo r14) (rvRawBytes r14)
      !d15 = fromRawBytes (rvTypeInfo r15) (rvRawBytes r15)
      !d16 = fromRawBytes (rvTypeInfo r16) (rvRawBytes r16)
      !d17 = fromRawBytes (rvTypeInfo r17) (rvRawBytes r17)
      !d18 = fromRawBytes (rvTypeInfo r18) (rvRawBytes r18)
  fromReturnValues _ = error "fromReturnValues: List length must be 18"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q, Data r, Data s) => RpcOutputSet (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
      !d8 = fromRawBytes (rvTypeInfo r8) (rvRawBytes r8)
      !d9 = fromRawBytes (rvTypeInfo r9) (rvRawBytes r9)
      !d10 = fromRawBytes (rvTypeInfo r10) (rvRawBytes r10)
      !d11 = fromRawBytes (rvTypeInfo r11) (rvRawBytes r11)
      !d12 = fromRawBytes (rvTypeInfo r12) (rvRawBytes r12)
      !d13 = fromRawBytes (rvTypeInfo r13) (rvRawBytes r13)
      !d14 = fromRawBytes (rvTypeInfo r14) (rvRawBytes r14)
      !d15 = fromRawBytes (rvTypeInfo r15) (rvRawBytes r15)
      !d16 = fromRawBytes (rvTypeInfo r16) (rvRawBytes r16)
      !d17 = fromRawBytes (rvTypeInfo r17) (rvRawBytes r17)
      !d18 = fromRawBytes (rvTypeInfo r18) (rvRawBytes r18)
      !d19 = fromRawBytes (rvTypeInfo r19) (rvRawBytes r19)
  fromReturnValues _ = error "fromReturnValues: List length must be 19"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q, Data r, Data s, Data t) => RpcOutputSet (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
      !d8 = fromRawBytes (rvTypeInfo r8) (rvRawBytes r8)
      !d9 = fromRawBytes (rvTypeInfo r9) (rvRawBytes r9)
      !d10 = fromRawBytes (rvTypeInfo r10) (rvRawBytes r10)
      !d11 = fromRawBytes (rvTypeInfo r11) (rvRawBytes r11)
      !d12 = fromRawBytes (rvTypeInfo r12) (rvRawBytes r12)
      !d13 = fromRawBytes (rvTypeInfo r13) (rvRawBytes r13)
      !d14 = fromRawBytes (rvTypeInfo r14) (rvRawBytes r14)
      !d15 = fromRawBytes (rvTypeInfo r15) (rvRawBytes r15)
      !d16 = fromRawBytes (rvTypeInfo r16) (rvRawBytes r16)
      !d17 = fromRawBytes (rvTypeInfo r17) (rvRawBytes r17)
      !d18 = fromRawBytes (rvTypeInfo r18) (rvRawBytes r18)
      !d19 = fromRawBytes (rvTypeInfo r19) (rvRawBytes r19)
      !d20 = fromRawBytes (rvTypeInfo r20) (rvRawBytes r20)
  fromReturnValues _ = error "fromReturnValues: List length must be 20"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q, Data r, Data s, Data t, Data u) => RpcOutputSet (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r21]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
      !d8 = fromRawBytes (rvTypeInfo r8) (rvRawBytes r8)
      !d9 = fromRawBytes (rvTypeInfo r9) (rvRawBytes r9)
      !d10 = fromRawBytes (rvTypeInfo r10) (rvRawBytes r10)
      !d11 = fromRawBytes (rvTypeInfo r11) (rvRawBytes r11)
      !d12 = fromRawBytes (rvTypeInfo r12) (rvRawBytes r12)
      !d13 = fromRawBytes (rvTypeInfo r13) (rvRawBytes r13)
      !d14 = fromRawBytes (rvTypeInfo r14) (rvRawBytes r14)
      !d15 = fromRawBytes (rvTypeInfo r15) (rvRawBytes r15)
      !d16 = fromRawBytes (rvTypeInfo r16) (rvRawBytes r16)
      !d17 = fromRawBytes (rvTypeInfo r17) (rvRawBytes r17)
      !d18 = fromRawBytes (rvTypeInfo r18) (rvRawBytes r18)
      !d19 = fromRawBytes (rvTypeInfo r19) (rvRawBytes r19)
      !d20 = fromRawBytes (rvTypeInfo r20) (rvRawBytes r20)
      !d21 = fromRawBytes (rvTypeInfo r21) (rvRawBytes r21)
  fromReturnValues _ = error "fromReturnValues: List length must be 21"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q, Data r, Data s, Data t, Data u, Data v) => RpcOutputSet (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r21,r22]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
      !d8 = fromRawBytes (rvTypeInfo r8) (rvRawBytes r8)
      !d9 = fromRawBytes (rvTypeInfo r9) (rvRawBytes r9)
      !d10 = fromRawBytes (rvTypeInfo r10) (rvRawBytes r10)
      !d11 = fromRawBytes (rvTypeInfo r11) (rvRawBytes r11)
      !d12 = fromRawBytes (rvTypeInfo r12) (rvRawBytes r12)
      !d13 = fromRawBytes (rvTypeInfo r13) (rvRawBytes r13)
      !d14 = fromRawBytes (rvTypeInfo r14) (rvRawBytes r14)
      !d15 = fromRawBytes (rvTypeInfo r15) (rvRawBytes r15)
      !d16 = fromRawBytes (rvTypeInfo r16) (rvRawBytes r16)
      !d17 = fromRawBytes (rvTypeInfo r17) (rvRawBytes r17)
      !d18 = fromRawBytes (rvTypeInfo r18) (rvRawBytes r18)
      !d19 = fromRawBytes (rvTypeInfo r19) (rvRawBytes r19)
      !d20 = fromRawBytes (rvTypeInfo r20) (rvRawBytes r20)
      !d21 = fromRawBytes (rvTypeInfo r21) (rvRawBytes r21)
      !d22 = fromRawBytes (rvTypeInfo r22) (rvRawBytes r22)
  fromReturnValues _ = error "fromReturnValues: List length must be 22"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q, Data r, Data s, Data t, Data u, Data v, Data w) => RpcOutputSet (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r21,r22,r23]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
      !d8 = fromRawBytes (rvTypeInfo r8) (rvRawBytes r8)
      !d9 = fromRawBytes (rvTypeInfo r9) (rvRawBytes r9)
      !d10 = fromRawBytes (rvTypeInfo r10) (rvRawBytes r10)
      !d11 = fromRawBytes (rvTypeInfo r11) (rvRawBytes r11)
      !d12 = fromRawBytes (rvTypeInfo r12) (rvRawBytes r12)
      !d13 = fromRawBytes (rvTypeInfo r13) (rvRawBytes r13)
      !d14 = fromRawBytes (rvTypeInfo r14) (rvRawBytes r14)
      !d15 = fromRawBytes (rvTypeInfo r15) (rvRawBytes r15)
      !d16 = fromRawBytes (rvTypeInfo r16) (rvRawBytes r16)
      !d17 = fromRawBytes (rvTypeInfo r17) (rvRawBytes r17)
      !d18 = fromRawBytes (rvTypeInfo r18) (rvRawBytes r18)
      !d19 = fromRawBytes (rvTypeInfo r19) (rvRawBytes r19)
      !d20 = fromRawBytes (rvTypeInfo r20) (rvRawBytes r20)
      !d21 = fromRawBytes (rvTypeInfo r21) (rvRawBytes r21)
      !d22 = fromRawBytes (rvTypeInfo r22) (rvRawBytes r22)
      !d23 = fromRawBytes (rvTypeInfo r23) (rvRawBytes r23)
  fromReturnValues _ = error "fromReturnValues: List length must be 23"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q, Data r, Data s, Data t, Data u, Data v, Data w, Data x) => RpcOutputSet (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r21,r22,r23,r24]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23,d24)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
      !d8 = fromRawBytes (rvTypeInfo r8) (rvRawBytes r8)
      !d9 = fromRawBytes (rvTypeInfo r9) (rvRawBytes r9)
      !d10 = fromRawBytes (rvTypeInfo r10) (rvRawBytes r10)
      !d11 = fromRawBytes (rvTypeInfo r11) (rvRawBytes r11)
      !d12 = fromRawBytes (rvTypeInfo r12) (rvRawBytes r12)
      !d13 = fromRawBytes (rvTypeInfo r13) (rvRawBytes r13)
      !d14 = fromRawBytes (rvTypeInfo r14) (rvRawBytes r14)
      !d15 = fromRawBytes (rvTypeInfo r15) (rvRawBytes r15)
      !d16 = fromRawBytes (rvTypeInfo r16) (rvRawBytes r16)
      !d17 = fromRawBytes (rvTypeInfo r17) (rvRawBytes r17)
      !d18 = fromRawBytes (rvTypeInfo r18) (rvRawBytes r18)
      !d19 = fromRawBytes (rvTypeInfo r19) (rvRawBytes r19)
      !d20 = fromRawBytes (rvTypeInfo r20) (rvRawBytes r20)
      !d21 = fromRawBytes (rvTypeInfo r21) (rvRawBytes r21)
      !d22 = fromRawBytes (rvTypeInfo r22) (rvRawBytes r22)
      !d23 = fromRawBytes (rvTypeInfo r23) (rvRawBytes r23)
      !d24 = fromRawBytes (rvTypeInfo r24) (rvRawBytes r24)
  fromReturnValues _ = error "fromReturnValues: List length must be 24"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q, Data r, Data s, Data t, Data u, Data v, Data w, Data x, Data y) => RpcOutputSet (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r21,r22,r23,r24,r25]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23,d24,d25)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
      !d8 = fromRawBytes (rvTypeInfo r8) (rvRawBytes r8)
      !d9 = fromRawBytes (rvTypeInfo r9) (rvRawBytes r9)
      !d10 = fromRawBytes (rvTypeInfo r10) (rvRawBytes r10)
      !d11 = fromRawBytes (rvTypeInfo r11) (rvRawBytes r11)
      !d12 = fromRawBytes (rvTypeInfo r12) (rvRawBytes r12)
      !d13 = fromRawBytes (rvTypeInfo r13) (rvRawBytes r13)
      !d14 = fromRawBytes (rvTypeInfo r14) (rvRawBytes r14)
      !d15 = fromRawBytes (rvTypeInfo r15) (rvRawBytes r15)
      !d16 = fromRawBytes (rvTypeInfo r16) (rvRawBytes r16)
      !d17 = fromRawBytes (rvTypeInfo r17) (rvRawBytes r17)
      !d18 = fromRawBytes (rvTypeInfo r18) (rvRawBytes r18)
      !d19 = fromRawBytes (rvTypeInfo r19) (rvRawBytes r19)
      !d20 = fromRawBytes (rvTypeInfo r20) (rvRawBytes r20)
      !d21 = fromRawBytes (rvTypeInfo r21) (rvRawBytes r21)
      !d22 = fromRawBytes (rvTypeInfo r22) (rvRawBytes r22)
      !d23 = fromRawBytes (rvTypeInfo r23) (rvRawBytes r23)
      !d24 = fromRawBytes (rvTypeInfo r24) (rvRawBytes r24)
      !d25 = fromRawBytes (rvTypeInfo r25) (rvRawBytes r25)
  fromReturnValues _ = error "fromReturnValues: List length must be 25"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q, Data r, Data s, Data t, Data u, Data v, Data w, Data x, Data y, Data z) => RpcOutputSet (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) where
  fromReturnValues [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r21,r22,r23,r24,r25,r26]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23,d24,d25,d26)
    where
      !d1 = fromRawBytes (rvTypeInfo r1) (rvRawBytes r1)
      !d2 = fromRawBytes (rvTypeInfo r2) (rvRawBytes r2)
      !d3 = fromRawBytes (rvTypeInfo r3) (rvRawBytes r3)
      !d4 = fromRawBytes (rvTypeInfo r4) (rvRawBytes r4)
      !d5 = fromRawBytes (rvTypeInfo r5) (rvRawBytes r5)
      !d6 = fromRawBytes (rvTypeInfo r6) (rvRawBytes r6)
      !d7 = fromRawBytes (rvTypeInfo r7) (rvRawBytes r7)
      !d8 = fromRawBytes (rvTypeInfo r8) (rvRawBytes r8)
      !d9 = fromRawBytes (rvTypeInfo r9) (rvRawBytes r9)
      !d10 = fromRawBytes (rvTypeInfo r10) (rvRawBytes r10)
      !d11 = fromRawBytes (rvTypeInfo r11) (rvRawBytes r11)
      !d12 = fromRawBytes (rvTypeInfo r12) (rvRawBytes r12)
      !d13 = fromRawBytes (rvTypeInfo r13) (rvRawBytes r13)
      !d14 = fromRawBytes (rvTypeInfo r14) (rvRawBytes r14)
      !d15 = fromRawBytes (rvTypeInfo r15) (rvRawBytes r15)
      !d16 = fromRawBytes (rvTypeInfo r16) (rvRawBytes r16)
      !d17 = fromRawBytes (rvTypeInfo r17) (rvRawBytes r17)
      !d18 = fromRawBytes (rvTypeInfo r18) (rvRawBytes r18)
      !d19 = fromRawBytes (rvTypeInfo r19) (rvRawBytes r19)
      !d20 = fromRawBytes (rvTypeInfo r20) (rvRawBytes r20)
      !d21 = fromRawBytes (rvTypeInfo r21) (rvRawBytes r21)
      !d22 = fromRawBytes (rvTypeInfo r22) (rvRawBytes r22)
      !d23 = fromRawBytes (rvTypeInfo r23) (rvRawBytes r23)
      !d24 = fromRawBytes (rvTypeInfo r24) (rvRawBytes r24)
      !d25 = fromRawBytes (rvTypeInfo r25) (rvRawBytes r25)
      !d26 = fromRawBytes (rvTypeInfo r26) (rvRawBytes r26)
  fromReturnValues _ = error "fromReturnValues: List length must be 26"

