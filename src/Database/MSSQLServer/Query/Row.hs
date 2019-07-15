{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}

module Database.MSSQLServer.Query.Row ( Row (..)
                                      ) where

import Database.Tds.Message
import Database.MSSQLServer.Query.Only


mcdTypeInfo :: MetaColumnData -> TypeInfo
mcdTypeInfo (MetaColumnData _ _ ti _ _) = ti


class Row a where
  fromListOfRawBytes :: [MetaColumnData] -> [RawBytes] -> a

-- [TODO] use Template Haskell
instance Row () where
  fromListOfRawBytes [] [] = ()
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 0"

instance (Data a) => Row (Only a) where
  fromListOfRawBytes [m1] [b1] = Only d1
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 1"

instance (Data a, Data b) => Row (a,b) where
  fromListOfRawBytes [m1,m2] [b1,b2] = (d1,d2)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 2"

instance (Data a, Data b, Data c) => Row (a,b,c) where
  fromListOfRawBytes [m1,m2,m3] [b1,b2,b3] = (d1,d2,d3)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 3"

instance (Data a, Data b, Data c, Data d) => Row (a,b,c,d) where
  fromListOfRawBytes [m1,m2,m3,m4] [b1,b2,b3,b4] = (d1,d2,d3,d4)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 4"

instance (Data a, Data b, Data c, Data d, Data e) => Row (a,b,c,d,e) where
  fromListOfRawBytes [m1,m2,m3,m4,m5] [b1,b2,b3,b4,b5] = (d1,d2,d3,d4,d5)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 5"

instance (Data a, Data b, Data c, Data d, Data e, Data f) => Row (a,b,c,d,e,f) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6] [b1,b2,b3,b4,b5,b6] = (d1,d2,d3,d4,d5,d6)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 6"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g) => Row (a,b,c,d,e,f,g) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7] [b1,b2,b3,b4,b5,b6,b7] = (d1,d2,d3,d4,d5,d6,d7)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 7"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h) => Row (a,b,c,d,e,f,g,h) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7,m8] [b1,b2,b3,b4,b5,b6,b7,b8] = (d1,d2,d3,d4,d5,d6,d7,d8)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      !d8 = fromRawBytes (mcdTypeInfo m8) b8
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 8"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i) => Row (a,b,c,d,e,f,g,h,i) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7,m8,m9] [b1,b2,b3,b4,b5,b6,b7,b8,b9] = (d1,d2,d3,d4,d5,d6,d7,d8,d9)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      !d8 = fromRawBytes (mcdTypeInfo m8) b8
      !d9 = fromRawBytes (mcdTypeInfo m9) b9
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 9"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j) => Row (a,b,c,d,e,f,g,h,i,j) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10] [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10] = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      !d8 = fromRawBytes (mcdTypeInfo m8) b8
      !d9 = fromRawBytes (mcdTypeInfo m9) b9
      !d10 = fromRawBytes (mcdTypeInfo m10) b10
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 10"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k) => Row (a,b,c,d,e,f,g,h,i,j,k) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11] [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      !d8 = fromRawBytes (mcdTypeInfo m8) b8
      !d9 = fromRawBytes (mcdTypeInfo m9) b9
      !d10 = fromRawBytes (mcdTypeInfo m10) b10
      !d11 = fromRawBytes (mcdTypeInfo m11) b11
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 11"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l) => Row (a,b,c,d,e,f,g,h,i,j,k,l) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12] [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      !d8 = fromRawBytes (mcdTypeInfo m8) b8
      !d9 = fromRawBytes (mcdTypeInfo m9) b9
      !d10 = fromRawBytes (mcdTypeInfo m10) b10
      !d11 = fromRawBytes (mcdTypeInfo m11) b11
      !d12 = fromRawBytes (mcdTypeInfo m12) b12
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 12"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m) => Row (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13] [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      !d8 = fromRawBytes (mcdTypeInfo m8) b8
      !d9 = fromRawBytes (mcdTypeInfo m9) b9
      !d10 = fromRawBytes (mcdTypeInfo m10) b10
      !d11 = fromRawBytes (mcdTypeInfo m11) b11
      !d12 = fromRawBytes (mcdTypeInfo m12) b12
      !d13 = fromRawBytes (mcdTypeInfo m13) b13
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 13"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n) => Row (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14] [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      !d8 = fromRawBytes (mcdTypeInfo m8) b8
      !d9 = fromRawBytes (mcdTypeInfo m9) b9
      !d10 = fromRawBytes (mcdTypeInfo m10) b10
      !d11 = fromRawBytes (mcdTypeInfo m11) b11
      !d12 = fromRawBytes (mcdTypeInfo m12) b12
      !d13 = fromRawBytes (mcdTypeInfo m13) b13
      !d14 = fromRawBytes (mcdTypeInfo m14) b14
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 14"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o) =>
         Row (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15] [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      !d8 = fromRawBytes (mcdTypeInfo m8) b8
      !d9 = fromRawBytes (mcdTypeInfo m9) b9
      !d10 = fromRawBytes (mcdTypeInfo m10) b10
      !d11 = fromRawBytes (mcdTypeInfo m11) b11
      !d12 = fromRawBytes (mcdTypeInfo m12) b12
      !d13 = fromRawBytes (mcdTypeInfo m13) b13
      !d14 = fromRawBytes (mcdTypeInfo m14) b14
      !d15 = fromRawBytes (mcdTypeInfo m15) b15
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 15"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p) =>
         Row (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16] [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      !d8 = fromRawBytes (mcdTypeInfo m8) b8
      !d9 = fromRawBytes (mcdTypeInfo m9) b9
      !d10 = fromRawBytes (mcdTypeInfo m10) b10
      !d11 = fromRawBytes (mcdTypeInfo m11) b11
      !d12 = fromRawBytes (mcdTypeInfo m12) b12
      !d13 = fromRawBytes (mcdTypeInfo m13) b13
      !d14 = fromRawBytes (mcdTypeInfo m14) b14
      !d15 = fromRawBytes (mcdTypeInfo m15) b15
      !d16 = fromRawBytes (mcdTypeInfo m16) b16
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 16"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q) =>
         Row (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17] [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      !d8 = fromRawBytes (mcdTypeInfo m8) b8
      !d9 = fromRawBytes (mcdTypeInfo m9) b9
      !d10 = fromRawBytes (mcdTypeInfo m10) b10
      !d11 = fromRawBytes (mcdTypeInfo m11) b11
      !d12 = fromRawBytes (mcdTypeInfo m12) b12
      !d13 = fromRawBytes (mcdTypeInfo m13) b13
      !d14 = fromRawBytes (mcdTypeInfo m14) b14
      !d15 = fromRawBytes (mcdTypeInfo m15) b15
      !d16 = fromRawBytes (mcdTypeInfo m16) b16
      !d17 = fromRawBytes (mcdTypeInfo m17) b17
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 17"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q, Data r) =>
         Row (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18] [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      !d8 = fromRawBytes (mcdTypeInfo m8) b8
      !d9 = fromRawBytes (mcdTypeInfo m9) b9
      !d10 = fromRawBytes (mcdTypeInfo m10) b10
      !d11 = fromRawBytes (mcdTypeInfo m11) b11
      !d12 = fromRawBytes (mcdTypeInfo m12) b12
      !d13 = fromRawBytes (mcdTypeInfo m13) b13
      !d14 = fromRawBytes (mcdTypeInfo m14) b14
      !d15 = fromRawBytes (mcdTypeInfo m15) b15
      !d16 = fromRawBytes (mcdTypeInfo m16) b16
      !d17 = fromRawBytes (mcdTypeInfo m17) b17
      !d18 = fromRawBytes (mcdTypeInfo m18) b18
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 18"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q, Data r, Data s) =>
         Row (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19] [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      !d8 = fromRawBytes (mcdTypeInfo m8) b8
      !d9 = fromRawBytes (mcdTypeInfo m9) b9
      !d10 = fromRawBytes (mcdTypeInfo m10) b10
      !d11 = fromRawBytes (mcdTypeInfo m11) b11
      !d12 = fromRawBytes (mcdTypeInfo m12) b12
      !d13 = fromRawBytes (mcdTypeInfo m13) b13
      !d14 = fromRawBytes (mcdTypeInfo m14) b14
      !d15 = fromRawBytes (mcdTypeInfo m15) b15
      !d16 = fromRawBytes (mcdTypeInfo m16) b16
      !d17 = fromRawBytes (mcdTypeInfo m17) b17
      !d18 = fromRawBytes (mcdTypeInfo m18) b18
      !d19 = fromRawBytes (mcdTypeInfo m19) b19
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 19"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q, Data r, Data s
         , Data t) =>
         Row (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20] [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      !d8 = fromRawBytes (mcdTypeInfo m8) b8
      !d9 = fromRawBytes (mcdTypeInfo m9) b9
      !d10 = fromRawBytes (mcdTypeInfo m10) b10
      !d11 = fromRawBytes (mcdTypeInfo m11) b11
      !d12 = fromRawBytes (mcdTypeInfo m12) b12
      !d13 = fromRawBytes (mcdTypeInfo m13) b13
      !d14 = fromRawBytes (mcdTypeInfo m14) b14
      !d15 = fromRawBytes (mcdTypeInfo m15) b15
      !d16 = fromRawBytes (mcdTypeInfo m16) b16
      !d17 = fromRawBytes (mcdTypeInfo m17) b17
      !d18 = fromRawBytes (mcdTypeInfo m18) b18
      !d19 = fromRawBytes (mcdTypeInfo m19) b19
      !d20 = fromRawBytes (mcdTypeInfo m20) b20
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 20"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q, Data r, Data s
         , Data t, Data u) =>
         Row (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21] [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      !d8 = fromRawBytes (mcdTypeInfo m8) b8
      !d9 = fromRawBytes (mcdTypeInfo m9) b9
      !d10 = fromRawBytes (mcdTypeInfo m10) b10
      !d11 = fromRawBytes (mcdTypeInfo m11) b11
      !d12 = fromRawBytes (mcdTypeInfo m12) b12
      !d13 = fromRawBytes (mcdTypeInfo m13) b13
      !d14 = fromRawBytes (mcdTypeInfo m14) b14
      !d15 = fromRawBytes (mcdTypeInfo m15) b15
      !d16 = fromRawBytes (mcdTypeInfo m16) b16
      !d17 = fromRawBytes (mcdTypeInfo m17) b17
      !d18 = fromRawBytes (mcdTypeInfo m18) b18
      !d19 = fromRawBytes (mcdTypeInfo m19) b19
      !d20 = fromRawBytes (mcdTypeInfo m20) b20
      !d21 = fromRawBytes (mcdTypeInfo m21) b21
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 21"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q, Data r, Data s
         , Data t, Data u, Data v) =>
         Row (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22] [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      !d8 = fromRawBytes (mcdTypeInfo m8) b8
      !d9 = fromRawBytes (mcdTypeInfo m9) b9
      !d10 = fromRawBytes (mcdTypeInfo m10) b10
      !d11 = fromRawBytes (mcdTypeInfo m11) b11
      !d12 = fromRawBytes (mcdTypeInfo m12) b12
      !d13 = fromRawBytes (mcdTypeInfo m13) b13
      !d14 = fromRawBytes (mcdTypeInfo m14) b14
      !d15 = fromRawBytes (mcdTypeInfo m15) b15
      !d16 = fromRawBytes (mcdTypeInfo m16) b16
      !d17 = fromRawBytes (mcdTypeInfo m17) b17
      !d18 = fromRawBytes (mcdTypeInfo m18) b18
      !d19 = fromRawBytes (mcdTypeInfo m19) b19
      !d20 = fromRawBytes (mcdTypeInfo m20) b20
      !d21 = fromRawBytes (mcdTypeInfo m21) b21
      !d22 = fromRawBytes (mcdTypeInfo m22) b22
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 22"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q, Data r, Data s
         , Data t, Data u, Data v, Data w) =>
         Row (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23] [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22,b23]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      !d8 = fromRawBytes (mcdTypeInfo m8) b8
      !d9 = fromRawBytes (mcdTypeInfo m9) b9
      !d10 = fromRawBytes (mcdTypeInfo m10) b10
      !d11 = fromRawBytes (mcdTypeInfo m11) b11
      !d12 = fromRawBytes (mcdTypeInfo m12) b12
      !d13 = fromRawBytes (mcdTypeInfo m13) b13
      !d14 = fromRawBytes (mcdTypeInfo m14) b14
      !d15 = fromRawBytes (mcdTypeInfo m15) b15
      !d16 = fromRawBytes (mcdTypeInfo m16) b16
      !d17 = fromRawBytes (mcdTypeInfo m17) b17
      !d18 = fromRawBytes (mcdTypeInfo m18) b18
      !d19 = fromRawBytes (mcdTypeInfo m19) b19
      !d20 = fromRawBytes (mcdTypeInfo m20) b20
      !d21 = fromRawBytes (mcdTypeInfo m21) b21
      !d22 = fromRawBytes (mcdTypeInfo m22) b22
      !d23 = fromRawBytes (mcdTypeInfo m23) b23
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 23"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q, Data r, Data s
         , Data t, Data u, Data v, Data w, Data x) =>
         Row (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24] [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22,b23,b24]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23,d24)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      !d8 = fromRawBytes (mcdTypeInfo m8) b8
      !d9 = fromRawBytes (mcdTypeInfo m9) b9
      !d10 = fromRawBytes (mcdTypeInfo m10) b10
      !d11 = fromRawBytes (mcdTypeInfo m11) b11
      !d12 = fromRawBytes (mcdTypeInfo m12) b12
      !d13 = fromRawBytes (mcdTypeInfo m13) b13
      !d14 = fromRawBytes (mcdTypeInfo m14) b14
      !d15 = fromRawBytes (mcdTypeInfo m15) b15
      !d16 = fromRawBytes (mcdTypeInfo m16) b16
      !d17 = fromRawBytes (mcdTypeInfo m17) b17
      !d18 = fromRawBytes (mcdTypeInfo m18) b18
      !d19 = fromRawBytes (mcdTypeInfo m19) b19
      !d20 = fromRawBytes (mcdTypeInfo m20) b20
      !d21 = fromRawBytes (mcdTypeInfo m21) b21
      !d22 = fromRawBytes (mcdTypeInfo m22) b22
      !d23 = fromRawBytes (mcdTypeInfo m23) b23
      !d24 = fromRawBytes (mcdTypeInfo m24) b24
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 24"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q, Data r, Data s
         , Data t, Data u, Data v, Data w, Data x, Data y) =>
         Row (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24,m25] [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22,b23,b24,b25]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23,d24,d25)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      !d8 = fromRawBytes (mcdTypeInfo m8) b8
      !d9 = fromRawBytes (mcdTypeInfo m9) b9
      !d10 = fromRawBytes (mcdTypeInfo m10) b10
      !d11 = fromRawBytes (mcdTypeInfo m11) b11
      !d12 = fromRawBytes (mcdTypeInfo m12) b12
      !d13 = fromRawBytes (mcdTypeInfo m13) b13
      !d14 = fromRawBytes (mcdTypeInfo m14) b14
      !d15 = fromRawBytes (mcdTypeInfo m15) b15
      !d16 = fromRawBytes (mcdTypeInfo m16) b16
      !d17 = fromRawBytes (mcdTypeInfo m17) b17
      !d18 = fromRawBytes (mcdTypeInfo m18) b18
      !d19 = fromRawBytes (mcdTypeInfo m19) b19
      !d20 = fromRawBytes (mcdTypeInfo m20) b20
      !d21 = fromRawBytes (mcdTypeInfo m21) b21
      !d22 = fromRawBytes (mcdTypeInfo m22) b22
      !d23 = fromRawBytes (mcdTypeInfo m23) b23
      !d24 = fromRawBytes (mcdTypeInfo m24) b24
      !d25 = fromRawBytes (mcdTypeInfo m25) b25
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 25"

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g, Data h, Data i, Data j, Data k, Data l, Data m, Data n, Data o, Data p, Data q, Data r, Data s
         , Data t, Data u, Data v, Data w, Data x, Data y, Data z) =>
         Row (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z) where
  fromListOfRawBytes [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24,m25,m26] [b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22,b23,b24,b25,b26]
    = (d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23,d24,d25,d26)
    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2
      !d3 = fromRawBytes (mcdTypeInfo m3) b3
      !d4 = fromRawBytes (mcdTypeInfo m4) b4
      !d5 = fromRawBytes (mcdTypeInfo m5) b5
      !d6 = fromRawBytes (mcdTypeInfo m6) b6
      !d7 = fromRawBytes (mcdTypeInfo m7) b7
      !d8 = fromRawBytes (mcdTypeInfo m8) b8
      !d9 = fromRawBytes (mcdTypeInfo m9) b9
      !d10 = fromRawBytes (mcdTypeInfo m10) b10
      !d11 = fromRawBytes (mcdTypeInfo m11) b11
      !d12 = fromRawBytes (mcdTypeInfo m12) b12
      !d13 = fromRawBytes (mcdTypeInfo m13) b13
      !d14 = fromRawBytes (mcdTypeInfo m14) b14
      !d15 = fromRawBytes (mcdTypeInfo m15) b15
      !d16 = fromRawBytes (mcdTypeInfo m16) b16
      !d17 = fromRawBytes (mcdTypeInfo m17) b17
      !d18 = fromRawBytes (mcdTypeInfo m18) b18
      !d19 = fromRawBytes (mcdTypeInfo m19) b19
      !d20 = fromRawBytes (mcdTypeInfo m20) b20
      !d21 = fromRawBytes (mcdTypeInfo m21) b21
      !d22 = fromRawBytes (mcdTypeInfo m22) b22
      !d23 = fromRawBytes (mcdTypeInfo m23) b23
      !d24 = fromRawBytes (mcdTypeInfo m24) b24
      !d25 = fromRawBytes (mcdTypeInfo m25) b25
      !d26 = fromRawBytes (mcdTypeInfo m26) b26
  fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 26"


