{-# OPTIONS_HADDOCK hide #-}

module Database.MSSQLServer.Query.Only ( Only (..)
                                       ) where

newtype Only a = Only {fromOnly::a}
               deriving (Show)

