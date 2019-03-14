{-# LANGUAGE DeriveDataTypeable #-}
module TestData.Foo2 where

-- base
import Data.Data
import Data.Typeable

data Foo = Foo Integer
  deriving (Show, Typeable, Data)

data Bar = Bar Integer Integer String
  deriving (Show, Typeable, Data)
