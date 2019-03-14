{-# LANGUAGE DeriveDataTypeable #-}
module TestData.Foo1 where

-- base
import Data.Data
import Data.Typeable

data Foo = Foo Integer Bool
  deriving (Show, Typeable, Data)
