{-# LANGUAGE DeriveDataTypeable #-}

module TestData.Foo1 where

-- base
import Data.Data
import Data.Typeable

data Foo = Foo Integer Bool
  deriving (Show, Eq, Typeable, Data)

foo = Foo 1 False
foo' = Foo 2 False

data Bar
  = Bar
      { barA :: Integer
      , barD :: Integer
      , barC :: Bool
      }
  | Baar
      { baarB :: Bool
      , baarA :: Int
      }
  deriving (Show, Eq, Typeable, Data)

bar =
  Bar
    { barA = 23
    , barD = 5
    , barC = True
    }

data Baz = Baz
  { bazFoo :: Foo
  , bazBar :: Bar
  }
  deriving (Show, Eq, Typeable, Data)

baz = Baz foo bar

data Frob = Frob Int
  deriving (Show, Eq, Typeable, Data)

frob = Frob 1
