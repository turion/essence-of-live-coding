{-# LANGUAGE DeriveDataTypeable #-}

module TestData.Foo2 where

-- base
import Data.Data
import Data.Typeable

data Same = Same String Int
  deriving (Show, Eq, Typeable, Data)

same = Same "the same" 42

data Similar = Similar String Int
  deriving (Show, Eq, Typeable, Data)

similar = Similar "similar" 100

data Foo
  = Fooo Integer
  | Foo Integer
  deriving (Show, Eq, Typeable, Data)

foo = Foo 2
foo' = Foo 1

data Bar
  = Bar
      { barB :: Integer
      , barA :: Integer
      , barC :: String
      }
  | Baar
      { baarA :: Int
      }
  deriving (Show, Eq, Typeable, Data)

bar =
  Bar
    { barB = 42
    , barA = 100
    , barC = "Bar"
    }

bar' =
  Bar
    { barB = 42
    , barA = 23
    , barC = "Bar"
    }

data Baz = Baz
  { bazBar :: Bar
  , bazFoo :: Foo
  }
  deriving (Show, Eq, Typeable, Data)

baz = Baz bar foo
baz' = Baz bar' foo'

data Frob = Frob Integer
  deriving (Show, Eq, Typeable, Data)

frob = Frob 2
frob' = Frob 1
