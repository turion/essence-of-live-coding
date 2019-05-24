{-# LANGUAGE DeriveDataTypeable #-}
-- base
import Data.Data

-- syb
import Data.Generics.Aliases
import qualified Data.Generics.Aliases as Aliases
import Data.Generics.Twins

-- essenceoflivecoding
import LiveCoding
import qualified TestData.Foo1 as Foo1
import qualified TestData.Foo2 as Foo2


intToInteger :: Int -> Integer
intToInteger = toInteger

main = do
  print $ 13 == migrate (10 :: Integer) (13 :: Integer)
  print $ Foo1.foo' == migrate Foo1.foo Foo2.foo
  print $ Foo2.foo' == migrate Foo2.foo Foo1.foo
  print $ Foo2.bar' == migrate Foo2.bar Foo1.bar
  print $ Foo2.baz' == migrate Foo2.baz Foo1.baz
  print $ Foo2.frob' == userMigrate intToInteger Foo2.frob Foo1.frob
-- TODO
-- * Make test suite assertions out of this
-- * Record tests
