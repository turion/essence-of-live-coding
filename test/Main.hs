{-# LANGUAGE DeriveDataTypeable #-}
-- base
import Data.Data

-- syb
import Data.Generics.Aliases
import qualified Data.Generics.Aliases as Aliases
import Data.Generics.Twins

-- essenceoflivecoding
import LiveCoding
import LiveCoding.Migrate
import qualified TestData.Foo1 as Foo1
import qualified TestData.Foo2 as Foo2

main = do
  print $ migrate (10 :: Integer) (13 :: Integer)
  --print $ migrate (Foo1.Foo 10) (Foo1.Foo 13)
  print $ setChildren (getChildrenSetters $ Foo1.Foo 1 False) $ Foo2.Bar 3 5 "Bar"
  print $ dataTypeOf $ Foo2.Foo 1
  print $ dataTypeOf $ Foo1.Foo 1 True
  print $ migrate (Foo1.Foo 1 True) $ Foo2.Foo 2

-- TODO
-- * Make test suite assertions out of this
-- * Record tests
