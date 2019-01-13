{-# LANGUAGE DeriveDataTypeable #-}
-- base
import Data.Data

-- syb
import Data.Generics.Aliases
import qualified Data.Generics.Aliases as Aliases
import Data.Generics.Twins

-- essenceoflivecoding
import LiveCoding hiding (Foo)
import qualified LiveCoding

data Foo = Foo Integer Bool
  deriving (Show, Typeable, Data)

main = do
  print $ updateWith (10 :: Integer) (13 :: Integer)
  --print $ updateWith (Foo 10) (Foo 13)
  print $ setChildren (getChildrenSetters $ Foo 1 False) $ Bar 3 5 "Bar"
  print $ dataTypeOf $ LiveCoding.Foo 1
  print $ dataTypeOf $ Foo 1 True
  print $ updateWith (Foo 1 True) $ LiveCoding.Foo 2
