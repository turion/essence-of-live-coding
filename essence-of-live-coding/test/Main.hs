{-# LANGUAGE DeriveDataTypeable #-}

-- test-framework
import Test.Framework

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2

-- essence-of-live-coding
import LiveCoding
import qualified TestData.Foo1 as Foo1
import qualified TestData.Foo2 as Foo2

intToInteger :: Int -> Integer
intToInteger = toInteger

main = defaultMain tests

tests = return $ testGroup "simple tests"
  [ testProperty "" $ 13 == migrate (10 :: Integer) (13 :: Integer)
  , testProperty "" $ Foo1.foo' == migrate Foo1.foo Foo2.foo
  , testProperty "" $ Foo2.foo' == migrate Foo2.foo Foo1.foo
  , testProperty "" $ Foo2.bar' == migrate Foo2.bar Foo1.bar
  , testProperty "" $ Foo2.baz' == migrate Foo2.baz Foo1.baz
  , testProperty "" $ Foo2.frob' == migrateWith (userMigration intToInteger) Foo2.frob Foo1.frob
  , testProperty "" $ Foo2.Frob 23 == migrate Foo2.frob (23 :: Integer)
  , testProperty "" $ Debugging { dbgState = (42 :: Integer), state = (23 :: Integer) } == migrate Debugging { dbgState = (42 :: Integer), state = (42 :: Integer) } (23 :: Integer)
  , testProperty "" $ (23 :: Integer) == migrate (5 :: Integer) Debugging { dbgState = (42 :: Integer), state = (23 :: Integer) }
  ]
