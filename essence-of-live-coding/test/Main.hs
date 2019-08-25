{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- test-framework
import Test.Framework

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2

-- QuickCheck
import Test.QuickCheck

-- essence-of-live-coding
import LiveCoding
import qualified TestData.Foo1 as Foo1
import qualified TestData.Foo2 as Foo2

intToInteger :: Int -> Integer
intToInteger = toInteger

main = defaultMain tests

tests =
  [ testGroup "Builtin types"
    [ testProperty "Same"
      $ \(x :: Integer) (y :: Integer) -> x === migrate y x
    , testProperty "Different"
      $ \(x :: Integer) (y :: Bool) -> y === migrate y x
    ]
  , testGroup "Product types"
    [ testProperty "Adds default field"
      $ Foo1.foo' === migrate Foo1.foo Foo2.foo
    , testProperty "Keeps only sensible field"
      $ Foo2.foo' === migrate Foo2.foo Foo1.foo
    ]
  , testGroup "Records"
    [ testProperty "Takes record field names into account"
      $ \barA barB barC barC2 barD
      -> Foo2.Bar { barC = barC2, .. } === migrate Foo2.Bar { barC = barC2, .. } Foo1.Bar { .. }
    , testProperty "Migrates nested records"
      $ Foo2.baz' === migrate Foo2.baz Foo1.baz
    ]
  , testGroup "Constructors"
    [ testProperty "Finds correct constructor"
      $ \x y z -> migrate (Foo2.Fooo z) (Foo1.Foo x y) === Foo2.Foo x
    ]
  , testGroup "User migration"
    [ testProperty "Can add migration from Int to Integer"
      $ Foo2.frob' === migrateWith (userMigration intToInteger) Foo2.frob Foo1.frob
    ]
  , testGroup "Newtypes"
    [ testProperty "Wraps into newtype"
      $ \(x :: Integer) -> Foo2.Frob x === migrate Foo2.frob x
    ]
  , testGroup "Debugging"
    [ testProperty "To debugging state"
      $ \(x :: Int) (y :: Int) (z :: Int)
      -> Debugging { dbgState = x, state = y } === migrate Debugging { dbgState = x, state = z } y
    , testProperty "From debugging state"
    $ \(x :: Int) (y :: Int) (z :: Int)
    -> x === migrate y Debugging { dbgState = z, state = x }
    ]
  ]
