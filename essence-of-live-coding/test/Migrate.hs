module Migrate where

-- base
import Data.Data (dataTypeName, isAlgType)

-- QuickCheck
import Test.QuickCheck.Property ((===))

-- test-framework
import Test.Framework

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- essence-of-live-coding
import LiveCoding (Data (dataTypeOf), castMigration, runSafeMigration)
import LiveCoding.Migrate

import Migrate.NoMigration
import qualified TestData.Foo1 as Foo1
import qualified TestData.Foo2 as Foo2

test =
  testGroup
    "Migrate"
    [ testGroup
        "Internal assumptions"
        [ testGroup
            "matchingAlgebraicDataTypes"
            [ testProperty "True for types with the same name" $
                matchingAlgebraicDataTypes Foo1.foo Foo2.foo
            , testGroup
                "debugging tests"
                [ testProperty "isAlgType" $ isAlgType $ dataTypeOf Foo1.foo
                ]
            ]
        ]
    , testGroup
        "standard migrations"
        [ testGroup
            "castMigration"
            [ testProperty "Migrates for same data type" $
                runSafeMigration castMigration Foo1.same Foo1.same
                  == Foo1.same
            , testProperty "Does not migrate for different data types" $
                runSafeMigration castMigration Foo1.same Foo2.same
                  == Foo1.same
            , testProperty "Migrates for same builtin type" $
                runSafeMigration castMigration (23 :: Int) (42 :: Int)
                  == 42
            , testProperty "Does not migrate for different builtin types" $
                runSafeMigration castMigration (23 :: Int) (42 :: Integer)
                  == 23
            ]
        , testGroup
            "sameConstructorMigration"
            [ testProperty "Migrates when constructor names and arity match" $
                runSafeMigration (sameConstructorMigration castMigration) Foo1.same Foo2.same
                  == Foo1.same'
            , testProperty "Migrates for same data type" $
                runSafeMigration (sameConstructorMigration castMigration) Foo1.same Foo1.same'
                  == Foo1.same'
            ]
        ]
    , Migrate.NoMigration.test
    ]
