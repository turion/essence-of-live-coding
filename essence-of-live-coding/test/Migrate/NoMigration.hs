{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Migrate.NoMigration where

-- base
import Control.Arrow ( Arrow(arr), (>>>) )
import Data.Maybe (fromJust)
import Data.Data (Data)

-- test-framework
import Test.Framework ( testGroup )

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2 ( testProperty )

-- essence-of-live-coding
import Util
import qualified LiveCoding.Migrate.NoMigration as NoMigration

data Stuff a = Stuff a deriving (Eq, Data)

test = testGroup "NoMigration unit tests"
  [ testProperty "LiveCoding.Migrate.NoMigration.delay migrates correctly to itself" CellMigrationSimulation
    { cell1 = NoMigration.delay 0
    , cell2 = NoMigration.delay 0
    , input1 = [1 :: Int, 2, 3, 4]
    , input2 = [5 :: Int, 6, 7, 8]
    , output1 = [0, 1, 2, 3]
    , output2 = [4, 5, 6, 7]
    }
  , testProperty "LiveCoding.Migrate.NoMigration.delay different type will not migrate" CellMigrationSimulation
    { cell1 = NoMigration.delay 0
    , cell2 = arr Stuff >>> NoMigration.delay (Stuff 99) >>> arr (\(Stuff a) -> a)
    , input1 = [1 :: Int, 2, 3, 4]
    , input2 = [10 :: Int, 10, 10, 10]
    , output1 = [0, 1, 2, 3]
    , output2 = [99, 10, 10, 10]
     }
  ]
