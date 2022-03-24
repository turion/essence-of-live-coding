{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Migrate.NoMigration where

-- base
import Control.Arrow (Arrow (arr), (>>>))
import Data.Data (Data)
import Data.Maybe (fromJust)
-- test-framework

-- test-framework-quickcheck2

-- essence-of-live-coding

import qualified LiveCoding.Migrate.NoMigration as NoMigration
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Util

data Stuff a = Stuff a deriving (Eq, Data)

test =
  testGroup
    "NoMigration unit tests"
    [ testProperty
        "LiveCoding.Migrate.NoMigration.delay migrates correctly to itself"
        CellMigrationSimulation
          { cell1 = NoMigration.delay 0,
            cell2 = NoMigration.delay 0,
            input1 = [1 :: Int, 2, 3, 4],
            input2 = [5 :: Int, 6, 7, 8],
            output1 = [0, 1, 2, 3],
            output2 = [4, 5, 6, 7]
          },
      testProperty
        "LiveCoding.Migrate.NoMigration.delay different type will not migrate"
        CellMigrationSimulation
          { cell1 = NoMigration.delay 0,
            cell2 = arr Stuff >>> NoMigration.delay (Stuff 99) >>> arr (\(Stuff a) -> a),
            input1 = [1 :: Int, 2, 3, 4],
            input2 = [10 :: Int, 10, 10, 10],
            output1 = [0, 1, 2, 3],
            output2 = [99, 10, 10, 10]
          }
    ]
