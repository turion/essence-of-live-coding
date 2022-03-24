{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module Feedback where

-- essence-of-live-coding

-- test-framework

-- test-framework-quickcheck2

-- QuickCheck

-- essence-of-live-coding
import LiveCoding
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Util

constCell :: Monad m => Int -> Cell m () Int
constCell cellState =
  Cell
    { cellStep = \state _ -> return (state, state),
      ..
    }

test =
  testGroup
    "Feedback"
    [ testProperty
        "Migrates into feedback"
        CellMigrationSimulation
          { cell1 = constCell 23,
            cell2 = feedback [] $ proc ((), ns) -> do
              n <- constCell 42 -< ()
              returnA -< (sum ns, n : ns),
            input1 = replicate 3 (),
            input2 = replicate 3 (),
            output1 = [23, 23, 23],
            output2 = [0, 23, 46]
          },
      testProperty
        "Migrates out of feedback"
        CellMigrationSimulation
          { cell1 = feedback [] $ proc ((), ns) -> do
              n <- constCell 23 -< ()
              returnA -< (sum ns, n : ns),
            cell2 = constCell 42,
            input1 = replicate 3 (),
            input2 = replicate 3 (),
            output1 = [0, 23, 46],
            output2 = [23, 23, 23]
          }
    ]
