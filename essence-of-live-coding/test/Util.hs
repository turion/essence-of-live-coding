{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Util where

-- base
import Data.Functor.Identity
import System.IO.Unsafe (unsafePerformIO)

-- QuickCheck
import Test.QuickCheck

-- essence-of-live-coding
import LiveCoding

{- | A quickcheckable unit test for migrations of cells.

You have to specify a cell which will then receive some input,
is fed each input element in a step, and produces some output.
Then the cell is migrated to a second cell, which again consumes input and produces output.

The test is passed if the same input produces the same output.

* 'cell1': The cell before the migration
* 'cell2': The cell after the migration
* 'input1': All input the cell before the migration receives
* 'input2': All input the cell after the migration receives
* 'output1': The expected output before the migration
* 'output2': The expected output after the migration
-}
data CellMigrationSimulation a b = CellMigrationSimulation
  { cell1 :: Cell Identity a b
  , cell2 :: Cell Identity a b
  , input1 :: [a]
  , input2 :: [a]
  , output1 :: [b]
  , output2 :: [b]
  }

instance (Eq b, Show b) => Testable (CellMigrationSimulation a b) where
  property CellMigrationSimulation {..} =
    let Identity (output1', output2') = simulateCellMigration cell1 cell2 input1 input2
     in output1 === output1' .&&. output2 === output2'

{- | Step the first cell with the first input,
   migrate it to the second cell,
   and step the migration result with the second input.
   Return both outputs.
-}
simulateCellMigration :: Monad m => Cell m a b -> Cell m a b -> [a] -> [a] -> m ([b], [b])
simulateCellMigration cell1 cell2 as1 as2 = do
  (bs1, cell1') <- steps cell1 as1
  let cell2' = hotCodeSwapCell cell2 cell1'
  (bs2, _) <- steps cell2' as2
  return (bs1, bs2)

-- FIXME move to essence-of-live-coding-quickcheck
-- https://github.com/turion/essence-of-live-coding/issues/36
instance (Arbitrary a, Testable prop) => Testable (Cell Identity a prop) where
  property cell = property $ do
    as <- arbitrary
    let (props, _) = runIdentity $ steps cell as
    return $ conjoin props

-- | Helper to unify cells to the 'Identity' monad.
inIdentityT :: Cell Identity a prop -> Cell Identity a prop
inIdentityT = id

{- | Basic unit test for 'Cell's.
   Check whether a given 'input' to your 'cell' results in a given 'output'.
-}
data CellSimulation a b = CellSimulation
  { cell :: Cell Identity a b
  , input :: [a]
  , output :: [b]
  }

instance (Eq b, Show b) => Testable (CellSimulation a b) where
  property CellSimulation {..} =
    property
      CellMigrationSimulation
        { cell1 = cell
        , cell2 = cell
        , input1 = input
        , input2 = []
        , output1 = output
        , output2 = []
        }
