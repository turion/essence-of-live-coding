{-# LANGUAGE MultiParamTypeClasses #-}
module Handle where

-- base
import Control.Arrow (arr, (>>>))
import Data.Functor.Identity
import Data.IORef

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2

-- QuickCheck
import Test.QuickCheck

-- essence-of-live-coding
import LiveCoding
import Util


cellWithAction
  :: (IORef Int -> IO b)
  -> Cell Identity a b
cellWithAction action = withEvilDebugger $ runHandlingStateC
  $ handling (ioRefHandle 0) >>> (liftCell $ arrM action)

test = testProperty "Preserve Handles" CellMigrationSimulation
  { cell1 = cellWithAction $ \ref -> do
      n <- readIORef ref
      let n' = n + 1
      writeIORef ref n'
      return n'
  , cell2 = cellWithAction readIORef
  , input1 = replicate 3 ()
  , input2 = replicate 3 ()
  , output1 = [1, 2, 3]
  , output2 = [3, 3, 3]
  }
