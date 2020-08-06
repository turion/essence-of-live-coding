{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Handle where

-- base
import Control.Arrow (arr, (>>>))
import Data.Functor.Identity

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict

-- test-framework
import Test.Framework

-- test-framework
import Test.Framework

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2

-- QuickCheck
import Test.QuickCheck

-- essence-of-live-coding
import qualified Handle.LiveProgram
import LiveCoding
import Util

testHandle :: Handle (State Int) String
testHandle = Handle
  { create = do
      n <- get
      return $ "Handle #" ++ show n
  , destroy = const $ put 10000
  }

cellWithAction
  :: State Int b
  -> Cell Identity () (String, Int)
cellWithAction action = flip runStateC 0 $ runHandlingStateC $ handling testHandle >>> arrM (<$ lift action)

test = testGroup "Handle"
  [ testProperty "Preserve Handles" CellMigrationSimulation
    { cell1 = cellWithAction $ do
        n <- get
        let n' = n + 1
        put n'
    , cell2 = cellWithAction $ return ()
    , input1 = replicate 3 ()
    , input2 = replicate 3 ()
    , output1 = ("Handle #0", ) <$> [1, 2, 3]
    , output2 = ("Handle #0", ) <$> [3, 3, 3]
    }
  , testProperty "Trigger destructors" CellMigrationSimulation
    { cell1 = cellWithAction $ return ()
    , cell2 = flip runStateC (error "Shouldn't reinitialise state")
        $ runHandlingStateC $ arr $ const "Done"
    , input1 = replicate 3 ()
    , input2 = replicate 3 ()
    , output1 = ("Handle #0", ) <$> replicate 3 0
    , output2 = ("Done", ) <$> replicate 3 10000
    }
  , Handle.LiveProgram.test
  ]
