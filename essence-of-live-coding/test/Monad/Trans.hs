{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Monad.Trans where

-- test-framework
import Test.Framework

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2

-- QuickCheck
import Test.QuickCheck

-- essence-of-live-coding
import LiveCoding
import LiveCoding.Cell.Monad.Trans (State(State))

test = testGroup "Monad.Trans"
  [ testProperty "Migrates into runStateL"
    $ \(stateT :: Int) (stateInternal :: Int)
      -> State { .. } === migrate State { stateInternal = 23, .. } stateInternal
  , testProperty "Migrates from runStateL"
  $ \(stateT :: Int) (stateInternal :: Int)
    -> stateInternal === migrate 42 State { .. }
  ]
