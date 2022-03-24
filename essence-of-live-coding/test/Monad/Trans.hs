{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monad.Trans where

-- test-framework

-- test-framework-quickcheck2

-- QuickCheck

-- essence-of-live-coding
import LiveCoding
import LiveCoding.Cell.Monad.Trans (State (State))
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

test =
  testGroup
    "Monad.Trans"
    [ testProperty "Migrates into runStateL" $
        \(stateT :: Int) (stateInternal :: Int) ->
          State {..} === migrate State {stateInternal = 23, ..} stateInternal,
      testProperty "Migrates from runStateL" $
        \(stateT :: Int) (stateInternal :: Int) ->
          stateInternal === migrate 42 State {..}
    ]
