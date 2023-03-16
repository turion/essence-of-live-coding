{-# LANGUAGE ScopedTypeVariables #-}

module Cell where

-- base

import Control.Category
import Data.Functor.Identity
import Prelude hiding (id)

-- transformers
import Control.Monad.Trans.Identity

-- test-framework
import Test.Framework

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2

-- QuickCheck
import Test.QuickCheck

-- essence-of-live-coding
import LiveCoding

import qualified Cell.Monad.Trans
import qualified Cell.Util

test =
  testGroup
    "Cell"
    [ testProperty "steps produces outputs" $
        \(inputs :: [Int]) -> inputs === fst (runIdentity $ steps (id :: Cell Identity Int Int) inputs)
    , testProperty "sumC works as expected" $
        forAll (vector 100) $ \(inputs :: [Int]) ->
          sum (init inputs)
            === last (fst (runIdentity $ steps (sumC :: Cell Identity Int Int) inputs))
    , Cell.Util.test
    , Cell.Util.testTraverse'
    , Cell.Monad.Trans.test
    ]
