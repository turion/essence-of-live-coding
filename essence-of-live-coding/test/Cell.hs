{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Cell where

-- base
import Prelude hiding (id)
import Control.Arrow ( Arrow(arr), (>>>) )
import Control.Category (id)
import Data.Functor.Identity

-- transformers
import Control.Monad.Trans.Identity

-- test-framework
import Test.Framework

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2

-- QuickCheck
import Test.QuickCheck

-- essence-of-live-coding
import LiveCoding.Cell

import qualified Cell.Util
import qualified Cell.Monad.Trans

test = testGroup "Cell"
  [ testProperty "steps produces outputs"
    $ \(inputs :: [Int]) -> inputs === fst (runIdentity $ steps (id :: Cell Identity Int Int) inputs)
  , testProperty "sumC works as expected"
    $ forAll (vector 100) $ \(inputs :: [Int]) ->
        sum (init inputs) ===
          last (fst (runIdentity $ steps (sumC :: Cell Identity Int Int) inputs))
  , Cell.Util.test
  , Cell.Util.testTraverse'
  , Cell.Monad.Trans.test
  ]
