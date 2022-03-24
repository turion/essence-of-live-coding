{-# LANGUAGE ScopedTypeVariables #-}

module Cell where

-- base

-- transformers

-- test-framework

-- test-framework-quickcheck2

-- QuickCheck

-- essence-of-live-coding

import qualified Cell.Monad.Trans
import qualified Cell.Util
import Control.Category
import Control.Monad.Trans.Identity
import Data.Functor.Identity
import LiveCoding
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Prelude hiding (id)

test =
  testGroup
    "Cell"
    [ testProperty "steps produces outputs" $
        \(inputs :: [Int]) -> inputs === fst (runIdentity $ steps (id :: Cell Identity Int Int) inputs),
      testProperty "sumC works as expected" $
        forAll (vector 100) $ \(inputs :: [Int]) ->
          sum (init inputs)
            === last (fst (runIdentity $ steps (sumC :: Cell Identity Int Int) inputs)),
      Cell.Util.test,
      Cell.Monad.Trans.test
    ]
