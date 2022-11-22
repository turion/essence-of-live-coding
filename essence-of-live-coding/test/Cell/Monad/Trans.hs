{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cell.Monad.Trans where

-- transformers
import Control.Monad.Trans.Reader

-- test-framework
import Test.Framework

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2

-- QuickCheck
import Test.QuickCheck hiding (output)

-- essence-of-live-coding
import LiveCoding

import Util

test =
  testGroup
    "Cell.Monad.Trans"
    [ testProperty "readerC" $ inIdentityT $ proc (n :: Int) -> do
        nReader <- runReaderC' $ constM ask -< (n, ())
        returnA -< n === nReader
    ]
