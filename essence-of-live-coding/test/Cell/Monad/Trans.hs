{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cell.Monad.Trans where

-- transformers
import Control.Monad.Trans.Reader
-- test-framework

-- test-framework-quickcheck2

-- QuickCheck

-- essence-of-live-coding
import LiveCoding
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding (output)
import Util

test =
  testGroup
    "Cell.Monad.Trans"
    [ testProperty "readerC" $
        inIdentityT $ proc (n :: Int) -> do
          nReader <- runReaderC' $ constM ask -< (n, ())
          returnA -< n === nReader
    ]
