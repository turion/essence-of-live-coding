{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cell.Util where

-- base
import qualified Control.Category as C
import Data.Functor.Identity
import Data.Maybe
import Control.Monad

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

test = testGroup "Utility unit tests"
--   [ testProperty "Buffer works as expected" CellSimulation
--       { cell = buffer
--       , input = []
--     --   , input =
--     --       [ []
--     --       , [Pop]
--     --       , [Push (23 :: Int)]
--     --       , []
--     --       , [Pop, Pop]
--     --       , [Push 42, Pop]
--     --       , [Push 1, Push 2]
--     --       , []
--     --       , []
--     --       , [Pop, Push 3]
--     --       , []
--     --       , [Pop]
--     --       , [Pop]
--     --       , []
--     --       ]
--       , output = []
--     --   , output =
--     --       [ Nothing
--     --       , Nothing
--     --       , Just 23
--     --       , Just 23
--     --       , Nothing
--     --       , Nothing
--     --       , Just 1
--     --       , Just 1
--     --       , Just 1
--     --       , Just 2
--     --       , Just 2
--     --       , Just 3
--     --       , Nothing
--     --       , Nothing
--     --       ]
--       }
  [ testProperty "buffered works as expected" CellSimulation
    { cell = buffered C.id
    , input =
        [ Just (23 :: Int)
        ]
    , output =
        [ Nothing
        ]
    }
  , testProperty "buffered can be used in an asynchronous setting" $ do
      jointInputs <- arbitrary -- Simulates when input arrives and when inner cell is activated
      -- Make sure cell is ticked at the last time, and no new input arrives
      let innerCell = proc (aMaybe :: Maybe Int) -> do
            isScheduled <- constM ask -< ()
            returnA -< guard isScheduled >> aMaybe
          outerCell = buffered innerCell
          (outputs, _) = runIdentity $ steps (runReaderC' outerCell) jointInputs
          labelString = unwords [show jointInputs, show outputs, show $ length jointInputs, show $ length outputs]
          inputs = snd $ unzip jointInputs
          bufferNotEmpty = isJust $ listToMaybe $ reverse outputs
      -- Make sure each message arrived exactly once, in order
      return
        $ counterexample labelString
        $ catMaybes inputs === catMaybes outputs
        .||. bufferNotEmpty
  ]
