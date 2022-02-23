{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cell.Util where

-- base
import qualified Control.Category as C
import Data.Functor.Identity
import Data.Maybe
import Control.Monad
import Data.List

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
  , testProperty "delay a >>> changes >>> hold a == delay a"
    $ \(inputs :: [Int]) (startValue :: Int) -> fst (runIdentity $ steps (delay startValue) inputs) ===
        fst (runIdentity $ steps (delay startValue >>> changes >>> hold startValue) inputs)
  , testProperty "changes applied to a cell that outputs a constant, always outputs Nothing"
    $ \(value :: Int) (inputs :: [Int]) -> [] ===
        catMaybes (fst (runIdentity $ steps (arr (const value) >>> changes) inputs))
  , testProperty "changes works as expected" CellSimulation
    { cell = changes
    , input =
        [ 1 :: Int
        , 1 :: Int
        , 2 :: Int
        , 2 :: Int
        ]
    , output =
        [ Nothing
        , Nothing
        , Just (2 :: Int)
        , Nothing
        ]
    }
  , testProperty "changes migrates correctly to itself" CellMigrationSimulation
    { cell1 = changes
    , cell2 = changes
    , input1 = [1, 2] :: [Int]
    , input2 = [3, 4] :: [Int]
    , output1 = [Nothing, Just 2]
    , output2 = [Just 3, Just 4]
    }
    , testProperty "resampleListPar works as expected"
    $ forAll (vector 100) $ \(inputs :: [(Int, Int)]) -> let inputs' = fmap pairToList inputs  in
        fmap sum (transpose (init inputs')) ===
          last (fst (runIdentity $ steps (resampleListPar (sumC :: Cell Identity Int Int)) inputs'))
  ]

pairToList :: (a, a) -> [a]
pairToList (x,y) = [x,y]
