{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Cell.Util where

-- base
import qualified Control.Category as C
import Data.Functor.Identity
import Data.Maybe
import Control.Monad
import Data.List
import Control.Exception

-- transformers
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy


-- vector-sized
import qualified Data.Vector.Sized as V

-- test-framework
import Test.Framework

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2

-- QuickCheck
import Test.QuickCheck hiding (output)

-- HUnit
import Test.HUnit (assertFailure)

-- test-framework-hunit
import Test.Framework.Providers.HUnit (testCase)

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
  , testProperty "delay a >>> changes >>> hold a = delay a"
    $ \(inputs :: [Int]) (startValue :: Int) ->
      CellIdentitySimulation  (delay startValue) (delay startValue >>> changes >>> hold startValue) inputs
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
  , testProperty "delay migrates correctly to itself" CellMigrationSimulation
    { cell1 = LiveCoding.delay 0
    , cell2 = LiveCoding.delay 0
    , input1 = [1 :: Int, 2, 3, 4]
    , input2 = [5 :: Int, 6, 7, 8]
    , output1 = [0, 1, 2, 3]
    , output2 = [4, 5, 6, 7]
    }
  , testProperty "delay migrates correctly with original type wrapped in data type with single constructor" CellMigrationSimulation
    { cell1 = LiveCoding.delay 0 :: Cell Identity Int Int
    , cell2 = arr Stuff >>> LiveCoding.delay (Stuff 99) >>> arr (\(Stuff a) -> a) :: Cell Identity Int Int
    , input1 = [1, 2, 3, 4] :: [Int]
    , input2 = [10, 10, 10, 10] :: [Int]
    , output1 = [0, 1, 2, 3] :: [Int]
    , output2 = [4, 10, 10, 10] :: [Int]
    }
  , testProperty "resampleListPar works as expected"
    $ forAll (vector 100) $ \(inputs :: [(Int, Int)]) -> let
        inputs' = fmap pairToList inputs
        pairToList :: (a, a) -> [a]
        pairToList (x,y) = [x,y]
      in 
        CellSimulation
        { cell = resampleListPar (sumC :: Cell Identity Int Int)
        , input = inputs'
        , output = fmap sum . transpose <$> [[0::Int,0]] : tail (inits (init inputs'))
        }
  , testProperty "resampleListPar grow" CellSimulation
    { cell = resampleListPar (sumC :: Cell Identity Int Int)
    , input = [[1,1,1],[1,1,1],[1,1,1,1],[1,1,1,1],[1,1,1,1,1]]
    , output = [[0,0,0],[1,1,1],[2,2,2,0],[3,3,3,1],[4,4,4,2,0]]
    }
  , testProperty "resampleListPar shrink" CellSimulation
    { cell = resampleListPar (sumC :: Cell Identity Int Int)
    , input = [[1,1,1],[1,1,1],[1,1],[1,1],[1],[]]
    , output = [[0,0,0],[1,1,1],[2,2],[3,3],[4],[]]
    }
  , testProperty "resampleListPar grow then shrink" CellSimulation
    { cell = resampleListPar (sumC :: Cell Identity Int Int)
    , input = [[1,1,1],[1,1,1,1],[1,1,1]]
    , output = [[0,0,0],[1,1,1,0],[2,2,2]]
    }
  , testProperty "resampleListPar shrink then grow" CellSimulation
    { cell = resampleListPar (sumC :: Cell Identity Int Int)
    , input = [[1,1,1],[1,1],[1,1,1]]
    , output = [[0,0,0],[1,1],[2,2,0]]
    }
  ]

testTraverse' :: Test
testTraverse' = testGroup "Traversing unit tests"
  [ testProperty "traverse' (arr f) = arr (f <$>) ([a])"
    $ \(input :: [[Int]], Fn (f :: Int -> Int)) -> traverseArrLaw f input
  , testProperty "traverse' (arr f) = arr (f <$>) (Maybe a)"
    $ \(input :: [Maybe Int], Fn (f :: Int -> Int)) -> traverseArrLaw f input
  , testProperty "traverse' (arr f) = arr (f <$>) (Vecttor 4 Int)"
    $ \(input :: [(Int,Int,Int,Int)], Fn (f :: Int -> Int)) -> traverseArrLaw f (V.fromTuple <$> input)
  , testProperty "traverse' works as expected for any Cell Identy Int Int created with constructor Cell ([a])"
    $ \(input :: [[Int]], s :: Int, Fn (f :: (Int, Int) -> (Int,Int))) -> traverseCellTest s (curry f) input
  , testProperty "traverse' works as expected for any Cell Identy Int Int created with constructor Cell (Maybe a)"
    $ \(input :: [Maybe Int], s :: Int, Fn (f :: (Int, Int) -> (Int,Int))) -> traverseCellTest s (curry f) input
  , testProperty "traverse' works as expected for any Cell Identy Int Int created with constructor Cell (Vecttor 4 Int)"
    $ \(input :: [(Int,Int,Int,Int)], s :: Int, Fn (f :: (Int, Int) -> (Int,Int))) -> traverseCellTest s (curry f) (V.fromTuple <$> input)
  , testCase "traverse' by itself does not force the entire list (ArrM)"
    $ testError "traverse' is too strict"
    $ head $ head $ fst $ runIdentity $ steps (traverse' (arr id) :: Cell Identity [Int] [Int]) [1 : error "Bang !"]
  , testCase "traverse' by itself does not force the entire list (Cell)"
    $ testError "Traverse' is too strict"
    $ head $ head $ fst $ runIdentity $ steps (traverse' cellId :: Cell Identity [Int] [Int]) [1 : error "Bang !"]
  , testCase "traverse' of a Cell composed with another Cell does not force the entire list"
    $ testError "Traverse' is too strict"
    $ head $ head $ fst $ runIdentity $ steps (traverse' cellId >>> cellId  :: Cell Identity [Int] [Int]) [1 : error "Bang !"]
  ]

traverseArrLaw ::
  Traversable f =>
  (a -> b) -> [f a] -> CellIdentitySimulation (f a) (f b)
traverseArrLaw f = CellIdentitySimulation (arr (f <$>)) (traverse' (arr f))

traverseCellTest :: forall s a b t . (Traversable t, Data b, Data s) =>
  s
  -> (s -> a -> (b,s))
  -> [t a]
  -> CellSimulation (t a) (t b)
traverseCellTest s f input = CellSimulation
  { cell = traverse' (Cell s (\s a -> pure $ f s a)) :: Cell Identity (t a) (t b)
  ,  output = runIdentity $ evalStateT (traverse (traverse (\a -> StateT (Identity . (`f` a)) )) input) s
  , ..
  }

-- | Tests whether the value `a` throws an exception.
testError :: String  -> a -> IO ()
testError message value = do
  let
    handler :: ErrorCall -> IO Bool
    handler e = pure True
  errored <- catch (pure $ seq value False) handler
  when errored $ assertFailure message

cellId :: Monad m => Cell m b b
cellId = Cell {..} where
  cellState = ()
  cellStep s a = pure (a,())

data Stuff a = Stuff a deriving (Eq, Data)
