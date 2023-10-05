{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cell.Util.Traversable where

-- base
import qualified Control.Category as C
import Control.Monad
import Data.Functor.Identity
import Data.List
import Data.Maybe
import GHC.TypeLits (KnownNat)

-- containers
import Data.Map (Map)
import Data.Sequence (Seq)

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

-- essence-of-live-coding
import LiveCoding

import Util

type TestTraversables = Traversables '[Maybe, [], V.Vector 10, Seq, Map Int]

testTraverse' :: Test
testTraverse' =
  testGroup
    "Traversing unit tests"
    [ genTraversableTests' @TestTraversables "traverse' (arr f) = arr (f <$>)" $
        makeTraversableTest (traverseArrLaw @Int @Int)
    , genTraversableTests' @TestTraversables
        "traverse' works as expected for any Cell Identy Int Int created with constructor Cell"
        $ makeTraversableTest (traverseCellTest @Int @Int @Int)
    , testProperty "traverse' by itself does not force the entire list (ArrM)" $
        CellSimulation
          { cell = arr head
          , input = [1 : error "Bang !"]
          , output = [1]
          }
    , testProperty "traverse' by itself does not force the entire list (Cell)" $
        CellSimulation
          { cell = toCell $ arr head
          , input = [1 : error "Bang !"]
          , output = [1]
          }
    ]

traverseArrLaw ::
  forall a b t.
  (Traversable t) =>
  Proxy t ->
  [t a] ->
  Fun a b ->
  CellIdentitySimulation (t a) (t b)
traverseArrLaw _ joinInput (Fn f) =
  CellIdentitySimulation
    { cellL = arr (f <$>)
    , cellR = traverse' (arr f)
    , ..
    }

traverseCellTest ::
  forall s a b t.
  (Traversable t, Data s) =>
  Proxy t ->
  s ->
  Fun (s, a) (b, s) ->
  [t a] ->
  CellSimulation (t a) (t b)
traverseCellTest _ s (Fn2 f) input =
  CellSimulation
    { cell = traverse' (Cell s (\s a -> pure $ f s a))
    , output = runIdentity $ evalStateT (traverse (traverse (\a -> StateT (Identity . (`f` a)))) input) s
    , ..
    }

makeTraversableTest :: forall (t :: * -> *) a. (Testable a, Typeable t) => (Proxy t -> a) -> Proxy t -> Test
makeTraversableTest a _ = testProperty (show (typeRep (Proxy :: Proxy t))) (a (Proxy :: Proxy t))

-- | A data type to store types which are instances of 'Traversable'.
data Traversables :: [* -> *] -> *

-- | A type class for induction on the type-level list containing the Traversables.
class GenTests a where
  genTraversableTests ::
    (forall (t :: * -> *). (Arbitrary (t Int), Show (t Int), Eq (t Int), Traversable t, Typeable t) => Proxy t -> Test) ->
    Proxy a ->
    [Test]

instance GenTests (Traversables '[]) where
  genTraversableTests _ _ = []

instance
  (GenTests (Traversables xs), Arbitrary (x Int), Show (x Int), Eq (x Int), Traversable x, Typeable x) =>
  GenTests (Traversables (x ': xs))
  where
  genTraversableTests f _ = f (Proxy :: Proxy x) : genTraversableTests f (Proxy :: Proxy (Traversables xs))

genTraversableTests' ::
  forall a.
  (GenTests a) =>
  String ->
  (forall (t :: * -> *). (Arbitrary (t Int), Show (t Int), Eq (t Int), Traversable t, Typeable t) => Proxy t -> Test) ->
  Test
genTraversableTests' message f = testGroup message $ genTraversableTests f (Proxy :: Proxy a)

instance (Arbitrary a, KnownNat n) => Arbitrary (V.Vector n a) where
  arbitrary = V.replicateM arbitrary
  shrink = V.mapM shrink
