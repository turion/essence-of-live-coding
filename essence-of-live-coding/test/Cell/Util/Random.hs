{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cell.Util.Random where

-- quickcheck
import Test.QuickCheck

-- essence-of-live-coding
import LiveCoding

instance (Monad m, Arbitrary1 m, CoArbitrary a, Arbitrary b) => Arbitrary (Cell m a b) where
  arbitrary =
    oneof
      [ genAtProxies genBasicCellAt
      , genAtProxies genCompositionAt
      , genArrM
      , genAtProxies genWithFirstAt''
      ]

genCellAtOut :: (Arbitrary1 m, CoArbitrary a, Arbitrary b, Monad m) => Proxy b -> Gen (Cell m a b)
genCellAtOut _ = arbitrary

genCellAtIn :: (Arbitrary1 m, CoArbitrary a, Arbitrary b, Monad m) => Proxy a -> Gen (Cell m a b)
genCellAtIn _ = arbitrary

genAtProxies :: (forall s. (Data s, CoArbitrary s, Arbitrary s) => Proxy s -> Gen a) -> Gen a
genAtProxies genAt =
  oneof
    [ genAt (Proxy @Int)
    , genAt (Proxy @())
    , genAt (Proxy @[Double])
    , genAt (Proxy @(Maybe Integer))
    , genAt (Proxy @String)
    ]

genArrM :: (CoArbitrary a, Arbitrary1 m, Arbitrary b) => Gen (Cell m a b)
genArrM = ArrM <$> liftArbitrary (liftArbitrary arbitrary)

genBasicCellAt :: (Data s, Arbitrary1 m, CoArbitrary s, CoArbitrary a, Arbitrary b, Arbitrary s) => Proxy s -> Gen (Cell m a b)
genBasicCellAt proxy = Cell <$> genStateAt proxy <*> genStepAt proxy

genStepAt :: (Arbitrary1 m, CoArbitrary s, CoArbitrary a, Arbitrary b, Arbitrary s) => Proxy s -> Gen (s -> a -> m (b, s))
genStepAt proxy = liftArbitrary $ liftArbitrary $ liftArbitrary arbitrary

genStateAt :: Arbitrary s => Proxy s -> Gen s
genStateAt _ = arbitrary

genCompositionAt :: (Monad m, Arbitrary1 m, CoArbitrary a, Arbitrary b, CoArbitrary b, Arbitrary c) => Proxy b -> Gen (Cell m a c)
genCompositionAt proxy = (>>>) <$> genCellAtOut proxy <*> genCellAtIn proxy

genFirst :: (Monad m, Arbitrary1 m, CoArbitrary a, Arbitrary b) => Proxy c -> Gen (Cell m (a, c) (b, c))
genFirst _ = first <$> arbitrary

genCellOutTuple :: (Monad m, Arbitrary1 m, CoArbitrary a, Arbitrary b, Arbitrary c) => Proxy b -> Proxy c -> Gen (Cell m a (b, c))
genCellOutTuple _ _ = arbitrary

genCellInTuple :: (Monad m, Arbitrary1 m, CoArbitrary a, CoArbitrary b, Arbitrary c) => Proxy a -> Proxy b -> Gen (Cell m (a, b) c)
genCellInTuple _ _ = arbitrary

-- a -> (b, d) -> (c, d) -> e
genWithFirstAt :: (Monad m, Arbitrary1 m, CoArbitrary a, CoArbitrary b, Arbitrary c, Arbitrary b, Arbitrary d, CoArbitrary c, CoArbitrary d, Arbitrary e) => Proxy b -> Proxy c -> Proxy d -> Gen (Cell m a e)
genWithFirstAt bProxy cProxy dProxy = (>>>) <$> genCellOutTuple bProxy dProxy <*> ((>>>) <$> genFirst dProxy <*> genCellInTuple cProxy dProxy)

genWithFirstAt' :: (Monad m, Arbitrary1 m, CoArbitrary a, Arbitrary c, Arbitrary d, CoArbitrary c, CoArbitrary d, Arbitrary e) => Proxy c -> Proxy d -> Gen (Cell m a e)
genWithFirstAt' cProxy dProxy = genAtProxies $ genWithFirstAt cProxy dProxy

genWithFirstAt'' :: (Monad m, Arbitrary1 m, CoArbitrary a, Arbitrary d, CoArbitrary d, Arbitrary e) => Proxy d -> Gen (Cell m a e)
genWithFirstAt'' dProxy = genAtProxies $ genWithFirstAt' dProxy
