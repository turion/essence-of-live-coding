{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module LiveCoding.Bayes where

-- base
import Control.Arrow
import Control.Monad (forM, join)
import Data.Data
import Data.Functor.Compose
import Data.Tuple (swap)

-- transformers
import Control.Monad.Trans.Free

-- monad-bayes
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population hiding (hoist)

-- livecoding

import Data.Functor (($>))
import LiveCoding (Cell (..), hoistCell, once_, resample, safe, safely)

runPopulationC ::
  forall m a b.
  Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x. Population m x -> Population m x) ->
  Cell (Population m) a b ->
  -- FIXME Why not Cell m a (Population b)
  Cell m a [(b, Log Double)]
runPopulationC nParticles resampler cell = runListC . fmap swap . handleCompose . hoistCell (massagePopulation . normalize . resampler) $ safely $ do
  once_ $ spawn nParticles
  safe cell

runPopulationC' ::
  forall m a b.
  Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x. Population m x -> Population m x) ->
  Cell (Population m) a b ->
  -- FIXME Why not Cell m a (Population b)
  Cell m a [(b, Log Double)]
runPopulationC' nParticles resampler cell = runFreeListC . fmap swap . handleCompose . hoistCell (massagePopulation' . normalize . resampler) $ safely $ do
  once_ $ spawn nParticles
  safe cell

massagePopulation :: Functor m => Population m a -> Compose (ListT m) ((,) (Log Double)) a
massagePopulation = Compose . ListT . fmap (map swap) . runPopulation

massagePopulation' :: Monad m => Population m a -> Compose (FreeListT m) ((,) (Log Double)) a
massagePopulation' = Compose . freeListT . fmap (map swap) . runPopulation

runPopulationC'' ::
  forall m a b.
  Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x. Population m x -> Population m x) ->
  Cell (Population m) a b ->
  -- FIXME Why not Cell m a (Population b)
  Cell m a [(b, Log Double)]
runPopulationC'' nParticles resampler Cell {cellState, cellStep} =
  Cell
    { cellState = replicate nParticles (cellState, 1 / fromIntegral nParticles)
    , cellStep = \s a -> do
        bAndS <- runPopulation $ normalize $ resampler $ forM s $ \(s, p) -> factor p >> cellStep s a
        -- FIXME This ugliness surely can be improved, maybe by returning into Population first and joining there?
        return $ unzip $ map (\((b, s), p) -> ((b, p), (s, p))) $ concatMap ((\(p, bs) -> (,p) <$> bs) . swap) bAndS
    }
-- It's a bit pointless to do this on an ArrM
runPopulationC'' nParticles resampler ArrM {runArrM} = ArrM {runArrM = runPopulation . normalize . resampler . runArrM}

-- FIXME see PR re-adding this to monad-bayes
normalize :: Monad m => Population m a -> Population m a
normalize = fromWeightedList . fmap (\particles -> second (/ (sum $ snd <$> particles)) <$> particles) . runPopulation

newtype ListT m a = ListT {getListT :: m [a]}
  deriving (Functor)

deriving via (Compose m []) instance Applicative m => Applicative (ListT m)

-- FIXME Can I rewrite everything with the free thing directly?
instance Monad m => Monad (ListT m) where
  ma >>= f = ListT $ runFreeListT $ freeListT (getListT ma) >>= freeListT . getListT . f

runListC :: Monad m => Cell (ListT m) a b -> Cell m a [b]
runListC Cell {cellState, cellStep} =
  Cell
    { cellState = [cellState]
    , cellStep = \s a -> do
        x <- forM s $ getListT . flip cellStep a
        return $ unzip $ concat x
    }
runListC ArrM {runArrM} = ArrM {runArrM = getListT . runArrM}

handleCompose :: ((forall s. Data s => Data (f s)), Monad m, Traversable f, Monad f) => Cell (Compose m f) a b -> Cell m a (f b)
handleCompose Cell {cellState, cellStep} =
  Cell
    { cellState = return cellState
    , cellStep = \fs a -> do
        x <- forM fs $ getCompose . flip cellStep a
        return $ fmap fst &&& fmap snd $ join x
    }
handleCompose ArrM {runArrM} = ArrM {runArrM = getCompose . runArrM}

handleCompose' :: ((forall s. Data s => Data (f s)), Monad m, Traversable f, Applicative f, Monad (Compose m f)) => Cell (Compose m f) a b -> Cell m a (f b)
handleCompose' Cell {cellState, cellStep} =
  Cell
    { cellState = pure cellState
    , cellStep = \fs a -> do
        x <- getCompose $ (Compose . return) =<< forM fs (`cellStep` a)
        return $ fmap fst &&& fmap snd $ x
    }
handleCompose' ArrM {runArrM} = ArrM {runArrM = getCompose . runArrM}

newtype FreeListT m a = FreeListT {getFreeListT :: FreeT [] m a}
  deriving (Functor, Applicative, Monad)

runFreeListT :: Monad m => FreeListT m a -> m [a]
runFreeListT = iterT (fmap concat . sequence) . fmap pure . getFreeListT

freeListT :: Monad m => m [a] -> FreeListT m a
freeListT = FreeListT . FreeT . fmap (Free . map return)

runFreeListC :: Monad m => Cell (FreeListT m) a b -> Cell m a [b]
runFreeListC Cell {cellState, cellStep} =
  Cell
    { cellState = [cellState]
    , cellStep = \s a -> do
        x <- runFreeListT $ forM s $ flip cellStep a
        return $ fmap fst &&& fmap snd $ concat x
    }
runFreeListC ArrM {runArrM} = ArrM {runArrM = runFreeListT . runArrM}
