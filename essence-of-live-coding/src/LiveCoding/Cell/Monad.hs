{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{- |
Handling monad morphisms.
-}
module LiveCoding.Cell.Monad where

-- essence-of-live-coding

import Control.Arrow (Arrow (arr), (>>>))
import Data.Data (Data, Typeable)
import LiveCoding.Cell

-- | Apply a monad morphism that also transforms the output to a cell.
hoistCellOutput ::
  (Monad m1, Monad m2) =>
  (forall s. m1 (b1, s) -> m2 (b2, s)) ->
  Cell m1 a b1 ->
  Cell m2 a b2
hoistCellOutput morph = hoistCellKleisli_ (morph .)

-- | Apply a transformation of Kleisli morphisms to a cell.
hoistCellKleisli_ ::
  (Monad m1, Monad m2) =>
  (forall s. (a1 -> m1 (b1, s)) -> (a2 -> m2 (b2, s))) ->
  Cell m1 a1 b1 ->
  Cell m2 a2 b2
hoistCellKleisli_ morph = hoistCellKleisli (morph .)

-- | Apply a transformation of stateful Kleisli morphisms to a cell.
hoistCellKleisli ::
  (Monad m1, Monad m2) =>
  (forall s. (s -> a1 -> m1 (b1, s)) -> (s -> a2 -> m2 (b2, s))) ->
  Cell m1 a1 b1 ->
  Cell m2 a2 b2
hoistCellKleisli morph ArrM {..} =
  ArrM
    { runArrM = (fmap fst .) $ ($ ()) $ morph $ const $ runArrM >>> fmap (,())
    }
hoistCellKleisli morph Cell {..} =
  Cell
    { cellStep = morph cellStep
    , ..
    }

{- | Apply a transformation of stateful Kleisli morphisms to a cell,
   changing the state type.
-}
hoistCellKleisliStateChange ::
  (Monad m1, Monad m2, Typeable t, (forall s. Data s => Data (t s))) =>
  ( forall s.
    (s -> a1 -> m1 (b1, s)) ->
    (t s -> a2 -> m2 (b2, t s))
  ) ->
  (forall s. (s -> t s)) ->
  Cell m1 a1 b1 ->
  Cell m2 a2 b2
hoistCellKleisliStateChange morph init Cell {..} =
  Cell
    { cellStep = morph cellStep
    , cellState = init cellState
    }
hoistCellKleisliStateChange morph init cell = hoistCellKleisliStateChange morph init $ toCell cell
