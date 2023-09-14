{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Run a cell at a fixed integer multiple speed.
The general approach is to take an existing cell (the "inner" cell)
and produce a new cell (the "outer" cell) that will accept several copies of the input.
The inner cell is stepped for each input.
-}
module LiveCoding.Cell.Resample where

-- base
import Control.Arrow
import Data.Maybe
import GHC.TypeNats

-- profunctors
import Data.Profunctor.Traversing (Traversing (traverse'))

-- vector-sized
import Data.Vector.Sized (Vector, fromList, toList)

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Cell.Monad

-- | Execute the inner cell for n steps per outer step.
resample :: (Monad m, KnownNat n) => Cell m a b -> Cell m (Vector n a) (Vector n b)
resample = traverse'

-- | Execute the cell for as many steps as the input list is long.
resampleList :: Monad m => Cell m a b -> Cell m [a] [b]
resampleList = traverse'

resampleMaybe :: Monad m => Cell m a b -> Cell m (Maybe a) (Maybe b)
resampleMaybe = traverse'

{- | Create as many cells as the input list is long and execute them in parallel
 (in the sense that each one has a separate state). At each tick the list with
 the different states grows or shrinks depending on the size of the input list.

 Similar to Yampa's [parC](https://hackage.haskell.org/package/Yampa-0.13.3/docs/FRP-Yampa-Switches.html#v:parC).
-}
resampleListPar :: Monad m => Cell m a b -> Cell m [a] [b]
resampleListPar (Cell initial step) = Cell {..}
  where
    cellState = []
    cellStep s xs = unzipResult <$> traverse (uncurryResult step) (zipResult s' xs)
      where
        s' = s ++ replicate (length xs - length s) initial
resampleListPar (ArrM f) = ArrM (traverse f)
