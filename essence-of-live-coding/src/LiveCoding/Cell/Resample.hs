{- |
Run a cell at a fixed integer multiple speed.
The general approach is to take an existing cell (the "inner" cell)
and produce a new cell (the "outer" cell) that will accept several copies of the input.
The inner cell is stepped for each input.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module LiveCoding.Cell.Resample where

-- base
import Control.Arrow
import Data.Maybe
import GHC.TypeNats

-- vector-sized
import Data.Vector.Sized

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Cell.Monad

-- | Execute the inner cell for n steps per outer step.
resample :: (Monad m, KnownNat n) => Cell m a b -> Cell m (Vector n a) (Vector n b)
resample cell = arr toList >>> resampleList cell >>> arr (fromList >>> fromJust)

-- | Execute the cell for as many steps as the input list is long.
resampleList :: Monad m => Cell m a b -> Cell m [a] [b]
resampleList = hoistCellKleisli morph
  where
    morph _ s [] = return ([], s)
    morph singleStep s (a : as) = do
      (!b , s' ) <- singleStep s a
      (!bs, s'') <- morph singleStep s' as
      return (b : bs, s'')

resampleMaybe :: Monad m => Cell m a b -> Cell m (Maybe a) (Maybe b)
resampleMaybe cell = arr maybeToList >>> resampleList cell >>> arr listToMaybe

-- | Create as many cells as the input list is long and execute them in parallel 
-- (in the sense that each one has a separate state). At each tick the list with
-- the different states grows or shrinks depending on the size of the input list.
--
-- Similar to Yampa's [parC](https://hackage.haskell.org/package/Yampa-0.13.3/docs/FRP-Yampa-Switches.html#v:parC).
resampleListPar :: Monad m => Cell m a b -> Cell m [a] [b]
resampleListPar (Cell initial step) = Cell cellState' cellStep' where
    cellState' = []
    cellStep' s xs = Prelude.unzip <$> traverse (uncurry step) (Prelude.zip s' xs)
        where
            s' = s Prelude.++ Prelude.replicate (Prelude.length xs - Prelude.length s) initial
resampleListPar (ArrM f) = ArrM (traverse f)
