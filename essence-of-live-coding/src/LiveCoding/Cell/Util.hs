{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
module LiveCoding.Cell.Util where

-- base
import Control.Arrow
import Data.Data (Data)

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Cell.Feedback

-- | Sum all past inputs, starting by the given number
sumFrom :: Monad m => Integer -> Cell m Integer Integer
sumFrom n0 = feedback n0 $ proc (n, acc) -> returnA -< (acc, acc + n)

-- | Count the number of ticks, starting at 0
count :: Monad m => Cell m a Integer
count = arr (const 1) >>> sumC

-- | Accumulate all incoming data,
--   using the given fold function and start value.
--   For example, if @'foldC' f b@ receives inputs @a0@, @a1@,...
--   it will output @b@, @f a0 b@, @f a1 $ f a0 b@, and so on.
foldC :: (Data b, Monad m) => (a -> b -> b) -> b -> Cell m a b
foldC step cellState = Cell { .. }
  where
    cellStep b a = let b' = step a b in return (b, b')
