{-# LANGUAGE Arrows #-}
module LiveCoding.Cell.Util where

-- base
import Control.Arrow

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Cell.Feedback

-- | Sum all past inputs, starting by the given number
sumFrom :: Monad m => Integer -> Cell m Integer Integer
sumFrom n0 = feedback n0 $ proc (n, acc) -> returnA -< (acc, acc + n)

-- | Count the number of ticks, starting at 0
count :: Monad m => Cell m a Integer
count = arr (const 1) >>> sumC
