{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
module Main where

-- base
import Control.Arrow as X
import Control.Concurrent
import Control.Monad (void)
import Control.Monad.Fix
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)

import Prelude hiding ((!))

-- transformers
import Control.Monad.Trans.Reader

-- vector
import qualified Data.Vector as Vector
import Data.Vector ((!))

-- pulse-simple
import Sound.Pulse.Simple

-- essence-of-live-coding
import LiveCoding

-- essence-of-live-coding-pulse
import LiveCoding.Pulse

fastSine :: (Data a, Floating a, MonadFix m) => Cell (ReaderT a m) () a
fastSine = proc _ -> do
  t <- constM ask -< ()
  rec
    let acc = - (2 * pi / t) ^ 2 * pos
    vel <- sumC' <<< delay 0 -< acc
    pos <- arr (+ 1) <<< sumC' -< vel
  returnA -< pos

sumC' :: (Monad m, Data a, Num a) => Cell m a a
sumC' = Cell
  { cellState = 0
  , cellStep  = \accum a -> let accum' = accum + a in return (accum', accum')
  }

cellA :: MonadFix m => Cell m () Float
cellA = runReaderC (44100 / 440) fastSine

cellB :: MonadFix m => Cell m () Float
cellB = proc _ -> do
  fDelta <- runReaderC 10 osc -< ()
  runReaderC' osc -< (440 + 5 * fDelta, ())

short :: Monad m => Float -> CellExcept m () Float ()
short frequency = try $ proc _ -> do
  count <- sumC -< 1 :: Int
  if count > 8000
    then throwC -< ()
    else returnA -< frequency

frequencies :: Monad m => Cell m () Float
frequencies = foreverC $ runCellExcept $ sequence $ (short . f) <$> [C, E, G]

cycleThrough :: Monad m => NonEmpty a -> Int -> Cell m () a
cycleThrough bs cycleLength = let vector = Vector.fromList (NonEmpty.toList bs) in proc _ -> do
  n <- modSum (cycleLength * length bs) -< 1
  returnA -< vector ! (n `div` cycleLength)

frequencies' = cycleThrough (NonEmpty.fromList $ [f D, f G, o $ f Bb]) 8000

pulseCell :: PulseCell () ()
pulseCell = frequencies' >>> osc' >>> arr (, ())

liveProgram :: LiveProgram (HandlingStateT IO)
liveProgram = liveCell $ pulseWrapC 1024 pulseCell >>> arr (const ())

main :: IO ()
main = runHandlingStateT $ foreground liveProgram
