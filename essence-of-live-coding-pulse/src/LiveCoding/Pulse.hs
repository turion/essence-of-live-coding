{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
module LiveCoding.Pulse where

-- base
import Control.Arrow as X
import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.Fix
import Data.Monoid (getSum, Sum(Sum))
import Control.Monad.IO.Class

-- transformers
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Strict

-- has-transformers
import Control.Monad.Trans.Has
import Control.Monad.Trans.Has.State

-- transformers-base
import Control.Monad.Base

-- pulse-simple
import Sound.Pulse.Simple

-- essence-of-live-coding
import LiveCoding
import LiveCoding.HandlingState (HasHandlingState)

type PulseT m = WriterT (Sum Float) m

type PulseCell m a b = Cell (PulseT m) a b

-- | Compose with this cell to play a sound sample.
addSample :: Monad m => PulseCell m Float ()
addSample = arr Sum >>> arrM tell

-- | Globally fix the sample rate to 48000 samples per second.
sampleRate :: Num a => a
sampleRate = 48000

{- | Create a pulse server backend handle.

Currently, this is always mono,
but with a future release of @pulse-simple@,
this might be configurable.
-}
pulseHandle :: Handle IO Simple
pulseHandle = Handle
  { create = simpleNew
      Nothing
      "example"
      Play
      Nothing
      "this is an example application"
      (SampleSpec (F32 LittleEndian) sampleRate 1)
      Nothing
      Nothing
  , destroy = simpleFree
  }

{- | Run a 'PulseCell' with a started pulse backend.

Currently, this is synchronous and blocking,
i.e. the resulting cell will block until the backend buffer is nearly empty.

This performs several steps of your cell at a time,
replicating the input so many times.
-}
pulseWrapC
  :: 
  (MonadBase IO m, HasHandlingState IO m) =>
  Int
  -- ^ Specifies how many steps of your 'PulseCell' should be performed in one step of 'pulseWrapC'.
  -> PulseCell IO a b
  -- ^ Your cell that produces samples.
  -> Cell m a [b]
pulseWrapC bufferSize cell = proc a -> do
  simple <- handling pulseHandle -< ()
  samplesAndBs <- resampleList $ hoistCell liftBase $ runWriterC cell -< replicate bufferSize a
  let (samples, bs) = unzip samplesAndBs
      samples' = getSum <$> samples
  arrM $ liftBase . uncurry simpleWrite -< samples' `seq` bs `seq` (simple, samples')
  returnA -< bs

{- | Returns the sum of all incoming values,
and wraps it between -1 and 1.

This is to prevent floating number imprecision when the sum gets too large.
-}
wrapSum :: (Monad m, Data a, RealFloat a) => Cell m a a
wrapSum = Cell
  { cellState = 0
  , cellStep  = \accum a ->
    let
        (_, accum') = properFraction $ accum + a
    in return (accum', accum')
  }

-- | Like 'wrapSum', but as an integral, assuming the PulseAudio 'sampleRate'.
wrapIntegral :: (Monad m, Data a, RealFloat a) => Cell m a a
wrapIntegral = arr (/ sampleRate) >>> wrapSum

-- | A sawtooth, or triangle wave, generator,
--   outputting a sawtooth wave with the given input as frequency.
sawtooth :: (Monad m, Data a, RealFloat a) => Cell m a a
sawtooth = wrapIntegral

modSum :: (Monad m, Data a, Integral a) => a -> Cell m a a
modSum denominator = Cell
  { cellState = 0
  , cellStep  = \accum a -> let accum' = (accum + a) `mod` denominator in return (accum', accum')
  }

clamp :: (Ord a, Num a) => a -> a -> a -> a
clamp lower upper a = min upper $ max lower a

-- | A sine oscillator.
--   Supply the frequency via the 'ReaderT' environment.
--   See 'osc'' and 'oscAt'.
osc :: (Data a, RealFloat a, Monad m) => Cell (ReaderT a m) () a
osc = proc _ -> do
  f <- constM ask -< ()
  phase <- wrapIntegral -< f
  returnA -< sin $ 2 * pi * phase

-- | A sine oscillator, at a fixed frequency.
oscAt :: (Data a, RealFloat a, Monad m) => a -> Cell m () a
oscAt = flip runReaderC osc

-- | A sine oscillator, at a frequency that can be specified live.
osc' :: (Data a, RealFloat a, Monad m) => Cell m a a
osc' = proc a -> do
  runReaderC' osc -< (a, ())

{- | A basic musical note (western traditional notation, german nomenclature).

Assumes equal temperament and removes enharmonic equivalents,
i.e. there is only Dis (= D sharp) but not Eb (= E flat).
-}
data Note
  = A
  | Bb
  | B
  | C
  | Cis
  | D
  | Dis
  | E
  | F
  | Fis
  | G
  | Gis
  deriving (Enum, Show)

-- | Calculate the frequency of a note,
--   with 'A' corresponding to 220 Hz.
f :: Note -> Float
f note = 220 * (2 ** (fromIntegral (fromEnum note) / 12))

-- | Transpose a frequency an octave higher, i.e. multiply by 2.
o :: Float -> Float
o = (* 2)

-- | Transpose a frequency an octave lower, i.e. divide by 2.
oB :: Float -> Float
oB = (/ 2)
