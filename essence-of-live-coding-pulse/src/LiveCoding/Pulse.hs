{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Arrows #-}
module LiveCoding.Pulse where

-- base
import Control.Arrow as X
import Control.Concurrent
import Control.Monad (void, forever)
import Control.Monad.Fix

-- transformers
import Control.Monad.Trans.Reader

-- pulse-simple
import Sound.Pulse.Simple

-- essence-of-live-coding
import LiveCoding
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Maybe (fromMaybe)

type PulseCell a b = Cell IO a (Float, b)

sampleRate :: Num a => a
sampleRate = 48000

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

pulseWrapC :: Int -> PulseCell a () -> Cell (HandlingStateT IO) a ()
pulseWrapC bufferSize cell = proc a -> do
  simple <- handling pulseHandle -< ()
  -- inSepThread calcAndPushSamples -< (simple, a)
  -- FIXME It remains to test whether sound actually works that way
  -- FIXME Also try only StrictData
  liftCell calcAndPushSamples -< (simple, a)
    where
      calcAndPushSamples = proc (simple, a) -> do
        samplesAndBs <- resampleList cell -< replicate bufferSize a
        let (samples, bs) = unzip samplesAndBs
        arrM $ uncurry simpleWrite -< (simple, samples)

inSepThread :: Cell IO a () -> Cell (HandlingStateT IO) a ()
inSepThread Cell { .. } = proc a -> do
  resultVar <- handling $ newMVarHandle Nothing -< ()
  liftCell Cell { cellStep = backgroundStep, cellState = cellState } -< (resultVar, a)
    where
      backgroundStep s (resultVar, a) = do
        s' <- fromMaybe s <$> takeMVar resultVar
        forkIO $ putMVar resultVar =<< (Just . snd) <$> cellStep s a
        return ((), s')
inSepThread notACell = inSepThread $ toCell notACell

-- Returns the sum between -1 and 1
wrapSum :: (Monad m, Data a, RealFloat a) => Cell m a a
wrapSum = Cell
  { cellState = 0
  , cellStep  = \accum a ->
    let
        (_, accum')  = properFraction $ accum + a
    in return (accum', accum')
  }

wrapIntegral :: (Monad m, Data a, RealFloat a) => Cell m a a
wrapIntegral = arr (/ sampleRate) >>> wrapSum

modSum :: (Monad m, Data a, Integral a) => a -> Cell m a a
modSum denominator = Cell
  { cellState = 0
  , cellStep  = \accum a -> let accum' = (accum + a) `mod` denominator in return (accum', accum')
  }


clamp :: (Ord a, Num a) => a -> a -> a -> a
clamp lower upper a = min upper $ max lower a

osc :: (Data a, RealFloat a, MonadFix m) => Cell (ReaderT a m) () a
osc = proc _ -> do
  f <- constM ask -< ()
  phase <- wrapSum -< f / 48000
  returnA -< sin $ 2 * pi * phase

osc' :: (Data a, RealFloat a, MonadFix m) => Cell m a a
osc' = proc a -> do
  runReaderC' osc -< (a, ())

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

f :: Note -> Float
f note = 220 * (2 ** (fromIntegral (fromEnum note) / 12))

o :: Float -> Float
o = (* 2)
