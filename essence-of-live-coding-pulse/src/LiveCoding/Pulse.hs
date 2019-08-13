{-# LANGUAGE Arrows #-}
module LiveCoding.Pulse where

-- base
import Control.Arrow as X
import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.Fix

-- transformers
import Control.Monad.Trans.Reader

-- pulse-simple
import Sound.Pulse.Simple

-- essence-of-live-coding
import LiveCoding

type PulseCell = Cell IO () Float

playPulseCell :: PulseCell -> IO (MVar PulseCell)
playPulseCell pulseCell = do
  var <- newMVar pulseCell
  pulseClient <- simpleNew
    Nothing
    "example"
    Play
    Nothing
    "this is an example application"
    (SampleSpec (F32 LittleEndian) 44100 1)
    Nothing
    Nothing
  forkIO $ forever $ do
    cell <- takeMVar var
    (samples, cell') <- steps cell $ replicate 1024 ()
    simpleWrite pulseClient samples
    putMVar var cell'
  return var

-- TODO Generalisable
updatePulse :: MVar PulseCell -> PulseCell -> IO ()
updatePulse var newCell = do
  oldCell <- takeMVar var
  putMVar var $ hotCodeSwapCell newCell oldCell

-- Returns the sum between -1 and 1
wrapSum :: (Monad m, Data a, RealFloat a) => Cell m a a
wrapSum = Cell
  { cellState = 0
  , cellStep  = \accum a ->
    let
        (_, accum')  = properFraction $ accum + a
    in return (accum', accum')
  }

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
  phase <- wrapSum -< f / 44100
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
