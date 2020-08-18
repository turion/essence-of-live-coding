{-# LANGUAGE Arrows #-}

-- base
import Control.Arrow
import Control.Concurrent
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.IORef
import Data.Maybe (fromMaybe)

-- time
import Data.Time.Clock

-- transformers
import Control.Monad.Trans.Class (MonadTrans(lift))

-- essence-of-live-coding
import LiveCoding

-- essence-of-live-coding-gloss
import LiveCoding.Gloss

-- essence-of-live-coding-pulse
import LiveCoding.Pulse

glossCell :: Cell PictureM () Float
glossCell = withDebuggerC glossCell' statePlay

glossCell' :: Cell PictureM () Float
glossCell' = proc () -> do
  -- hoistCell liftIO $ printFPS "glossCell: " -< ()
  gearAngle <- integrate -< 30
  addPicture -< gear gearAngle
  phase <- integrate -< 5
  addPicture -< rotate gearAngle $ blinker phase
  returnA -< gearAngle

-- Note that ghcid and cabal repl have very different performance! But only sometimes. Sometimes cabal repl is good or bad as well.
-- Is it that disk IO simply interrupts everything?

blinker :: Float -> Picture
blinker phase
  = translate 0 100
  $ color (greyN $ 0.7 + 0.2 * (sin phase))
  $ thickCircle 5 10

gear :: Float -> Picture
gear angle = scale 3 3 $ rotate angle $ pictures
  [ color (dim green) $ pictures $
    thickCircle 20 30
    : [rotate blockAngle block | blockAngle <- [0, 45 .. 315]]
  , color red $ pictures [ wedge, scale (-1) 1 wedge ]
  ]
  where
    block = rectangleSolid 85 15
    wedge = polygon
      [ ( 0,  -5)
      , ( 0,  20)
      , (10, -10)
      ]

tones = [A, C, E]

pulseCell :: PulseCell IO Float ()
pulseCell = proc angle -> do
  pulse <- osc' -< cycleTones $ round angle
  addSample     -< atan pulse ** 3
  returnA       -< ()

cycleTones :: Int -> Float
cycleTones angle = f
  $ (tones !!)
  $ (`mod` length tones)
  $ angle `div` (60 `div` length tones)

getAngleEvery :: Int -> Cell IO (IORef Float) Int
getAngleEvery maxCount = proc ref -> do
  count <- sumC   -< 1
  mAngle <- if count `mod` maxCount == 0
    then arr Just <<< arrM readIORef -< ref
    else returnA                     -< Nothing
  angle <- keep 0 -< mAngle
  returnA         -< round angle

-- | Output an estimate of the number of ticks per second
fps :: Cell IO () Float
fps = Cell
  { cellState = Nothing
  , cellStep = \lastTimestampMaybe () -> do
      now <- getCurrentTime
      let last = fromMaybe now lastTimestampMaybe
      return (1 / realToFrac (now `diffUTCTime` last), Just now)
  }

printFPS :: MonadIO m => String -> Cell m () ()
printFPS msg = hoistCell liftIO fps >>> arr (show >>> (msg ++)) >>> arrM (liftIO . putStrLn)

printTime :: MonadIO m => String -> Cell m () ()
printTime msg = constM $ liftIO $ putStrLn =<< ((msg ++) . show) <$> getCurrentTime

liveProgram :: LiveProgram (HandlingStateT IO)
liveProgram = liveCell mainCell

mainCell :: Cell (HandlingStateT IO) () ()
mainCell = proc () -> do
  -- printTime "mainCell      : " -< ()
  phaseMaybe <- glossWrapC defaultSettings glossCell -< ()
  phase <- keep 0 -< phaseMaybe
  pulseWrapC 1500 pulseCell -< phase
  -- printTime "mainCell pulse: " -< ()
  -- arrM $ liftIO . print -< (p, g)
  -- printFPS "mainCell" -< ()
  -- printTime "mainCell gloss: " -< ()
  -- arrM $ lift . threadDelay -< 100 -- TODO Tweak for better performance
  returnA -< ()

main :: IO ()
main = runHandlingStateT $ foreground liveProgram
