{-# LANGUAGE Arrows #-}

-- base
import Control.Arrow
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.IORef

-- essence-of-live-coding
import LiveCoding

-- essence-of-live-coding-gloss
import LiveCoding.Gloss

-- essence-of-live-coding-pulse
import LiveCoding.Pulse

glossProg :: IORef Float -> LiveProgram (HandlingStateT IO)
glossProg = liveCell . glossWrapC defaultSettings . glossCell

glossCell :: IORef Float -> Cell PictureM () ()
glossCell ref = withDebuggerC (glossCell' ref) statePlay

glossCell' :: IORef Float -> Cell PictureM () ()
glossCell' ref = proc () -> do
  gearAngle <- integrate         -< 30
  addPicture                     -< gear gearAngle
  arrM (liftIO . writeIORef ref) -< gearAngle
  phase     <- integrate         -< 5
  addPicture                     -< rotate gearAngle $ blinker phase

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

tones = [D, F, A]

pulseCell :: IORef Float -> PulseCell
pulseCell ref = proc _ -> do
  angle <- getAngleEvery 1024 ref -< ()
  osc'                            -< cycleTones angle

cycleTones :: Int -> Float
cycleTones angle = f
  $ (tones !!)
  $ (`mod` length tones)
  $ angle `div` (60 `div` length tones)

getAngleEvery :: Int -> IORef Float -> Cell IO () Int
getAngleEvery maxCount ref = proc _ -> do
  count <- sumC   -< 1
  mAngle <- if count `mod` maxCount == 0
    then arr Just <<< constM (readIORef ref) -< ()
    else returnA                             -< Nothing
  angle <- keep 0 -< mAngle
  returnA         -< round angle


main :: IO ()
main = do
  ref <- newIORef 0
  void $ playPulseCell $ pulseCell ref
  void $ runHandlingStateT $ foreground $ glossProg ref
  void $ getLine
