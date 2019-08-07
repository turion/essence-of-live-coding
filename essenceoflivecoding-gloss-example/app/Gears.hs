{-# LANGUAGE Arrows #-}

-- base
import Control.Arrow
import Control.Monad.IO.Class
import Data.IORef

-- essenceoflivecoding
import LiveCoding

-- essenceoflivecoding-gloss
import LiveCoding.Gloss

-- essenceoflivecoding-pulse
import LiveCoding.Pulse

glossCell :: IORef Float -> GlossCell
glossCell ref = withDebuggerC (glossCell' ref) statePlay

glossCell' :: IORef Float -> GlossCell
glossCell' ref = proc _events -> do
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

tones = [C, E, G]

pulseCell :: IORef Float -> PulseCell
pulseCell ref = proc _ -> do
  angle <- getAngleEvery 1024 ref -< ()
  let frequency = f $ (tones !!)
        $ (`mod` length tones)
        $ angle `div` (360 `div` length tones)
  osc' -< frequency

getAngleEvery :: Int -> IORef Float -> Cell IO () Int
getAngleEvery maxCount ref = proc _ -> do
  count <- sumC -< 1
  mAngle <- if count `mod` maxCount == 0
    then arr Just <<< constM (readIORef ref) -< ()
    else returnA                             -< Nothing
  angle <- keep 0 -< mAngle
  returnA         -< round angle


main :: IO ()
main = do
  ref <- newIORef 0
  playCellForeground $ glossCell ref
