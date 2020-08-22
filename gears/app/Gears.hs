{-# LANGUAGE Arrows #-}

-- base
import Control.Arrow
import Control.Monad (void)
import Control.Monad.IO.Class

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
  gearAngle <- integrate -< 30
  addPicture             -< gear gearAngle
  phase     <- integrate -< 5
  addPicture             -< rotate gearAngle $ blinker phase
  returnA                -< gearAngle

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

pulseCell :: PulseCell IO Float ()
pulseCell = proc angle -> do
  pulse <- sawtooth -< cycleTones $ round angle
  addSample         -< pulse

cycleTones :: Int -> Float
cycleTones angle = f
  $ (tones !!)
  $ (`mod` length tones)
  $ angle `div` (60 `div` length tones)

liveProgram :: LiveProgram (HandlingStateT IO)
liveProgram = liveCell mainCell

mainCell :: Cell (HandlingStateT IO) () ()
mainCell = proc () -> do
  angleMaybe <- glossWrapC defaultSettings glossCell -< ()
  angle <- keep 0                                    -< angleMaybe
  pulseWrapC 800 pulseCell                           -< angle
  returnA                                            -< ()

main :: IO ()
main = runHandlingStateT $ foreground liveProgram
