{-# LANGUAGE Arrows #-}
--essenceoflivecoding
import LiveCoding

-- essenceoflivecoding-gloss
import LiveCoding.Gloss

glossCell :: GlossCell
glossCell = withDebuggerC glossCell' statePlay

glossCell' :: GlossCell
glossCell' = proc _events -> do
  gearAngle <- integrate -< 30
  addPicture             -< gear gearAngle
  phase     <- integrate -< 5
  addPicture             -< rotate gearAngle $ blinker phase

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

main :: IO ()
main = playCellForeground glossCell
