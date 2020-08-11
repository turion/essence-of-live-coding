{-# LANGUAGE Arrows #-}

-- base
import Data.Maybe (isJust)

-- yampa
import FRP.Yampa

sine k = proc () -> do
  rec
    let acc = - k * pos
    vel <- integral -< acc
    pos <- (+1) ^<< integral -< vel
  returnA -< pos

oscillator :: Double -> Double -> SF a Double
oscillator amp period = proc _ -> do
  rec
    let acc = - (2.0*pi/period)^(2 :: Int) * p
    v <- integral -< acc
    p <- (amp +) ^<< integral -< v
  returnA -< p

timestep :: Double
timestep = 1000

mainSF = proc _ -> do
  -- x <- sine (1 :: Double) -< ()
  x <- oscillator 1 10 -< ()
  Event s <- accumBy (+) 0 -< Event x
  -- s <- integral -< x
  -- Event c <- count -< Event ()
  c <- integral -< timestep
  if s `seq` c > 1000 * 1000
    then returnA -< Left s
    else returnA -< Right x

main = reactimate
  (return ())
  (const $ return (1/timestep, Just ()))
  --(const $ \b -> if isJust b then print b >> return True else return False)
  helper
  mainSF
  where
    helper _ (Left s) = print s >> return True
    helper _ (Right x) = return False
