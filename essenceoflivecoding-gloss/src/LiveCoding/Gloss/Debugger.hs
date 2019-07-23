{-# LANGUAGE Arrows #-}
module LiveCoding.Gloss.Debugger where

-- base
import Control.Arrow
import Data.Data

-- transformers
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

-- syb
import Data.Generics.Text

-- gloss
import Graphics.Gloss

-- essenceoflivecoding
import LiveCoding

-- essenceoflivecoding-gloss
import LiveCoding.Gloss.PictureM

statePicture :: Data s => s -> Picture
statePicture = translate (-100) 100 . scale 0.1 0.1 . color red . text . stateShow

statePlay :: Debugger PictureM
statePlay = Debugger $ liveCell $ every 100 >>> keep blank >>> arrM (lift . tell)

every :: Data s => Integer -> Cell (StateT s PictureM) () (Maybe Picture)
every maxN = proc () -> do
  n <- sumC -< 1
  if n `mod` maxN == 0
  then do
    s <- getC -< ()
    let pic = statePicture s
    returnA -< Just pic
  else
    returnA  -< Nothing
