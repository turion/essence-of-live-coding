{-# LANGUAGE Arrows #-}
module LiveCoding.Gloss.Debugger where

-- base
import Control.Arrow
import Data.Data

-- transformers
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

-- syb
import Data.Generics.Text

-- gloss
import Graphics.Gloss

-- essence-of-live-coding
import LiveCoding

-- essence-of-live-coding-gloss
import LiveCoding.Gloss.PictureM

statePicture :: Data s => s -> Picture
statePicture = translate (-100) 200 . scale 0.2 0.2 . color red . text . stateShow

statePlay :: Debugger PictureM
statePlay = Debugger $ liveCell $ every 2 >>> keep blank >>> arrM (lift . tell)

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
