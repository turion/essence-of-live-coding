{-# LANGUAGE Arrows #-}

module LiveCoding.Gloss
  ( module X
  , module LiveCoding.Gloss
  ) where

-- base
import Control.Concurrent
import Data.IORef

-- transformers
import Control.Monad.Trans.Writer

-- gloss
import Graphics.Gloss as X
import Graphics.Gloss.Interface.IO.Game

-- essence-of-live-coding
import LiveCoding

-- essence-of-live-coding-gloss
import LiveCoding.Gloss.Debugger as X
import LiveCoding.Gloss.PictureM as X

type GlossCellWorldForeground = (GlossCell, [Event], Picture)

playCellForeground :: GlossCell -> IO ()
playCellForeground cell = playIO (InWindow "Gears" (600, 800) (20, 20)) black stepRate (initialWorld cell) toPicture handleEvent playStep

type GlossCellWorld = (MVar GlossCell, [Event], Picture)

-- TODO Abstract external main loops
playCell :: GlossCell -> IO (MVar GlossCell)
playCell glossCell = do
  var <- newMVar glossCell
  forkIO $ playIO (InWindow "Gears" (600, 800) (20, 20)) black stepRate (initialWorld var) toPicture handleEvent playStepMVar
  return var

-- TODO Of course these are general for cells
updateGloss :: MVar GlossCell -> GlossCell -> IO ()
updateGloss var newGlossCell = do
  oldGlossCell <- takeMVar var
  putMVar var $ hotCodeSwapCell newGlossCell oldGlossCell

initialWorld cell = (cell, [], blank)
toPicture (_, _, picture) = do
  --putStrLn "toPicture"
  threadDelay 10000
  return picture
handleEvent event (cell, events, picture) = do
  --putStrLn "handleEvent"
  threadDelay 10000
  return (cell, event : events, picture)
playStep _ (cell, events, _) = do
  (picture, cell') <- fmap massageWriterOutput $ runWriterT $ step cell events
  threadDelay 10000
  --putStrLn "playStep"
  return (cell', [], picture)
playStepMVar _ (var, events, _) = do
  cell <- takeMVar var
  (picture, cell') <- fmap massageWriterOutput $ runWriterT $ step cell events
  putMVar var cell'
  threadDelay 10000
  --putStrLn "playStepMVar"
  return (var, [], picture)

glossWrap :: GlossCell -> IO (LiveProgram IO)
glossWrap cell = do
  pictureVar <- newMVar blank
  eventRef <- newIORef []
  stepVar <- newMVar 0
  let
    getPicture () = takeMVar pictureVar
    putEvent event () = modifyIORef eventRef $ (event :)
    putStep _ () = putMVar stepVar $ 1 / stepRate
  forkIO $ playIO (InWindow "Gears" (600, 800) (20, 20)) black stepRate () getPicture putEvent putStep
  let
    putPicture = putMVar pictureVar
    getEvents = atomicModifyIORef eventRef $ \events -> ([], events)
    getStep = takeMVar stepVar
  return $ liveCell $ proc _ -> do
    _       <- constM getStep            -< ()
    events  <- constM getEvents          -< ()
    picture <- runPictureM cell -< events
    arrM putPicture                            -< picture
