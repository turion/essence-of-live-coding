{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module LiveCoding.Gloss (
  module X,
  module LiveCoding.Gloss,
) where

-- base
import Control.Concurrent
import Control.Monad (when)
import Data.IORef
import System.Exit (exitSuccess)

-- transformers
import Control.Arrow (returnA)
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.Writer

-- gloss
import Graphics.Gloss as X
import Graphics.Gloss.Interface.IO.Game as X
import qualified Graphics.UI.GLUT as GLUT

-- essence-of-live-coding
import LiveCoding

-- essence-of-live-coding-gloss
import LiveCoding.Gloss.Debugger as X
import LiveCoding.Gloss.PictureM as X

{- | In a 'Handle', store a separate thread where the gloss main loop is executed,
   and several concurrent variables to communicate with it.
-}
data GlossHandle = GlossHandle
  { glossThread :: ThreadId
  , glossVars :: GlossVars
  }

-- | The concurrent variables needed to communicate with the gloss thread.
data GlossVars = GlossVars
  { glossEventsRef :: IORef [Event]
  -- ^ Stores all 'Event's that arrived since the last tick
  , glossPicRef :: IORef Picture
  -- ^ Stores the next 'Picture' to be painted
  , glossDTimeVar :: MVar Float
  -- ^ Stores the time passed since the last tick
  , glossExitRef :: IORef Bool
  -- ^ Write 'True' here to stop the gloss thread
  }

{- | Collect all settings that the @gloss@ backend requires.
   Taken from @rhine-gloss@.
-}
data GlossSettings = GlossSettings
  { displaySetting :: Display
  -- ^ Display mode (e.g. 'InWindow' or 'FullScreen').
  , backgroundColor :: Color
  -- ^ Background color.
  , stepsPerSecond :: Int
  -- ^ Number of simulation steps per second of real time.
  , debugEvents :: Bool
  -- ^ Print all incoming events to the console.
  }

defaultSettings :: GlossSettings
defaultSettings =
  GlossSettings
    { displaySetting = InWindow "Essence of live coding" (600, 800) (20, 20)
    , backgroundColor = black
    , stepsPerSecond = 30
    , debugEvents = False
    }

{- | Will create a handle for communication with the gloss thread,
   and start gloss.
-}
glossHandle :: GlossSettings -> Handle IO GlossHandle
glossHandle GlossSettings {..} =
  Handle
    { create = do
        glossEventsRef <- newIORef []
        glossDTimeVar <- newEmptyMVar
        glossPicRef <- newIORef blank
        glossExitRef <- newIORef False
        let glossVars = GlossVars {..}
        glossThread <-
          forkIO $
            playIO displaySetting backgroundColor stepsPerSecond glossVars getPicture (handleEvent debugEvents) stepGloss
        return GlossHandle {..}
    , destroy = \GlossHandle {glossVars = GlossVars {..}, ..} -> writeIORef glossExitRef True
    }

getPicture :: GlossVars -> IO Picture
getPicture GlossVars {..} = readIORef glossPicRef

handleEvent :: Bool -> Event -> GlossVars -> IO GlossVars
handleEvent debugEvents event vars@GlossVars {..} = do
  when debugEvents $ print event
  modifyIORef glossEventsRef (event :)
  return vars

stepGloss :: Float -> GlossVars -> IO GlossVars
stepGloss dTime vars@GlossVars {..} = do
  putMVar glossDTimeVar dTime
  exitNow <- readIORef glossExitRef
  when exitNow $ GLUT.leaveMainLoop
  -- when exitNow $ GLUT.leaveMainLoop >> exitSuccess
  return vars

{- | Given a cell in the gloss monad 'PictureM',
start the gloss backend and connect the cell to it.

This introduces 'Handle's containing the gloss background thread,
which need to be taken care of by calling 'runHandlingState'
or a similar function.

The resulting cell never blocks,
but returns 'Nothing' if there currently is no gloss tick.
-}
glossWrapC ::
  GlossSettings ->
  Cell PictureM a b ->
  Cell (HandlingStateT IO) a (Maybe b)
glossWrapC glossSettings cell = proc a -> do
  GlossHandle {..} <- handling $ glossHandle glossSettings -< ()
  liftCell pump -< (glossVars, a)
  where
    pump = proc (GlossVars {..}, a) -> do
      timeMaybe <- arrM tryTakeMVar -< glossDTimeVar
      case timeMaybe of
        Just _ -> do
          events <- arrM $ flip atomicModifyIORef ([],) -< glossEventsRef
          (picture, b) <- runPictureT cell -< (events, a)
          arrM (uncurry writeIORef) -< (glossPicRef, picture)
          returnA -< Just b
        Nothing -> do
          arrM threadDelay -< 1000 -- Prevent too much CPU load
          returnA -< Nothing
