{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Arrows #-}

module LiveCoding.Gloss
  ( module X
  , module LiveCoding.Gloss
  ) where

-- base
import Control.Concurrent
import Control.Monad (when)
import Data.IORef
import System.Exit (exitSuccess)

-- transformers
import Control.Arrow (returnA)
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State.Strict (StateT)

-- gloss
import Graphics.Gloss as X
import Graphics.Gloss.Interface.IO.Game as X

-- essence-of-live-coding
import LiveCoding

-- essence-of-live-coding-gloss
import LiveCoding.Gloss.Debugger as X
import LiveCoding.Gloss.PictureM as X

-- | In a 'Handle', store a separate thread where the gloss main loop is executed,
--   and several concurrent variables to communicate with it.
data GlossHandle = GlossHandle
  { glossThread :: ThreadId
  , glossVars   :: GlossVars
  }

-- | The concurrent variables needed to communicate with the gloss thread.
data GlossVars = GlossVars
  { glossEventsRef :: IORef [Event]
    -- ^ Stores all 'Event's that arrived since the last tick
  , glossPicRef    :: IORef Picture
    -- ^ Stores the next 'Picture' to be painted
  , glossDTimeVar  :: MVar Float
    -- ^ Stores the time passed since the last tick
  , glossExitRef   :: IORef Bool
    -- ^ Write 'True' here to stop the gloss thread
  }

-- | Collect all settings that the @gloss@ backend requires.
--   Taken from @rhine-gloss@.
data GlossSettings = GlossSettings
  { displaySetting  :: Display      -- ^ Display mode (e.g. 'InWindow' or 'FullScreen').
  , backgroundColor :: Color        -- ^ Background color.
  , stepsPerSecond  :: Int          -- ^ Number of simulation steps per second of real time.
  }

defaultSettings :: GlossSettings
defaultSettings = GlossSettings
  { displaySetting  = InWindow "Essence of live coding" (600, 800) (20, 20)
  , backgroundColor = black
  , stepsPerSecond  = 30
  }

-- | Will create a handle for communication with the gloss thread,
--   and start gloss.
glossHandle :: GlossSettings -> Handle IO GlossHandle
glossHandle GlossSettings { .. } = Handle
  { create = do
      glossEventsRef <- newIORef []
      glossDTimeVar <- newEmptyMVar
      glossPicRef <- newIORef blank
      glossExitRef <- newIORef False
      let glossVars = GlossVars { .. }
      glossThread <- forkOS
        $ playIO displaySetting backgroundColor stepsPerSecond glossVars getPicture handleEvent stepGloss
      return GlossHandle { .. }
  , destroy = \GlossHandle { glossVars = GlossVars { .. }, .. } -> writeIORef glossExitRef True
  }

getPicture :: GlossVars -> IO Picture
getPicture GlossVars { .. } = do
  threadDelay 10000
  readIORef glossPicRef

handleEvent :: Event -> GlossVars -> IO GlossVars
handleEvent event vars@GlossVars { .. } = do
  threadDelay 10000
  modifyIORef glossEventsRef (event :)
  return vars

stepGloss :: Float -> GlossVars -> IO GlossVars
stepGloss dTime vars@GlossVars { .. } = do
  threadDelay $ round $ dTime * 1000
  putMVar glossDTimeVar dTime
  exitNow <- readIORef glossExitRef
  when exitNow exitSuccess
  return vars

-- | Given a cell in the gloss monad 'PictureM',
--   start the gloss backend and connect the cell to it.
--   This introduces 'Handle's, which need to be taken care of by calling 'runHandlingState'
--   or a similar function.
glossWrapC :: GlossSettings -> Cell PictureM a b -> Cell (StateT (HandlingState IO) IO) a b
glossWrapC glossSettings cell = proc a -> do
  GlossHandle { .. } <- handling $ glossHandle glossSettings -< ()
  liftCell pump -< (glossVars, a)
  where
    pump = proc (GlossVars { .. }, a) -> do
      dTime  <- arrM takeMVar                         -< glossDTimeVar
      events <- arrM $ flip atomicModifyIORef' ([], ) -< glossEventsRef
      (picture, b) <- runPictureT cell                -< (events, a)
      arrM (uncurry writeIORef)                       -< (glossPicRef, picture)
      -- arrM threadDelay                                -< round $ 1000 * dTime -- TODO Tweak for better performance
      returnA                                         -< b
