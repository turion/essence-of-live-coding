{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module LiveCoding.RuntimeIO.Launch where

-- base
import Control.Concurrent
import Control.Monad
import Data.Data

-- transformers
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except

-- essence-of-live-coding
import LiveCoding.Debugger
import LiveCoding.Handle
import LiveCoding.LiveProgram
import LiveCoding.LiveProgram.Except
import LiveCoding.LiveProgram.HotCodeSwap
import LiveCoding.Cell.Monad.Trans
import LiveCoding.Exceptions.Finite (Finite)
import LiveCoding.HandlingState

{- | Monads in which live programs can be launched in 'IO',
for example when you have special effects that have to be handled on every reload.

The only thing necessary is to transform the 'LiveProgram'
into one in the 'IO' monad, and the rest is taken care of in the framework.
-}
class Monad m => Launchable m where
  runIO :: LiveProgram m -> LiveProgram IO

instance Launchable IO where
  runIO = id

instance (Typeable m, Launchable m) => Launchable (StateT (HandlingState m) m) where
  runIO = runIO . runHandlingState

-- | Upon an exception, the program is restarted.
--   To handle or log the exception, see "LiveCoding.LiveProgram.Except".
instance (Data e, Finite e, Launchable m) => Launchable (ExceptT e m) where
  runIO liveProgram = runIO $ foreverCLiveProgram $ try liveProgram

{- | The standard top level @main@ for a live program.

Typically, you will define a top level 'LiveProgram' in some monad like @'HandlingStateT' 'IO'@,
and then add these two lines of boiler plate:

@
main :: IO ()
main = liveMain liveProgram
@
-}
liveMain
  :: Launchable m
  => LiveProgram m
  -> IO ()
liveMain = foreground . runIO

-- | Launch a 'LiveProgram' in the foreground thread (blocking).
foreground :: Monad m => LiveProgram m -> m ()
foreground liveProgram
  =   stepProgram liveProgram
  >>= foreground

-- | A launched 'LiveProgram' and the thread in which it is running.
data LaunchedProgram (m :: * -> *) = LaunchedProgram
  { programVar :: MVar (LiveProgram IO)
  , threadId   :: ThreadId
  }

{- | Launch a 'LiveProgram' in a separate thread.

The 'MVar' can be used to 'update' the program while automatically migrating it.
The 'ThreadId' represents the thread where the program runs in.
You're advised not to kill it directly, but to run 'stop' instead.
-}
launch
  :: Launchable m
  => LiveProgram m
  -> IO (LaunchedProgram m)
launch liveProg = do
  programVar <- newMVar $ runIO liveProg
  threadId <- forkIO $ background programVar
  return LaunchedProgram { .. }

-- | Migrate (using 'hotCodeSwap') the 'LiveProgram' to a new version.
update
  :: Launchable m
  => LaunchedProgram m
  -> LiveProgram     m
  -> IO ()
update LaunchedProgram { .. } newProg = modifyMVarMasked_ programVar
  $ return . hotCodeSwap (runIO newProg)

{- | Stops a thread where a 'LiveProgram' is being executed.

Before the thread is killed, an empty program (in the monad @m@) is first inserted and stepped.
This can be used to call cleanup actions encoded in the monad,
such as 'HandlingStateT'.
-}
stop
  :: Launchable m
  => LaunchedProgram m
  -> IO ()
stop launchedProgram@LaunchedProgram { .. } = do
  update launchedProgram mempty
  stepLaunchedProgram launchedProgram
  killThread threadId

-- | Launch a 'LiveProgram', but first attach a debugger to it.
launchWithDebugger
  :: (Monad m, Launchable m)
  => LiveProgram m
  -> Debugger m
  -> IO (LaunchedProgram m)
launchWithDebugger liveProg debugger = launch $ liveProg `withDebugger` debugger

-- | This is the background task executed by 'launch'.
background :: MVar (LiveProgram IO) -> IO ()
background var = forever $ do
  liveProg  <- takeMVar var
  liveProg' <- stepProgram liveProg
  putMVar var liveProg'

-- | Advance a 'LiveProgram' by a single step.
stepProgram :: Monad m => LiveProgram m -> m (LiveProgram m)
stepProgram LiveProgram {..} = do
  liveState' <- liveStep liveState
  return LiveProgram { liveState = liveState', .. }

-- | Advance a launched 'LiveProgram' by a single step and store the result.
stepLaunchedProgram
  :: (Monad m, Launchable m)
  => LaunchedProgram m
  -> IO ()
stepLaunchedProgram LaunchedProgram { .. } = modifyMVarMasked_ programVar stepProgram
