\begin{comment}
\begin{code}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module LiveCoding.RuntimeIO where

-- base
import Control.Arrow
import Control.Monad
import Control.Concurrent
import Data.Data
import Data.IORef

-- essenceoflivecoding
import LiveCoding.LiveProgram
import LiveCoding.Debugger
import LiveCoding.Migrate

\end{code}
\end{comment}
\begin{code}

launch :: LiveProgram IO -> IO (MVar (LiveProgram IO))
launch liveProg = do
  var <- newMVar liveProg
  forkIO $ background var
  return var

launchWithDebugger :: LiveProgram IO -> Debugger -> IO (MVar (LiveProgram IO))
launchWithDebugger liveProg debugger = do
  var <- newMVar liveProg
  forkIO $ backgroundWithDebugger var debugger
  return var

debug :: Debugger -> LiveProgram IO -> IO ()
debug Debugger { .. } (LiveProgram state _) = debugState state

newtype CountObserver = CountObserver { observe :: IO Integer }
countDebugger :: IO (Debugger, CountObserver)
countDebugger = do
  countRef <- newIORef 0
  observeVar <- newEmptyMVar
  let debugger = Debugger $ const $ do
        n <- readIORef countRef
        putMVar observeVar n
        yield
        void $ takeMVar observeVar
        writeIORef countRef $ n + 1
      observer = CountObserver $ yield >> readMVar observeVar
  return (debugger, observer)

await :: CountObserver -> Integer -> IO ()
await CountObserver { .. } nMax = go
 where
  go = do
    n <- observe
    if n > nMax then return () else go

stepProgram :: Monad m => LiveProgram m -> m (LiveProgram m)
stepProgram LiveProgram {..} = do
  liveState' <- liveStep liveState
  return LiveProgram { liveState = liveState', .. }

backgroundWithDebugger :: MVar (LiveProgram IO) -> Debugger -> IO ()
backgroundWithDebugger var debugger = forever $ do
  liveProg <- takeMVar var
  liveProg' <- stepProgram liveProg
  debug debugger liveProg'
  combine var liveProg'

background :: MVar (LiveProgram IO) -> IO ()
background var = backgroundWithDebugger var noDebugger

combine :: MVar (LiveProgram IO) -> LiveProgram IO -> IO ()
combine var prog = do
  success <- tryPutMVar var prog
  unless success $ do
    newProg <- takeMVar var
    combine var $ combineLiveProgram prog newProg

combineLiveProgram :: LiveProgram m -> LiveProgram m -> LiveProgram m
combineLiveProgram (LiveProgram oldState oldStep) (LiveProgram newState newStep) = LiveProgram (newState `migrate` oldState) newStep

update :: MVar (LiveProgram IO) -> LiveProgram IO -> IO ()
update var liveProg = void $ forkIO $ putMVar var liveProg

foreground :: Monad m => LiveProgram m -> m ()
foreground liveProgram = stepProgram liveProgram >>= foreground
\end{code}
