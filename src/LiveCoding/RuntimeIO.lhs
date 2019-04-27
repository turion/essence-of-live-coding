\begin{comment}
\begin{code}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module LiveCoding.RuntimeIO where

-- base
import Control.Arrow
import Control.Monad
import Control.Concurrent
import Data.Data

-- essenceoflivecoding
import LiveCoding.LiveProgram
import LiveCoding.Debugger
import LiveCoding.Migrate

\end{code}
\end{comment}
\begin{code}
-- type LiveProg = Cell IO () ()
type Debugger = forall s . Data s => s -> IO ()

noDebugger = const $ return ()

launch :: LiveProgram IO -> IO (MVar (LiveProgram IO))
launch liveProg = do
  var <- newMVar liveProg
  forkIO $ background var
  return var

debug :: Debugger -> LiveProgram IO -> IO ()
debug debugger (LiveProgram state _) = debugger state

stepProgram :: LiveProgram IO -> IO (LiveProgram IO)
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
-- background var = backgroundWithDebugger var $ stateShow >>> putStrLn
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
\end{code}
