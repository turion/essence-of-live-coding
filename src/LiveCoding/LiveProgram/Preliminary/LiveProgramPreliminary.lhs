\begin{figure}
\begin{comment}
\begin{code}
{-# LANGUAGE RecordWildCards #-}

module LiveCoding.LiveProgram.Preliminary.LiveProgramPreliminary where

-- base
import Control.Concurrent
import Control.Monad (forever)

\end{code}
\end{comment}
\begin{code}
data LiveProgram m s = LiveProgram
  { liveState :: s
  , liveStep  :: s -> m s
  }

stepProgram
  :: Monad m
  => LiveProgram m s -> m (LiveProgram m s)
stepProgram liveProgram@LiveProgram { .. } = do
  liveState' <- liveStep liveState
  return liveProgram { liveState = liveState' }

stepProgramMVar
  :: MVar (LiveProgram IO s)
  -> IO ()
stepProgramMVar var = do
  currentProgram <- takeMVar var
  nextProgram <- stepProgram currentProgram
  putMVar var nextProgram

launch
  ::           LiveProgram IO s
  -> IO (MVar (LiveProgram IO s))
launch liveProgram = do
  var <- newMVar liveProgram
  forkIO $ forever $ stepProgramMVar var
  return var
\end{code}
\caption{\texttt{LiveProgramPreliminary.lhs}}
\label{fig:LiveProgramPreliminary}
\end{figure}
