\begin{figure}
\begin{comment}
\begin{code}
{-# LANGUAGE RecordWildCards #-}

module LiveCoding.Preliminary.LiveProgram.LiveProgramPreliminary where

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
\end{code}
\begin{code}
stepProgram
  :: Monad m
  => LiveProgram m s -> m (LiveProgram m s)
stepProgram liveProgram@LiveProgram { .. } = do
  liveState' <- liveStep liveState
  return liveProgram { liveState = liveState' }
\end{code}
\begin{code}
stepProgramMVar
  :: MVar (LiveProgram IO s)
  -> IO ()
stepProgramMVar var = do
  currentProgram <- takeMVar var
  nextProgram <- stepProgram currentProgram
  putMVar var nextProgram
\end{code}
\begin{code}
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
