\begin{figure}
\begin{comment}
\begin{code}
module LiveProgram1 where
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
\end{code}
\fxerror{Compile these as well}
\fxwarning{Is the module name still appropriate?}
\caption{\texttt{LiveProgram1.lhs}}
\label{fig:LiveProgram1}
\end{figure}
