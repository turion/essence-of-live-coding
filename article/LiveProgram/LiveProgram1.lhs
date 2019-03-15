\begin{figure}
\begin{comment}
\begin{code}
module LiveProgram1 where
\end{code}
\end{comment}
\begin{code}
data LiveProgram s = LiveProgram
  { liveState :: s
  , liveStep  :: s -> IO s
  }

stepProgram :: LiveProgram -> IO LiveProgram
stepProgram liveProgram@LiveProgram { .. } = do
  liveState' <- liveStep liveState
  return liveProgram { liveState = liveState' }
\end{code}
\fxerror{Compile these as well}
\caption{\texttt{LiveProgram1.lhs}}
\label{fig:LiveProgram1}
\end{figure}
