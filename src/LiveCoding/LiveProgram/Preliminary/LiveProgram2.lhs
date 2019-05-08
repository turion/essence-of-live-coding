\begin{figure}
\begin{comment}
\begin{code}
{-# LANGUAGE ExistentialQuantification #-}

module LiveCoding.LiveProgram.Preliminary.LiveProgram2 where

-- base
import Data.Data
\end{code}
\end{comment}
\begin{code}
data LiveProgram = forall s . Data s
  => LiveProgram
  { liveState :: s
  , liveStep  :: s -> IO s
  }
\end{code}
\fxerror{Compile these as well}
\caption{\texttt{LiveProgram2.lhs}}
\label{fig:LiveProgram2}
\end{figure}
