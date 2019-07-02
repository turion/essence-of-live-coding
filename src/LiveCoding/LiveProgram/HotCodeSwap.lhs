\begin{comment}
\begin{code}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module LiveCoding.LiveProgram.HotCodeSwap where

-- essenceoflivecoding
import LiveCoding.LiveProgram
import LiveCoding.Migrate

\end{code}
\end{comment}
\begin{code}
hotCodeSwap
  :: LiveProgram m
  -> LiveProgram m
  -> LiveProgram m
hotCodeSwap
  (LiveProgram newState newStep)
  (LiveProgram oldState _)
  = LiveProgram
  { liveState = migrate newState oldState
  , liveStep  = newStep
  }
\end{code}
