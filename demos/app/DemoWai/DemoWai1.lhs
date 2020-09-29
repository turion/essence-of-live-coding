\begin{comment}
\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DemoWai.DemoWai1 where

-- base
import Data.Data
import Control.Concurrent.MVar

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- essence-of-live-coding
import LiveCoding hiding (State)

import DemoWai.Env
\end{code}
\end{comment}

\begin{figure}
\begin{code}
data State = State
  { nVisitors :: Integer
  } deriving Data
\end{code}
\begin{code}
oldServer :: LiveProgram (ReaderT Env IO)
oldServer = LiveProgram
  { liveState = State 0
  , liveStep  = \State { .. } -> do
      Env { .. } <- ask
      _ <- lift $ takeMVar requestVar
      let nVisitorsNew = nVisitors + 1
      lift $ putMVar responseVar $ unlines
        [ "This is Ye Olde Server."
        , "You are visitor #"
        <> show nVisitorsNew <> "."
        ]
      return $ State nVisitorsNew
  }
\end{code}
\caption{DemoWai1.lhs}
\label{fig:DemoWai1}
\end{figure}
