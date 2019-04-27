\begin{comment}
\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DemoWai.DemoWai1 where

-- base
import Data.Data
import Control.Concurrent.MVar

-- bytestring
import Data.ByteString.Lazy.Char8

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- essenceoflivecoding
import LiveCoding.LiveProgram

import DemoWai.Env
\end{code}
\end{comment}

\begin{figure}
\begin{code}
data State = State
  { nVisitors :: Integer
  } deriving Data

oldServer :: LiveProgram (ReaderT Env IO)
oldServer = LiveProgram
  { liveState = State 0
  , liveStep  = \State { .. } -> do
      Env { .. } <- ask
      _ <- lift $ takeMVar requestVar
      let nVisitorsNew = nVisitors + 1
          msg = pack
            $  "This is Ye Olde Server.\n"
            <> "You are visitor #"
            <> show nVisitorsNew <> "."
      lift $ putMVar responseVar msg
      return $ State nVisitorsNew
  }
\end{code}
\caption{DemoWai1.lhs}
\label{fig:DemoWai1}
\end{figure}
