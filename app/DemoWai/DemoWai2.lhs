\begin{comment}
\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DemoWai.DemoWai2 where

-- base
import Control.Concurrent.MVar
import Data.Data
import Data.Maybe (fromMaybe)

-- bytestring
import Data.ByteString.Lazy.Char8

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- wai
import Network.Wai

-- essenceoflivecoding
import LiveCoding.LiveProgram

import DemoWai.Env
\end{code}
\end{comment}

\fxwarning{For fun we could also make a server out of it that says "You again!" when the same user agent comes and doesn't increment the counter. (That would save the fromStrict)}
\begin{figure}
\begin{code}
data State = State
  { nVisitors :: Integer
  , lastAgent :: Maybe ByteString
  } deriving Data

newServer :: LiveProgram (ReaderT Env IO)
newServer = LiveProgram
  { liveState = State 0 Nothing
  , liveStep  = \State { .. } -> do
      Env { .. } <- ask
      request <- lift $ takeMVar requestVar
      let nVisitorsNew = nVisitors + 1
          msg =  "This is Fancy Nu $3rv3r!\n"
            <> "You are visitor #"
            <> (pack $ show nVisitorsNew)
            <> maybe "." (".\nLast agent: " <>)
               lastAgent
          lastAgentNew = fmap fromStrict
            $ lookup "User-Agent"
            $ requestHeaders request
      lift $ putMVar responseVar msg
      return $ State nVisitorsNew lastAgentNew
  }
\end{code}
\caption{DemoWai2.lhs}
\label{fig:DemoWai2}
\end{figure}