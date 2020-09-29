\begin{comment}
\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DemoWai.DemoWai2 where

-- base
import Control.Concurrent.MVar
import Data.Data
import Data.Maybe (maybeToList)

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- bytestring
import Data.ByteString.Char8 (unpack)

-- wai
import Network.Wai

-- essence-of-live-coding
import LiveCoding hiding (State)

import DemoWai.Env
\end{code}
\end{comment}

\fxwarning{Can we simplify this server?}
\fxwarning{For fun we could also make a server out of it that says "You again!" when the same user agent comes and doesn't increment the counter. (That would save the fromStrict)}
\begin{figure}
\begin{code}
data State = State
  { nVisitors :: Integer
  , lastAgent :: Maybe String
  } deriving Data
\end{code}
\begin{code}
newServer :: LiveProgram (ReaderT Env IO)
newServer = LiveProgram
  { liveState = State 0 Nothing
  , liveStep  = \State { .. } -> do
      Env { .. } <- ask
      request <- lift $ takeMVar requestVar
      let nVisitorsNew = nVisitors + 1
          lastAgentStrings = case lastAgent of
            Nothing -> []
            Just str -> ["Last agent: " <> str]
          lastAgentNew = fmap unpack
            $ lookup "User-Agent"
            $ requestHeaders request
      lift $ putMVar responseVar $ unlines $
        [ "This is Fancy Nu $3rv3r!"
        , "You are visitor #"
        <> show nVisitorsNew <> "."
        ] ++ lastAgentStrings
      return $ State nVisitorsNew lastAgentNew
  }
\end{code}
\caption{DemoWai2.lhs}
\label{fig:DemoWai2}
\end{figure}
