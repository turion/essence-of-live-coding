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
import Prelude hiding (unlines)

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

\fxwarning{Can we simplify this server?}
\fxwarning{For fun we could also make a server out of it that says "You again!" when the same user agent comes and doesn't increment the counter. (That would save the fromStrict)}
\begin{figure}
\begin{code}
data State = State
  { nVisitors :: Integer
  , lastAgent :: Maybe ByteString
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
          lastAgentNew = fmap fromStrict
            $ lookup "User-Agent"
            $ requestHeaders request
      lift $ putMVar responseVar $ unlines $
        [ "This is Fancy Nu $3rv3r!"
        , "You are visitor #"
        <> (pack $ show nVisitorsNew) <> "."
        ] ++ maybeToList
        (("Last agent: " <>) <$> lastAgent)
      return $ State nVisitorsNew lastAgentNew
  }
\end{code}
\caption{DemoWai2.lhs}
\label{fig:DemoWai2}
\end{figure}
