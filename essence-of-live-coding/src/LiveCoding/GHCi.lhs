\begin{comment}
\begin{code}
{- | Support functions to call common live coding functionalities like launching and reloading
from a @ghci@ or @cabal repl@ session.

You typically don't need to import this module in your code,
but you should load it in your interactive session,
ideally by copying the file `essence-of-live-coding/.ghci` to your project,
adjusting it to your needs and launching @cabal repl@.
-}
module LiveCoding.GHCi where

-- base
import Control.Concurrent

-- foreign-store
import Foreign.Store

-- essence-of-live-coding
import LiveCoding.LiveProgram
import LiveCoding.RuntimeIO
import LiveCoding.RuntimeIO.Launch

livelaunch _ = return $ unlines
  [ "launchedProgram <- launch liveProgram"
  , "save launchedProgram"
  ]

livestop _ = return "stop launchedProgram"

\end{code}

\end{comment}
\begin{figure}
\begin{code}
-- The LiveProgram serves as a proxy for m.
load :: Launchable m => LiveProgram m -> IO (LaunchedProgram m)
load _ = readStore $ Store 0
\end{code}
\begin{code}
save :: Launchable m => LaunchedProgram m -> IO ()
save = writeStore $ Store 0
\end{code}
% Could also parametrise by liveProgram
\begin{code}
liveinit _ = return $ unlines
  [ "programVar <- newMVar liveProgram"
  , "threadId <- myThreadId"
  , "save LaunchedProgram { .. }"
  ]
\end{code}
\begin{code}
livereload _ = return $ unlines
  [ ":reload"
  , "launchedProgram <- load liveProgram"
  , "update launchedProgram liveProgram"
  ]
\end{code}
\begin{code}
livestep _ = return "stepLaunchedProgram launchedProgram"
\end{code}
\caption{\texttt{GHCi.lhs}}
\label{fig:ghci}
\end{figure}
