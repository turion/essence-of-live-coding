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

livestop _ = return "stop LaunchedProgram { .. } liveProgram"

\end{code}

\begin{spec}
-- Previous version. Could also parametrise by liveProgram
liveinit "" = liveinit "liveProgram"
liveinit progName = return $ unlines
  [ "programVar <- newMVar " ++ progName
  , "save programVar"
  ]
\end{spec}
\begin{spec}
livereload "" = livereload "liveProgram"
livereload progName = return $ unlines
  [ ":reload"
  , "programVar <- load"
  , "update programVar " ++ progName
  ]
\end{spec}
\end{comment}
\begin{figure}
\begin{code}
-- code is getting uglier. Do I want to store the m type? Because then I can't switch backend monads easily anymore
load :: IO (MVar (LiveProgram IO), Maybe ThreadId)
load = readStore $ Store 0
\end{code}
\begin{code}
save :: (MVar (LiveProgram IO), Maybe ThreadId) -> IO ()
save = writeStore $ Store 0
\end{code}
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
  , "launchedProgram@LaunchedProgram { .. } <- load"
  , "update launchedProgram liveProgram"
  ]
\end{code}
\begin{code}
livestep _ = return "stepProgramMVar programVar"
\end{code}
\caption{\texttt{GHCi.lhs}}
\label{fig:ghci}
\end{figure}
