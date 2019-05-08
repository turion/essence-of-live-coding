\begin{comment}
\begin{code}
module LiveCoding.GHCi where

-- base
import Control.Concurrent

-- foreign-store
import Foreign.Store

-- essenceoflivecoding
import LiveCoding.LiveProgram
import LiveCoding.RuntimeIO

-- TODO livelaunch
\end{code}

\begin{spec}
-- Previous version. Could also parametrise by var
liveinit "" = liveinit "liveProgram"
liveinit progName = return $ unlines
  [ "var <- newMVar " ++ progName
  , "save var"
  ]

livereload "" = livereload "liveProgram"
livereload progName = return $ unlines
  [ ":reload"
  , "var <- load"
  , "update var " ++ progName
  ]
\end{spec}
\end{comment}
\begin{figure}
\begin{code}
load :: IO (MVar (LiveProgram IO))
load = readStore (Store 0)

save :: MVar (LiveProgram IO) -> IO ()
save var = writeStore (Store 0) var

liveinit _ = return $ unlines
  [ "var <- newMVar liveProgram"
  , "save var"
  ]

livereload _ = return $ unlines
  [ ":reload"
  , "var <- load"
  , "update var liveProgram"
  ]

livestep _ = return "stepProgramMVar var"
\end{code}
\caption{\texttt{GHCi.lhs}}
\label{fig:ghci}
\end{figure}
