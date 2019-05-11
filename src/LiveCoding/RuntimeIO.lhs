\begin{comment}
\begin{code}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module LiveCoding.RuntimeIO where

-- base
import Control.Arrow
import Control.Concurrent
import Control.Monad
import Data.Data

-- essenceoflivecoding
import LiveCoding.LiveProgram
import LiveCoding.Debugger
import LiveCoding.Migrate

stepProgram :: Monad m => LiveProgram m -> m (LiveProgram m)
stepProgram LiveProgram {..} = do
  liveState' <- liveStep liveState
  return LiveProgram { liveState = liveState', .. }

stepProgramMVar
  :: MVar (LiveProgram IO)
  -> IO ()
stepProgramMVar var = do
  currentProgram <- takeMVar var
  nextProgram <- stepProgram currentProgram
  putMVar var nextProgram
\end{code}
\end{comment}

\section{The runtime}
\label{sec:runtime}

\subsection{Hands on interaction}
Enough declaration.
Let us get semantic and run some live programs!
In the preliminary version,
a function \mintinline{haskell}{stepProgram} implemented a single execution step,
and it can be reused here,
up to removing the explicit state type.
The runtime behaviour of a live program is defined by calling this function repeatedly.
We could of course run the program in the foreground thread:
\begin{code}
foreground :: Monad m => LiveProgram m -> m ()
foreground liveProgram
  =   stepProgram liveProgram
  >>= foreground
\end{code}
But this would leave no possibility to exchange the program with a new one.
%But this would then become the main loop,
%and leave no control to exchange the program with a new one.
Instead, we can store the program in an \mintinline{haskell}{MVar}
and call \mintinline{haskell}{stepProgramMVar} on it.
Now that we can migrate any \mintinline{haskell}{Data},
we can follow the original plan of exchanging the live program in mid-execution:
\begin{code}
update
  :: MVar (LiveProgram IO)
  ->       LiveProgram IO
  -> IO ()
update var newProg = do
  oldProg <- takeMVar var
  putMVar var $ hotCodeSwap newProg oldProg
\end{code}
\fxwarning{Why did I have forkIO here?}
The old program is retrieved from the concurrent variable,
migrated to the new state,
and put back for further execution.
And so begins our first live coding session in GHCi
(line breaks added for readability):
\begin{verbatim}
 > var <- newMVar $ LiveProgram 0
    $ \s -> print s >> return (s + 1)
 > stepProgramMVar var
0
 > stepProgramMVar var
1
 > update var $ LiveProgram 0
    $ \s -> print s >> return (s - 1)
 > stepProgramMVar var
2
 > stepProgramMVar var
1
 > stepProgramMVar var
0
\end{verbatim}
%When the live program was updated,
Upon updating,
the state was correctly preserved.
The programs were specified in the interactive session here,
but of course we will want to load the program from a file,
and use GHCi's \texttt{:reload} functionality when we have edited it.
But as soon as we do this,
the local binding \mintinline{haskell}{var} is lost.
The package \texttt{foreign-store} \cite{foreign-store} offers a remedy:
\mintinline{haskell}{var} can be stored persistently across reloads.
To facilitate its usage, GHCi macros are defined for the initialisation and reload operations (Figure \ref{fig:ghci}).
\input{../essenceoflivecoding-ghci/src/LiveCoding/GHCi.lhs}
They assume the main live program and the \mintinline{haskell}{MVar} to be called \mintinline{haskell}{liveProgram} and \mintinline{haskell}{var},
respectively,
but this can of course be generalised.
With the macros loaded, the session simplifies to:
\begin{verbatim}
 > :liveinit
 > :livestep
0
 > :livestep
1
 > :livereload
[1 of 1] Compiling Main ( ... )
Ok, one module loaded.
 > :livestep
2
 > :livestep
1
 > :livestep
0
\end{verbatim}
Before entering \texttt{:livereload},
the main file was edited in place and reloaded.
\begin{comment}
\begin{code}
launch :: LiveProgram IO -> IO (MVar (LiveProgram IO))
launch liveProg = do
  var <- newMVar liveProg
  forkIO $ background var
  return var

launchWithDebugger :: LiveProgram IO -> Debugger IO -> IO (MVar (LiveProgram IO))
launchWithDebugger liveProg debugger = launch $ liveProg `withDebugger` debugger
{-
  var <- newMVar liveProg
  forkIO $ backgroundWithDebugger var debugger
  return var
-}
debug :: Debugger_ -> LiveProgram IO -> IO (LiveProgram IO)
debug Debugger_ { .. } LiveProgram { .. } = do
  liveState' <- debugState liveState
  return LiveProgram { liveState = liveState', .. }

backgroundWithDebugger :: MVar (LiveProgram IO) -> Debugger_ -> IO ()
backgroundWithDebugger var debugger = forever $ do
  liveProg   <- takeMVar var
  liveProg'  <- stepProgram liveProg
  liveProg'' <- debug debugger liveProg'
  putMVar var liveProg''

background :: MVar (LiveProgram IO) -> IO ()
background var = backgroundWithDebugger var noDebugger

{-
-- Old version where combine was called from background
combine :: MVar (LiveProgram IO) -> LiveProgram IO -> IO ()
combine var prog = do
  success <- tryPutMVar var prog
  unless success $ do
    newProg <- takeMVar var
    combine var $ hotCodeSwap prog newProg
-}
\end{code}
\end{comment}
Of course,
it is not intended to enter \texttt{:livestep} repeatedly when coding.
We want to launch a separate thread which executes the steps in the background.
Again, we can reuse the function \mintinline{haskell}{launch}.
(Only the type signature needs updating.)
In the next subsection,
a full example is shown.
