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

-- essence-of-live-coding
import LiveCoding.LiveProgram
import LiveCoding.LiveProgram.HotCodeSwap
import LiveCoding.Debugger
import LiveCoding.Migrate
import LiveCoding.RuntimeIO.Launch
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
To facilitate its usage, GHCi macros are defined for the initialisation and reload operations.

Of course,
it is not intended to enter \texttt{:livestep} repeatedly when coding.
We want to launch a separate thread which executes the steps in the background.
Again, we can reuse the function \mintinline{haskell}{launch}.
(Only the type signature needs updating.)
Using \texttt{ghcid} (``GHCi as a daemon'' \cite{ghcid}),
the launching and reloading operations can be automatically triggered upon starting \texttt{ghcid} and editing the code,
allowing for a smooth live coding experience without any manual intervention.

In the next subsection,
a full example is shown.
