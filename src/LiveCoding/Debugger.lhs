\begin{comment}
\begin{code}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module LiveCoding.Debugger where

-- base
import Control.Concurrent
import Control.Monad (void)
import Data.Data
import Data.IORef

\end{code}
\end{comment}

\subsection{Debugging the live state}
Having the complete state of the program in one place allows us to inspect and debug it in a central place.
We might want to display the state, or aspects of it,
interact with the user and possibly even change it in place,
if necessary.
These patterns are abstracted in a simple definition:
\begin{code}
newtype Debugger = Debugger
  { debugState
      :: forall s . Data s => s -> IO s
  }
\end{code}
A simple debugger prints\footnote{%
Thanks to the \mintinline{haskell}{Data} typeclass,
the state does not need to be an instance of \mintinline{haskell}{Show} for this to work.
}
the state to the console,
a more sophisticated debugger could connect to a GUI and display the state there,
even offering the user to edit it live.

\fxwarning{Should I explain countDebugger? What for?}
\fxerror{Pick up debuggers later when talking about Cells. Maybe move debuggers there and immediately showcase them. Or at least show a debugger here, maybe with the live server.}

Debuggers are endomorphisms in the Kleisli category of \mintinline{haskell}{IO},
and thus \mintinline{haskell}{Monoid}s:
A pair of them can be chained by executing them sequentially,
and the trivial debugger purely \mintinline{haskell}{return}s the state unchanged.

\begin{comment}
\begin{code}
instance Semigroup Debugger where
  debugger1 <> debugger2 = Debugger $ \s -> debugState debugger1 s >>= debugState debugger2

instance Monoid Debugger where
  mempty = noDebugger

noDebugger :: Debugger
noDebugger = Debugger $ return

newtype CountObserver = CountObserver { observe :: IO Integer }

countDebugger :: IO (Debugger, CountObserver)
countDebugger = do
  countRef <- newIORef 0
  observeVar <- newEmptyMVar
  let debugger = Debugger $ \s -> do
        n <- readIORef countRef
        putMVar observeVar n
        yield
        void $ takeMVar observeVar
        writeIORef countRef $ n + 1
        return s
      observer = CountObserver $ yield >> readMVar observeVar
  return (debugger, observer)

await :: CountObserver -> Integer -> IO ()
await CountObserver { .. } nMax = go
 where
  go = do
    n <- observe
    if n > nMax then return () else go
\end{code}
\end{comment}
\fxerror{Examples for cells and stateprintdebugger}
