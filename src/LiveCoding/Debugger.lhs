\begin{comment}
\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module LiveCoding.Debugger where

-- base
import Control.Concurrent
import Control.Monad (void)
import Data.Data
import Data.IORef

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State

-- syb
import Data.Generics.Text

-- essenceoflivecoding
import LiveCoding.LiveProgram
import LiveCoding.Cell

\end{code}
\end{comment}

\subsection{Debugging the live state}
Having the complete state of the program in one place allows us to inspect and debug it in a central place.
We might want to display the state, or aspects of it,
interact with the user and possibly even change it in place,
if necessary.
\begin{comment}
These patterns are abstracted in a simple definition:
\fxerror{Could make Debuggers Cells as well,
or rather \mintinline{haskell}{type Debugger_ = LiveProgram (ReaderT (Data s => s)) IO}.
Then have \mintinline{haskell}{withDebugger :: LiveProgram IO -> Debugger_ -> LiveProgram IO}.
Either the dbugger coul dbe synhronous,
or even asynchronous and only rceive th estate through an IORef.}
\begin{code}
newtype Debugger_ = Debugger_
  { debugState
      :: forall s . Data s => s -> IO s
  }
\end{code}
\end{comment}
In short, a debugger is a program that can read and modify,
as an additional effect,
the state of an arbitrary live program:
\begin{code}
data Debugger m = Debugger
  { getDebugger :: forall s .
      Data s => LiveProgram (StateT s m)
  }
\end{code}
A simple debugger prints the unmodified state to the console:
\begin{code}
gshowDebugger :: Debugger IO
gshowDebugger = Debugger
  $ liveCell $ arrM $ const $ do
    state <- get
    lift $ putStrLn $ gshow state
\end{code}
Thanks to the \mintinline{haskell}{Data} typeclass,
the state does not need to be an instance of \mintinline{haskell}{Show} for this to work:
\texttt{syb} offers a generic \mintinline{haskell}{gshow} function.
A more sophisticated debugger could connect to a GUI and display the state there,
even offering the user to pause the execution and edit the state live.
\fxwarning{Should I explain countDebugger? What for?}
\begin{comment}
\fxerror{To make this work or show this,
should make them Cell IO () (), then they are endos in the Cell IO category.}
Debuggers are endomorphisms in the Kleisli category of \mintinline{haskell}{IO},
and thus \mintinline{haskell}{Monoid}s:
A pair of them can be chained by executing them sequentially,
and the trivial debugger purely \mintinline{haskell}{return}s the state unchanged.
\end{comment}
We can bake a debugger into a live program:
\begin{code}
withDebugger
  :: Monad       m
  => LiveProgram m
  -> Debugger    m
  -> LiveProgram m
\end{code}
\begin{comment}
\begin{code}
withDebugger
  (LiveProgram    state    step)
  (Debugger (LiveProgram dbgState dbgStep))
  = LiveProgram { .. } where
    liveState = Debugging { .. }
    liveStep  = \Debugging { .. } -> do
      state' <- step state
      s <- runStateT (dbgStep dbgState) state'
      return $ uncurry Debugging s
\end{code}
\end{comment}
Again, let us understand the function through its state type:
\begin{code}
data Debugging dbgState state = Debugging
  { dbgState :: dbgState
  , state    :: state
  } deriving Data
\end{code}
On every step, the debugger becomes active after the cell steps,
and is fed the current \mintinline{haskell}{state} of the main program.
\fxerror{We need to make sure migration doesn't fail when we add them!}
Depending on \mintinline{haskell}{dbgState},
it may execute some side effects or mutate the \mintinline{haskell}{state},
or do nothing at all\footnote{%
This option is important performance: E.g. for an audio application,
performing side effects on every sample would be an unbearable slow down.}.
\begin{comment}
We can start them alongside with the live program:
\fxwarning{Move appropriately, e.g. a separate file RuntimeDebugger}
\begin{spec}
launchWithDebugger
  :: LiveProgram IO
  -> Maybe Int
  -> Debugger
  -> IO (MVar (LiveProgram IO))
\end{spec}
\fxerror{Implement the Maybe Int parameter!}
The optional parameter of type \mintinline{haskell}{Maybe Int} specifies between how many execution steps the debugger should be called.
(For an audio application, calling it on every sample would be an unbearable performance penalty.)
\end{comment}

Live programs with debuggers are started just as usual.
\begin{comment}
\fxwarning{Automatise this and the next output}
Inspecting the state of the example \mintinline{haskell}{printSineWait} from Section \ref{sec:control flow context} is daunting, though:
\begin{verbatim}
Waiting...
(Composition ((,) (Composition ((,) (()) 
(Composition ((,) (()) (Composition ((,) 
(Composition ((,) (()) (Composition ((,) 
[...]
\end{verbatim}
\fxerror{I still hav the tuples here!}
The arrow syntax desugaring introduces a lot of irrelevant overhead such as compositions with the trivial state type \mintinline{haskell}{()},
hiding the parts of the state we are actually interested in.
Luckily, it is a simple, albeit lengthy exercise in generic programming to prune all irrelevant parts of the state,
resulting in a tidy output%\footnote{%
%Line breaks were added to fit the columns.}
 like:
\end{comment}
Let us inspect the state of the example \mintinline{haskell}{printSineWait} from Section \ref{sec:control flow context}.
It is a simple, albeit lengthy exercise in generic programming to prune all irrelevant parts of the state when printing it,
resulting in a tidy output like:
\fxwarning{Automatise this}
\begin{verbatim}
Waiting...
NotThrown: (1.0e-3)
 >>> +(0.0) >>> (0.0)+ >>> (1)
NotThrown: (2.0e-3)
 >>> +(0.0) >>> (0.0)+ >>> (2)
[...]
Waiting...
NotThrown: (2.0009999999998906)
 >>> +(0.0) >>> (0.0)+ >>> (2001)
Exception:
 >>> +(3.9478417604357436e-3) >>> (0.0)+
 >>> (2002)
[...]
\end{verbatim}
\begin{comment}
Exception:
 >>> +(7.895683520871487e-3) >>>
 (3.947841760435744e-6)+
 >>> (2003)
\end{comment}
The cell is initialised in a state where the exception hasn't been thrown yet,
and the \mintinline{haskell}{localTime} is \mintinline{haskell}{1.0e-3} seconds.
The next line corresponds to the initial state (position and velocity) of the sine generator which will be activated after the exception has been thrown,
followed by the internal counter of \mintinline{haskell}{printEverySecond}.
In the next step, local time and counter have progressed.
Two thousand steps later, the exception is finally thrown,
and the sine wave starts.

\begin{comment}
\begin{code}
instance Semigroup Debugger_ where
  debugger1 <> debugger2 = Debugger_ $ \s -> debugState debugger1 s >>= debugState debugger2

instance Monoid Debugger_ where
  mempty = noDebugger

noDebugger :: Debugger_
noDebugger = Debugger_ $ return

newtype CountObserver = CountObserver { observe :: IO Integer }

countDebugger :: IO (Debugger IO, CountObserver)
countDebugger = do
  countRef <- newIORef 0
  observeVar <- newEmptyMVar
  let debugger = Debugger $ liveCell $ arrM $ const $ lift $ do
        n <- readIORef countRef
        putMVar observeVar n
        yield
        void $ takeMVar observeVar
        writeIORef countRef $ n + 1
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
