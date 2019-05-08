\begin{comment}
\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module LiveCoding.Forever where
-- base
import Control.Arrow
import Control.Concurrent (threadDelay)
import Control.Monad.Fix
import Data.Data
import Data.Void

-- transformers
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

-- essenceoflivecoding
import LiveCoding.Bind
import LiveCoding.Cell
import LiveCoding.Commutable (CellExcept)
import LiveCoding.LiveProgram

\end{code}
\end{comment}

\subsection{Exceptions forever}

But what if we want to change between the two oscillators indefinitely?
The one temptation we have to resist is to recurse in the \mintinline{haskell}{CellExcept} context to prove the absence of exceptions:
\fxerror{Example}
\begin{code}
sinesForever' :: CellExcept IO () Double Void
sinesForever' = do
  try $ sine 3  >>> throwWhen0
  try $ sine 10 >>> throwWhen0
  sinesForever'
\end{code}
It typechecks, but it does \emph{not} execute correctly.
\fxerror{Why does it hang? Does it really hang?}
As the initial state is built up,
the definition of \mintinline{haskell}{sinesForever'} inquires about the initial state of all cells in the \mintinline{haskell}{do}-expression,
but last one is again \mintinline{haskell}{foo},
and thus already initialising such a cell hangs in an infinite loop.
The resolution is an explicit loop operator,
and faith in the library user to remember to employ it.
\begin{code}
foreverE
  :: (Monad m, Data e)
  =>                e
  -> Cell (ReaderT  e (ExceptT e m)) a b
  -> Cell                        m   a b
\end{code}
The loop function receives as arguments an initial exception,
and a cell that is to be executed forever\footnote{%
Of course, the monad \mintinline{haskell}{m} may again contain exceptions that can be used to gracefully shut down the execution.
}.
\begin{comment}
\begin{code}
foreverE e (Cell state step) = Cell { .. }
  where
    cellState = ForeverE
      { lastException = e
      , initState     = state
      , currentState  = state
      }
    cellStep f@ForeverE { .. } a = do
      continueExcept <- runExceptT $ runReaderT (step currentState a) lastException
      case continueExcept of
        Left e' -> cellStep f { lastException = e', currentState = initState } a
        Right (b, state') -> return (b, f { currentState = state' })
\end{code}
\end{comment}

Again, it is instructive to look at the internal state of the looped cell:
\begin{code}
data ForeverE e s = ForeverE
  { lastException :: e
  , initState     :: s
  , currentState  :: s
  }
  deriving Data
\end{code}
\mintinline{haskell}{foreverE e cell} is initialised with the initial state of \mintinline{haskell}{cell},
and the initial exception \mintinline{haskell}{e}.
Then \mintinline{haskell}{cell} is stepped until it encounters an exception.
This new exception is stored,
and the cell is restarted with the original initial state.
The cell may use the \mintinline{haskell}{ReaderT e} effect
to ask for the last thrown exception
(or the initial exception, if none was thrown yet).
The exception is thus the only method of passing on data to the next loop iteration.
It is the user's responsibility to ensure that it does not introduce a space leak,
for example through a lazy calculation that builds up bigger and bigger thunks.

In our example, we need not pass on any data,
so a simpler version of the loop operator is defined:
\begin{code}
foreverC
  :: (Data e, Monad m)
  => Cell (ExceptT e m) a b
  -> Cell            m  a b
\end{code}
\begin{comment}
\begin{code}
foreverC cell = foreverE () $ liftCell $ hoistCell (withExceptT $ const ()) cell
\end{code}
\end{comment}
Now we can finally implement our cell:
\begin{code}
sinesForever :: MonadFix m => Cell m () Double
sinesForever = foreverC $ runCellExcept $ do
  try $ sine 4 >>> throwWhen0
  try $ sine 6 >>> throwWhen0

printSinesForever :: LiveProgram IO
printSinesForever = liveCell
  $   sinesForever
  >>> printEverySecond
\end{code}
Let us run it:
\verbatiminput{../DemoSinesForever.txt}
\fxwarning{Not looking too good. Also, is the [...] good or not? (Here and elsewhere)}

\fxerror{``Forever and ever?'' Show graceful shutdown with ExceptT. Have to change the runtime slightly for this.}
\fxnote{Awesome idea: Electrical circuits simulation where we can change the circuits live!}
