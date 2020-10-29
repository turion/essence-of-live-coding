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

-- essence-of-live-coding
import LiveCoding.Bind
import LiveCoding.Cell
import LiveCoding.Exceptions
import LiveCoding.CellExcept
import LiveCoding.LiveProgram

\end{code}
\end{comment}

\subsection{Exceptions Forever}

\fxwarning{Opportunity to call this an SF here (and elsewhere)}
What if we want to change between the oscillator and a waiting period indefinitely?
In other words, how do we repeatedly execute this action:
\begin{code}
sinesWaitAndTry
  :: MonadFix   m
  => CellExcept m () String ()
sinesWaitAndTry = do
  try $ arr (const "Waiting...") >>> wait 1
  try $ sine 5 >>> arr asciiArt  >>> wait 5
\end{code}
\fxwarning{wait is an unintuitive name. Sounds blocking. "forwardFor"?}
The one temptation we have to resist is to recurse in the \mintinline{haskell}{CellExcept} context to prove the absence of exceptions:
\begin{code}
sinesForever'
  :: MonadFix   m
  => CellExcept m () String Void
sinesForever' = do
  sinesWaitAndTry
  sinesForever'
\end{code}
It typechecks, but it does \emph{not} execute correctly.
\fxerror{Why does it hang? Does it really hang?}
As the initial state is built up,
this definition inquires about the initial state of all cells in the \mintinline{haskell}{do}-expression,
but the last one is again \mintinline{haskell}{sinesForever'},
and thus already initialising such a cell hangs in an infinite loop.
Using the standard function \mintinline{haskell}{forever :: Applicative f => f a -> f ()} has the same deficiency,
\fxerror{Have we tested that?}
as it is defined in the same way.

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
and a cell that is to be executed forever.
Of course, the monad \mintinline{haskell}{m} may again contain exceptions that can be used to break from this loop.
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
\mintinline{haskell}{foreverE e cell} starts with the initial state of \mintinline{haskell}{cell},
and a given value \mintinline{haskell}{e}.
Then \mintinline{haskell}{cell} is stepped,
mutating \mintinline{haskell}{currentState},
until it encounters an exception.
This new exception is stored,
and the cell is restarted with the original initial state.
The cell may use the additional input \mintinline{haskell}{e}
to ask for the last thrown exception
(or the initial value, if none was thrown yet).
The exception is thus the only method of passing on data to the next loop iteration.\footnote{%
It is the user's responsibility to ensure that it does not introduce a space leak,
for example through a lazy calculation that builds up bigger and bigger thunks.
}
In our example, we need not pass on any data,
so a simpler version of the loop operator is defined:
\begin{code}
foreverC
  :: (Data e, Monad m)
  => Cell (ExceptT e m) a b
  -> Cell            m  a b
foreverC = foreverE () . liftCell
  . hoistCell (withExceptT $ const ())
\end{code}
Now we can finally implement our cell:
\fxwarning{Not an SF. Add MonadFix to SF defintiion?}
\begin{code}
sinesForever :: MonadFix m => Cell m () String
sinesForever = foreverC
  $ runCellExcept
  $ sinesWaitAndTry
\end{code}
\begin{code}
printSinesForever :: LiveProgram IO
printSinesForever = liveCell
  $   sinesForever
  >>> printEverySecond
\end{code}
Let us run it:
\verbatiminput{../demos/DemoSinesForever.txt}
\fxwarning{Is the [...] good or not? (Here and elsewhere)}
\fxerror{What's the advantage of forever? How to livecode with it?}

\fxerror{``Forever and ever?'' Show graceful shutdown with ExceptT. Have to change the runtime slightly for this.}
\fxnote{Awesome idea: Electrical circuits simulation where we can change the circuits live!}
