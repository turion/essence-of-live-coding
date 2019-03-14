\begin{comment}
\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module LiveCoding.Forever where
-- base
import Control.Arrow
import Data.Data

-- transformers
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

-- essenceoflivecoding
import LiveCoding.Cell

\end{code}
\end{comment}

The one temptation we have to resist is to recurse in the \mintinline{haskell}{CellExcept} context to prove the absence of exceptions:
\fxfatal{Example}
\begin{spec}
foo = bar *> foo
\end{spec}
\fxfatal{Why does it hang? Does it really hang?}
As the initial state is built up,
the definition of foo inquires about the initial state of the right hand side of \mintinline{haskell}{*>},
but this is again foo,
and thus already initialising such a cell hangs in an infinite loop.
The resolution is an explicit loop operator,
and faith in the library user to employ it.
\begin{code}
foreverE
  :: (Monad m, Data e)
  =>                e
  -> Cell (ReaderT  e (ExceptT e m)) a b
  -> Cell                        m   a b
\end{code}
\fxfatal{Explain how to use it, and state below}
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

\begin{code}
data ForeverE e s = ForeverE
  { lastException :: e
  , initState     :: s
  , currentState  :: s
  }
  deriving Data
\end{code}

\begin{comment}
\begin{code}
foreverC
  :: (Data e, Monad m)
  => Cell (ExceptT e m) a b
  -> Cell            m  a b
foreverC cell = foreverE () $ liftCell $ hoistCell (withExceptT $ const ()) cell
\end{code}
\end{comment}
