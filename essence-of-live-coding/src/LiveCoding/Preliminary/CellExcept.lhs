\begin{comment}
\begin{code}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module LiveCoding.Preliminary.CellExcept where

-- base
import Control.Arrow
import Data.Data
import Data.Either (fromRight)
import Data.Void

-- transformers
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Preliminary.CellExcept.Applicative
import LiveCoding.Exceptions
\end{code}
\end{comment}

\paragraph{Using exceptions}
\fxwarning{We didn't mention the newtype in the last paragraph, this is maybe confusing}
We can enter the \mintinline{haskell}{CellExcept} context from an exception-throwing cell,
trying to execute it until the exception occurs:
\fxerror{This doesn't work here anymore because we haven't explained how it's a newtype.
Also we already know that try needs an extra type class. Take this from the monad section.}
\begin{code}
try
  :: Data          e
  => Cell (ExceptT e m) a b
  -> CellExcept      m  a b e
try = CellExcept id
\end{code}
And we can leave it safely once we have proven that there are no exceptions left to throw,
i.e. the exception type is empty:
\fxerror{I'm using runCellExcept which wasn't explained yet}
\begin{code}
safely
  :: Monad      m
  => CellExcept m a b Void
  -> Cell       m a b
safely = hoistCell discardVoid . runCellExcept

discardVoid
  :: Functor      m
  => ExceptT Void m a
  ->              m a
discardVoid
  = fmap (either absurd id) . runExceptT
\end{code}
One way to prove the absence of further exceptions is,
of course, to run an exception-free cell:
\begin{code}
safe :: Monad m => Cell m a b -> CellExcept m a b void
safe cell = CellExcept
  { fmapExcept = absurd
  , cellExcept = liftCell cell
  }
\end{code}
If we want to leave an exception unhandled,
this is also possible:
\begin{code}
runCellExcept
  :: Monad           m
  => CellExcept      m  a b e
  -> Cell (ExceptT e m) a b
runCellExcept CellExcept { .. }
  = hoistCell (withExceptT fmapExcept)
    cellExcept
\end{code}
This is especially useful for shutting down a live program gracefully,
using \mintinline{haskell}{e} as the exit code.
\fxerror{But we haven't implemented that yet. And also can only do that with a more general "reactimate"}
