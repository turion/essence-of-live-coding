\begin{comment}
\begin{code}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module LiveCoding.Preliminary.CellExcept.Applicative where

-- base
import Data.Data

-- transformers
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Exceptions

\end{code}
\end{comment}

\paragraph{Applying it to \mintinline{haskell}{Applicative}}
If we are allowed to read the first exception during the execution of the second cell,
we can simply re-raise it once the second exception is thrown:
\begin{code}
andThen
  :: (Data e1, Monad m)
  => Cell (ExceptT  e1      m) a b
  -> Cell (ExceptT      e2  m) a b
  -> Cell (ExceptT (e1, e2) m) a b
cell1 `andThen` Cell { .. } = cell1 >>>= Cell
  { cellStep = \state (e1, a) ->
      withExceptT (e1, ) $ cellStep state a
  , ..
  }
\end{code}

\begin{comment}
\begin{spec}
  hoistCell readException cell2
  where
    readException
      :: Functor m
      => ExceptT                 e2  m  x
      -> ReaderT e1 (ExceptT(e1, e2) m) x
    readException exception = ReaderT
      $ \e1 -> withExceptT (e1, ) exception
\end{spec}
\end{comment}
Given two \mintinline{haskell}{Cell}s,
the first may throw an exception,
upon which the second cell gains control.
As soon as it throws a second exception,
both exceptions are thrown as a tuple.

At this point, we unfortunately have to give up the efficient \mintinline{haskell}{newtype}.
The spoilsport is, again the type class \mintinline{haskell}{Data},
to which the exception type \mintinline{haskell}{e1} is subjected
(since the exception must be stored during the execution of the second cell).
But the issue is minor,
it is fixed by defining the \emph{free functor},
or \emph{Co-Yoneda construction}:
\fxwarning{Maybe cite http://comonad.com/reader/2016/adjoint-triples/ or search something else}
\fxwarning{Possible other names: Mode}
\begin{code}
data CellExcept a b m e = forall e' .
  Data e' => CellExcept
  { fmapExcept :: e' -> e
  , cellExcept :: Cell (ExceptT e' m) a b
  }
\end{code}
While ensuring that we only store cells with exceptions that can be \emph{bound},
we do not restrict the parameter type \mintinline{haskell}{e}.

It is known that this construction gives rise to a \mintinline{haskell}{Functor} instance for free:
\begin{code}
instance Functor (CellExcept a b m) where
  fmap f CellExcept { .. } = CellExcept
    { fmapExcept = f . fmapExcept
    , ..
    }
\end{code}

The \mintinline{haskell}{Applicative} instance arises from the work we have done so far.
\mintinline{haskell}{pure} is implemented by throwing a unit and transforming it to the required exception,
while sequential application is a bookkeeping exercise around the previously defined function \mintinline{haskell}{andThen}:
\begin{code}
instance Monad m
  => Applicative (CellExcept a b m) where
  pure e = CellExcept
    { fmapExcept = const e
    , cellExcept = constM $ throwE ()
    }

  CellExcept fmap1 cell1 <*>
    CellExcept fmap2 cell2 = CellExcept { .. }
    where
      fmapExcept (e1, e2) = fmap1 e1
        $ fmap2 e2
      cellExcept = cell1 `andThen` cell2
\end{code}
