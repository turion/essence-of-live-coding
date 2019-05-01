\begin{comment}
\begin{code}
{-# LANGUAGE ExistentialQuantification #-}

{-# LANGUAGE RecordWildCards #-}
module LiveCoding.CellExcept where

-- base
import Control.Arrow
import Data.Data
import Data.Either (fromRight)
import Data.Void

-- transformers
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

-- essenceoflivecoding
import LiveCoding.Cell
import LiveCoding.Exceptions
\end{code}
\end{comment}

\begin{code}
-- TODO
{-
runExceptC
  :: (Data e, Monad m)
  => Cell (ExceptT e m) a           b
  -> Cell            m  a (Either e b)
runExceptC cellE = (cellE >>> arr Right) >>>= hoistCell discardVoid (constM ask >>> arr Left)
-}
\end{code}
\end{comment}
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
\fxwarning{Thin out and remove only those that are actually needed in the main example}
