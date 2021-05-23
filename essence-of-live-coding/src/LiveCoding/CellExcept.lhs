\begin{comment}
\begin{code}
{-# LANGUAGE GADTs #-}

module LiveCoding.CellExcept where

-- base
import Control.Monad
import Data.Data
import Data.Void

-- transformers
import Control.Monad.Trans.Except

-- essenceoflivecoding
import LiveCoding.Cell
import LiveCoding.Exceptions
import LiveCoding.Exceptions.Finite
\end{code}
\end{comment}

We can save on boiler plate by dropping the Coyoneda embedding for an ``operational'' monad:
\fxerror{Cite operational}
\fxerror{Move the following code into appendix?}
\begin{code}
data CellExcept m a b e where
  Return :: e -> CellExcept m a b e
  Bind
    :: CellExcept m a b e1
    -> (e1 -> CellExcept m a b e2)
    -> CellExcept m a b e2
  Try
    :: (Data e, Finite e)
    => Cell (ExceptT e m) a b
    -> CellExcept m a b e
\end{code}

\begin{comment}
\begin{code}
instance Monad m => Functor (CellExcept m a b) where
  fmap = liftM

instance Monad m => Applicative (CellExcept m a b) where
  pure = return
  (<*>) = ap
\end{code}
\end{comment}
The \mintinline{haskell}{Monad} instance is now trivial:
\begin{code}
instance Monad m => Monad (CellExcept m a b) where
  return = Return
  (>>=) = Bind
\end{code}
As is typical for operational monads, all of the effort now goes into the interpretation function:
\begin{code}
runCellExcept
  :: Monad           m
  => CellExcept      m  a b e
  -> Cell (ExceptT e m) a b
\end{code}
\begin{spec}
runCellExcept (Bind (Try cell) g)
  = cell >>>= commute (runCellExcept . g)
runCellExcept ... = ...
\end{spec}
\begin{comment}
\begin{code}
runCellExcept (Return e) = constM $ throwE e
runCellExcept (Try cell) = cell
runCellExcept (Bind (Try cell) g) = cell >>>== commute (runCellExcept . g)
runCellExcept (Bind (Return e) f) = runCellExcept $ f e
runCellExcept (Bind (Bind ce f) g) = runCellExcept $ Bind ce $ \e -> Bind (f e) g
\end{code}
\end{comment}

As a slight restriction of the framework,
throwing exceptions is now only allowed for finite types:
\begin{code}
try
  :: (Data e, Finite e)
  => Cell (ExceptT e m) a b
  -> CellExcept m a b e
try = Try
\end{code}
In practice however, this is less often a limitation than first assumed,
since in the monad context,
calculations with all types are allowed again.
\fxerror{But the trouble remains that builtin types like Int and Double can't be thrown.}

\fxfatal{The rest is explained in the main article differently. Merge.}
\begin{comment}
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
safe :: Monad m => Cell m a b -> CellExcept m a b Void
safe cell = try $ liftCell cell

-- | Run a monadic action and immediately raise its result as an exception.
once :: (Monad m, Data e, Finite e) => (a -> m e) -> CellExcept m a arbitrary e
once kleisli = try $ arrM $ ExceptT . (Left <$>) . kleisli

-- | Like 'once', but the action does not have an input.
once_ :: (Monad m, Data e, Finite e) => m e -> CellExcept m a arbitrary e
once_ = once . const
\end{code}
\end{comment}
