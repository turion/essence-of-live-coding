\begin{comment}
\begin{code}
module LiveCoding.CellExcept.Newtype where

\end{code}
\end{comment}

To create our own control flow context,
we might be tempted to create a simple newtype:

\begin{code}
newtype CellExcept m a b e
      = CellExcept (Cell (ExceptT e m) a b)
\end{code}

Our hope is to give \mintinline{haskell}{Functor}, \mintinline{haskell}{Applicative} and \mintinline{haskell}{Monad} instances to it.

The first step is not to hard.
When an exception is raised,
we simply apply a given function to it:

\begin{code}
instance Functor m
  => Functor (CellExcept m a b) where
  fmap f (CellExcept cell) = CellExcept
    $ hoistCell (withExceptT f) cell
\end{code}

The \mintinline{haskell}{pure} function of the \mintinline{haskell}{Applicative} class
(or equivalently, \mintinline{haskell}{return} of the \mintinline{haskell}{Monad}),
is simply throwing an exception,
\fxwarning{Can quote the actual function and section}
wrapped in the newtype.
