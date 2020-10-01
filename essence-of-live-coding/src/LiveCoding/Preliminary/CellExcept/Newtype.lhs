\begin{comment}
\begin{code}
module LiveCoding.Preliminary.CellExcept.Newtype where

-- base
import Control.Arrow
import Data.Void

-- transformers
import Control.Monad.Trans.Except

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Exceptions
\end{code}
\end{comment}

\subsection{Control flow context}
\label{sec:control flow context}
%\paragraph{Wrapping exceptions}
Inspired by \cite[Section 2, "Control Flow through Exceptions"]{Rhine},
%we create our own control flow context,
%by introducing a newtype:
we introduce a newtype:

\begin{code}
newtype CellExcept m a b e = CellExcept
  { runCellExcept :: Cell (ExceptT e m) a b }
\end{code}

We can enter the \mintinline{haskell}{CellExcept} context from an exception-throwing cell,
trying to execute it until the exception occurs:
\begin{code}
try
  :: Cell (ExceptT e m) a b
  -> CellExcept      m  a b e
try = CellExcept
\end{code}
And we can leave it safely once we have proven that there are no exceptions left to throw,
i.e. the exception type is empty (represented in Haskell by \mintinline{haskell}{Void}):
\begin{code}
safely
  :: Monad      m
  => CellExcept m a b Void
  -> Cell       m a b
\end{code}
\begin{comment}
\begin{code}
safely = hoistCell discardVoid . runCellExcept
  where
    discardVoid
      = fmap (either absurd id) . runExceptT
\end{code}
\end{comment}
One way to prove the absence of further exceptions is,
of course, to run an exception-free cell:
\begin{code}
safe
  :: Monad      m
  => Cell       m a b
  -> CellExcept m a b Void
\end{code}
\begin{comment}
\begin{code}
safe cell = CellExcept $ liftCell cell
\end{code}
\end{comment}

\paragraph{The return of the monad}
Our new hope is to give \mintinline{haskell}{Functor}, \mintinline{haskell}{Applicative} and \mintinline{haskell}{Monad} instances to \mintinline{haskell}{CellExcept}.
We will explore now how this allows for rich control flow.

The \mintinline{haskell}{Functor} instance is not too hard.
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
wrapped in the newtype:
\begin{code}
pure
  :: Monad      m
  =>                  e
  -> CellExcept m a b e
pure e = CellExcept $ arr (const e) >>> throwC
\end{code}

Like the sequential application operator \mintinline{haskell}{<*>} from the \mintinline{haskell}{Applicative} class
can be defined from the bind operator \mintinline{haskell}{>>=},
it can also be defined from the \emph{live bind} operator \mintinline{haskell}{>>>=} introduced previously.
As a technical tour-de-force,
even a \mintinline{haskell}{Monad} instance for \mintinline{haskell}{CellExcept} can be derived with some modifications.
This is shown at length in an appendix\footnote{%
Available online at \href{https://www.manuelbaerenz.de/essence-of-live-coding/EssenceOfLiveCodingAppendix.pdf}{https://www.manuelbaerenz.de/essence-of-live-coding/EssenceOfLiveCodingAppendix.pdf}.
}.

But how can \mintinline{haskell}{Applicative} and \mintinline{haskell}{Monad} be put to use?
The foreground value of \mintinline{haskell}{CellExcept} is the thrown exception.
With \mintinline{haskell}{pure}, such values are created,
and \mintinline{haskell}{Functor} allows us to perform computations with them.
The classes \mintinline{haskell}{Applicative} and \mintinline{haskell}{Monad} allow us to \emph{chain} the execution of exception throwing cells:
\fxwarning{Comment on how Monad is even stronger than Applicative?}
