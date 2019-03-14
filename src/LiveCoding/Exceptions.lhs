\begin{comment}
\begin{code}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module LiveCoding.Exceptions
  ( module LiveCoding.Exceptions
  , module Control.Monad.Trans.Except
  ) where
-- TODO Don't export newtype CellExcept and Functor here and haddock mark it

-- base
import Control.Arrow
import Data.Data

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

-- essenceoflivecoding
import LiveCoding.Cell

\end{code}
\end{comment}

\paragraph{The return of the monad?}
For simply throwing exceptions, no new concepts are needed:
\begin{code}
throwC :: Monad m => Cell (ExceptT e m) e arbitrary
throwC = arrM throwE
\end{code}
The above function simply throws the incoming exception.
To throw an exception only if a certain condition is satisfied,
\mintinline{haskell}{if}-constructs in arrow syntax can be readily used.

\begin{comment}
\begin{code}
throwIf :: Monad m => (a -> Bool) -> e -> Cell (ExceptT e m) a a
throwIf condition e = proc a -> do
  if condition a
  then throwC  -< e
  else returnA -< a

throwIf_ :: Monad m => (a -> Bool) -> Cell (ExceptT () m) a a
throwIf_ condition = throwIf condition ()
\end{code}
\end{comment}

\input{ApplicativeCellExcept.lhs}

\paragraph{An existential crisis}
Invigorated, we want to implement the holy grail of Haskell,
\mintinline{haskell}{>>=}.
Unwrapped from the \mintinline{haskell}{newtype},
it would have a type signature like this:
\begin{spec}
bindCell
  :: Cell (ExceptT e1        m) a b
  -> (e1 -> Cell (ExceptT e2 m) a b)
  ->        Cell (ExceptT e2 m) a b
\end{spec}
Its intended semantics is straightforward:
Execute the first cell until it throws an exception,
then use this exception to choose the second cell,
which is to be executed subsequently.

But what is the state type of the result?
When implementing \mintinline{haskell}{cell `bindCell` handler},
we would need to specify some type of internal state.
Before the exception is thrown, this should certainly be the state of \mintinline{haskell}{cell},
but what afterwards?
Worse, the state type of \mintinline{haskell}{handler e1} depends on the \emph{value} of the exception \mintinline{haskell}{e1}!
Without having ordered them, dependent types suddenly jump in our faces,
in the disguise of existential quantification.

Impulsively, we want to shove the existential state type back where it came from.
Why not simply store \mintinline{haskell}{handler e1} as state once the exception \mintinline{haskell}{e1} was thrown,
and use the aptly named \mintinline{haskell}{step} from Section \ref{sec:cells} as step function?
(This is basically the final encoding from Section \ref{sec:msfs and final coalgebras},
and exactly how Dunai implements this feature.)
Because \mintinline{haskell}{Cell}s are not \mintinline{haskell}{Data}.

\paragraph{Live bind}
Accepting a setback, but not final defeat,
we note that the fundamental issue is the inability to typecheck the state of the cell we would like to switch to,
at least not if this cell has to depend on the thrown exception.

If we were able to offer the typechecker a state type immediately,
and defer the actual choice to a later moment,
we can succeed.
What if we were to supply the thrown exception not when instantiating the new cell,
but while it is running, as a live input?

To appreciate the way this idea is implemented here,
observe that, if we are comfortable with the idea of the transformer \mintinline{haskell}{ReaderT r m a} being isomorphic to simply \mintinline{haskell}{r -> m a},
one could rewrite the type signature of bind as:
\begin{spec}
(>>=) :: Monad m => m a -> ReaderT a m b -> m b
\end{spec}
In words, bind handles a \mintinline{haskell}{ReaderT} effect.
This inspires the type signature of the following control flow combinator,
which we will call \emph{live bind}:

\begin{code}
(>>>=)
  :: (Data e1, Monad m)
  => Cell (ExceptT e1             m)  a b
  -> Cell (ReaderT e1 (ExceptT e2 m)) a b
  -> Cell             (ExceptT e2 m)  a b
\end{code}
Its syntax is a combination of the monadic bind \mintinline{haskell}{>>=} and the sequential composition operator \mintinline{haskell}{>>>}.
\begin{comment}
\begin{code}
Cell state1 step1 >>>= Cell state2 step2 = Cell { .. }
  where
    cellState = NotYetThrown state1 state2
    cellStep (NotYetThrown state1 state2) a = do
      continueExcept <- lift $ runExceptT $ step1 state1 a
      case continueExcept of
        Right (b, state1') -> return (b, NotYetThrown state1' state2)
        Left e1 -> cellStep (Thrown e1 state2) a
    cellStep (Thrown e1 state2) a = do
      (b, state2') <- runReaderT (step2 state2 a) e1
      return (b, Thrown e1 state2')
\end{code}
\end{comment}
It is easier to understand its behaviour once we know the type of state it stores.
Assume the cells \mintinline{haskell}{cell1} and \mintinline{haskell}{cell2} store state of the types \mintinline{haskell}{state1} and \mintinline{haskell}{state2},
respectively.
Then \mintinline{haskell}{cell1 >>>= cell2} holds state of the following type:
\begin{code}
data LiveBindState e1 state1 state2
    = NotYetThrown    state1 state2
    | Thrown       e1        state2
  deriving (Typeable, Data)
\end{code}
Before an exception is thrown, it is initialised with the initial state of both cells,
\mintinline{haskell}{NotYetThrown state1 state2}.
If no exception occurs, only \mintinline{haskell}{state1} is stepped.
As soon as an exception is thrown,
the state is switched to \mintinline{haskell}{Thrown e state2}.
The state of \mintinline{haskell}{cell1} is discarded,
all information in it relevant to the rest of the live program must be passed into the exception.
The thrown exception \mintinline{haskell}{e} is passed into the \mintinline{haskell}{ReaderT e} context of \mintinline{haskell}{cell2},
which is then executed indefinitely.
The resulting cell may throw an exception of its own,
which can in turn be handled again.
The state of \mintinline{haskell}{cell1 >>>= cell2} not only holds the state of the individual cells,
but also the \emph{control flow state},
that is, it designates which cell currently has control.

\paragraph{Applying it to \mintinline{haskell}{Applicative}}
Armed with this new control flow operator,
we can already implement quite a few nontrivial programs.
\fxerror{example}
And like the sequential application operator \mintinline{haskell}{<*>} of the \mintinline{haskell}{Applicative} class
can be defined from \mintinline{haskell}{>>=},
it can also be defined from \mintinline{haskell}{>>>=},
as we shall see now!
If we are allowed to read the first exception during the execution of the second cell,
we can simply re-raise it once the second exception is thrown:
\begin{code}
andThen
  :: (Data e1, Monad m)
  => Cell (ExceptT  e1      m) a b
  -> Cell (ExceptT      e2  m) a b
  -> Cell (ExceptT (e1, e2) m) a b
cell1 `andThen` cell2
  = cell1 >>>= hoistCell readException cell2
  where
    readException
      :: Functor m
      => ExceptT                 e2  m  x
      -> ReaderT e1 (ExceptT(e1, e2) m) x
    readException exception = ReaderT
      $ \e1 -> withExceptT (e1, ) exception
\end{code}
Given two \mintinline{haskell}{Cell}s,
the first may throw an exception,
upon which the second cell gains control.
As soon as it throws a second exception,
both exceptions are thrown as a tuple.
