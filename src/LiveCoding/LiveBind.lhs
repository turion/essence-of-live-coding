
\paragraph{An existential crisis}
Invigorated, we want to implement the holy grail of Haskell,
\mintinline{haskell}{>>=}.
Unwrapped from the \mintinline{haskell}{newtype},
it would have a type signature like this:
\begin{spec}
bindCell
  :: Monad                   m
  => Cell (ExceptT e1        m) a b
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
in the disguise of existential quantification.\footnote{%
Unfortunately, we cannot achieve the goal by reverting to the preliminary definition of live programs,
which did not make the state type existential.
The corresponding \mintinline{haskell}{Cell} definition would not be an instance of \mintinline{haskell}{Arrow} anymore,
and the type signatures would bloat indefinitely.
But worst of all, \mintinline{haskell}{bindCell} would restrict the state of all cells the handler could output to the same type!
Except in very simple cases, we could not branch between different cells at all.
}
Impulsively, we want to shove the existential state type back where it came from.
Why not simply store \mintinline{haskell}{handler e1} as state once the exception \mintinline{haskell}{e1} was thrown,
and use the aptly named \mintinline{haskell}{step} from Section \ref{sec:cells} as step function?
(This is basically the final encoding from Section \ref{sec:msfs and final coalgebras},
and exactly how Dunai implements this feature.)
But it is not possible,
because \mintinline{haskell}{Cell}s are not \mintinline{haskell}{Data}.

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
\fxerror{Is it easier to discuss runExceptC and then store the exception there?}
\begin{spec}
(>>>=)
  :: (Data e1, Monad m)
  => Cell (ExceptT e1             m)  a b
  -> Cell (ReaderT e1 (ExceptT e2 m)) a b
  -> Cell             (ExceptT e2 m)  a b
\end{spec}
Its syntax is a combination of the monadic bind \mintinline{haskell}{>>=} and the sequential composition operator \mintinline{haskell}{>>>}.
\begin{comment}
\begin{spec}
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
\end{spec}
It is easier to understand its behaviour once we know the type of state it stores.
Assume the cells \mintinline{haskell}{cell1} and \mintinline{haskell}{cell2} store state of the types \mintinline{haskell}{state1} and \mintinline{haskell}{state2},
respectively.
Then \mintinline{haskell}{cell1 >>>= cell2} holds state of the following type:
\begin{code}
data LiveBindState e1 state1 state2
  =  NotYetThrown     state1 state2
  |  Thrown        e1        state2
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

