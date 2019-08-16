\begin{comment}
\begin{code}
{-# LANGUAGE Arrows #-}

module LiveCoding.Preliminary.CellExcept.Monad where

-- transformers
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Exceptions

\end{code}
\end{comment}

\subsection{Finite patience with monads}
\fxerror{Explain that we're using applicative do, and use it in example?}
While \mintinline{haskell}{Applicative} control flow is certainly appreciated,
and the live bind combinator \mintinline{haskell}{>>>=} is even more expressive,
it still encourages boilerplate code like the following:
\fxerror{Replace by actual example}
\begin{spec}
throwBool >>>= proc (bool, a) -> do
  if bool
  then foo1 -< a
  else foo2 -< a
\end{spec}
The annoyed library user will promptly abbreviate this pattern:
\fxerror{Rewrite such as to write a commute-like function, as motivation for the type class}
\begin{code}
bindBool
  :: Monad m
  => Cell (ExceptT Bool       m) a b
  -> (Bool -> Cell (ExceptT e m) a b)
  -> Cell          (ExceptT e m) a b
bindBool cell handler
  = cell >>>= proc (bool, a) -> do
      if bool
      then handler True  -< a
      else handler False -< a
\end{code}
\fxwarning{And now use bindBool to rewrite the upper example}
\begin{comment}
\begin{code}
{-
bindBool'
  :: (Monad m, Data e, Finite e)
  => CellExcept m a b Bool
  -> (Bool -> CellExcept m a b e)
  -> CellExcept m a b e
bindBool' cellE handler = CellExcept
  { fmapExcept = id
  , cellExcept = runCellExcept cellE `bindBool` (runCellExcept . handler)
  }
-}
\end{code}
\fxerror{Finish the wrapped thing}
\end{comment}
\fxerror{We have a Data e here suddenly.
Can we be cleverer than id?}
But, behold!
Up to the \mintinline{haskell}{CellExcept} wrapper,
we have just implemented bind,
the holy grail which we assumed to be denied!
The bound type is restricted to \mintinline{haskell}{Bool},
admitted,
but if it is possible to bind \mintinline{haskell}{Bool},
then it is certainly possible to bind \mintinline{haskell}{(Bool, Bool)},
by nesting two \mintinline{haskell}{if}-statements.
By the same logic, we can bind \mintinline{haskell}{(Bool, Bool, Bool)} %, 
%\mintinline{haskell}{(Bool, Bool, Bool, Bool)},
and so on
(and of course any isomorphic type as well).
In fact, \emph{any finite type} can be bound in principle,
by embedding it in such a binary vector.
For what follows, we will only consider finite algebraic datatypes.
These are essentially the unit type (or any single constructor type),
sum types (or multiple constructor types) of other finite types,
and product types (or multiple argument constructors).
Recursive datatypes are infinite in Haskell
(consider, e.g., the list type).

How can it be that a general bind function does not type-check,
but we can implement one for any finite type?
If the exception type \mintinline{haskell}{e} is finite,
the type checker can inspect the state type of the cell \mintinline{haskell}{handler e}
for every possible exception value,
\emph{at compile time}.
All that is needed is a little help to spell out all the possible cases,
as has been done for \mintinline{haskell}{Bool}.
\fxerror{Wow! This means that the control state of such live programs is always finite! This means e.g. that we can completely analyse CTL on it!}

But certainly, we don't want to write out all possible values of a type before we can bind it.
Again, the Haskellers' aversion to boilerplate has created a solution that can be tailored to our needs:
Generic deriving \cite{GenericDeriving}.
We simply need to implement a bind function for generic sum types and product types,
then this function can be abstracted into a type class,
and GHC can infer a default instance for every algebraic data type by adding a single line of boilerplate.
Since the type class is defined for all finite algebraic datatypes, we will call it \mintinline{haskell}{Finite}.
\fxerror{Example}
Any user-contributed or standard type can be an instance this type class,
given that it is not recursive.
\fxerror{We omitted functions! But this isn't such a big problem since they don't have a Data instance anyways.}

It is possible to restrict the previous \mintinline{haskell}{CellExcept} definition by the typeclass:
\begin{spec}
data CellExcept m a b e = forall e' .
  (Data e', Finite e') => CellExcept
  { fmapExcept :: e' -> e
  , cellExcept :: Cell (ExceptT e' m) a b
  }
\end{spec}
Implementing the individual bind functions for sums and products,
and finally writing down the complete \mintinline{haskell}{Monad} instance is a tedious exercise in Generic deriving.
\input{../essence-of-live-coding/src/LiveCoding/CellExcept.lhs}
