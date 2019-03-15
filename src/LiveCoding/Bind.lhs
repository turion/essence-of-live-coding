\begin{comment}
\begin{code}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module LiveCoding.Bind where

-- base
import Control.Arrow
import Data.Data
import Data.Either (fromRight)
import Data.Void
-- import Prelude hiding ((>>=), return, (>>))

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

-- essenceoflivecoding
import LiveCoding.Commutable
import LiveCoding.Cell
-- import LiveCoding.CellExcept
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
throwBool >>>= proc a -> do
  bool <- constM ask -< ()
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
  -> Cell (ExceptT e m) a b
bindBool cell handler = cell >>>= proc a -> do
  bool <- constM ask -< ()
  if bool
  then liftCell $ handler True  -< a
  else liftCell $ handler False -< a
\end{code}

\begin{comment}
\begin{code}
{-
bindBool'
  :: (Monad m, Data e, Commutable e)
  => CellExcept m a b Bool
  -> (Bool -> CellExcept m a b e)
  -> CellExcept m a b e
bindBool' cellE handler = CellExcept
  { fmapExcept = id
  , cellExcept = runCellExcept cellE `bindBool` (runCellExcept . handler)
  }
-}
\end{code}
\fxfatal{Finish the wrapped thing}
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
By the same logic, we can bind \mintinline{haskell}{(Bool, Bool, Bool)}, 
\mintinline{haskell}{(Bool, Bool, Bool, Bool)},
and so on
(and of course any isomorphic type as well).
In fact, \emph{any finite type} can be bound,
by embedding it in such a binary vector.

How can it be that a general bind function does not type-check,
but we can implement one for any finite type?
If the exception type \mintinline{haskell}{e} is finite,
the type checker can inspect the state type of the cell \mintinline{haskell}{handler e}
for every possible exception value,
at compile time!
All that is needed is a little help to spell out all the possible cases,
as has been done for \mintinline{haskell}{Bool}.

But certainly, we don't want to write out all possible values of a type before we can bind it!
Again, the Haskellers' aversion to boilerplate has created a solution that can be tailored to our needs:
Generic deriving \cite{magalhaes2010generic}.
We simply need to implement a bind function for generic sum types and product types,
then this function can be abstracted into a type class \mintinline{haskell}{Finite},
and GHC can infer a default instance for every algebraic data type by adding a single line of boilerplate.
\fxerror{Example}
Any user-contributed or standard type can be an instance this type class,
given that it is finite.

Again by the power of the Coyoneda embedding,
we can restrict the \mintinline{haskell}{CellExcept} definition by the typeclass:
\begin{spec}
data CellExcept m a b e = forall e' .
  (Data e', Finite e') => CellExcept
  { fmapExcept :: e' -> e
  , cellExcept :: Cell (ExceptT e' m) a b
  }
\end{spec}
\fxwarning{At the end rename ECommutable etc}
Implementing the individual bind functions for sums and products,
and finally writing down the complete \mintinline{haskell}{Monad} instance is a tedious exercise in Generic deriving.
As a slight restriction of the framework,
throwing exceptions is now only allowed for finite types:
\begin{spec}
try
  :: (Data e, Finite e)
  => Cell (ExceptT e m) a b
  -> CellExcept m a b e
\end{spec}
In practice however, this is rarely a severe limitation since in the monad context,
calculations with all types are allowed again.

\fxerror{Wow! This means that the control state of such live programs is always finite! This means e.g. that we can completely analyse CTL on it!}

\fxerror{Not sure whether I want to say it like that. Maybe first talk about commuting Reader, and then go on.}
\fxerror{Wouldhave to call it Finite here now because there is no justification yet to call it Commutable, because we didn't explain the commuting thing.
One way to explain the commuting stuff would be to completely forget about Applicative and straight go from live bind to finite bind.}

\begin{comment}
\begin{code}

instance Monad m => Functor (CellExcept m a b) where
instance Monad m => Applicative (CellExcept m a b) where

instance Monad m => Monad (CellExcept m a b) where
  -- return :: Monad m => e -> CellExcept m a b e
  -- return = pure
  return e = CellExcept
    { fmapExcept = const e
    , cellExcept = constM $ throwE ()
    }

  -- Can I make an operation monad?
  --(>>=)
  --  :: Monad m
  --  => CellExcept m a b e1
  --  -> (e1 -> CellExcept m a b e2)
  --  -> CellExcept m a b e2
  CellExcept fmap1 cell1 >>= handler = go fmap1 cell1 $ ecommute (handler . fmap1)
    where
      go fmap1 cell1 CellExceptReader { .. } = CellExcept
        { cellExcept = cell1 >>>= newCell
        , fmapExcept = newFmap
        }
{-
CellExcept fmap1 cell1 >>= handler =
  let
    Thing newfmap newCell = handlerToThing handler
    cellExcept = cell1 >>>= liveHandler
    -- liveHandler :: Cell (ReaderT e1' (ExceptT ? m)) a b
    liveHandler = commute somehandler
    -- somehandler :: e1' -> Cell (ExceptT ? m) a b
    somehandler = commute $ handler . fmap1
    -- fmapExcept :: ? -> e2
    fmapExcept = _
    -- Data ?, Commutable ?
  in CellExcept { .. }
    --cell1 >>>= commute (runCellExcept . handler . fmap1)
-}
--(>>)
--  :: (Monad m, Commutable e1, Data e2, Commutable e2)
--  => CellExcept m a b e1 -> CellExcept m a b e2 -> CellExcept m a b e2
--cellExcept1 >> cellExcept2 = cellExcept1 >>= const cellExcept2

--ifThenElse True a _ = a
--ifThenElse False _ a = a

-- TODO Actually want to import
runCellExcept
  :: Monad           m
  => CellExcept      m  a b e
  -> Cell (ExceptT e m) a b
runCellExcept CellExcept { .. }
  = hoistCell (withExceptT fmapExcept)
    cellExcept

try :: (Data e, ECommutable e) => Cell (ExceptT e m) a b -> CellExcept m a b e
try = CellExcept id
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
  = fmap (fromRight
      (error "safely: Received Left")
    ) . runExceptT
safe :: Monad m => Cell m a b -> CellExcept m a b void
safe cell = CellExcept
  { fmapExcept = absurd
  , cellExcept = liftCell cell
  }
\end{code}
\end{comment}
