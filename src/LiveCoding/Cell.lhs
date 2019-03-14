\fxerror{If more space left, show definitions and explain}
\fxerror{Reorganise in modules properly. For now, don't worry too much.}
\begin{comment}
\begin{code}
-- | TODO: Proper haddock docs
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module LiveCoding.Cell where

-- base
import Control.Arrow
import Control.Category
import Data.Data
import Prelude hiding ((.), id)

-- transformers
import Control.Monad.Trans.Class

\end{code}
\end{comment}

Indeed, we would want these building blocks to be some flavour of functions!
In our definition of live programs as pairs of state and state steppers,
we can generalise the step functions to an additional input and output type:
\begin{spec}
ioStep :: a -> s -> IO (b, s)
\end{spec}
The reader may have rightfully become weary of the ubiquitous \mintinline{haskell}{IO} monad;
and promoting it to an arbitrary monad will turn out shortly to be a very useful generalisation.

\subsection{Cells}
\label{sec:cells}

We collect these insights in a definition,
calling them cells,
the building blocks of everything live:

\begin{comment}
\begin{code}
-- | The basic building block of a live program.
\end{code}
\end{comment}
\begin{code}
data Cell m a b = forall s . Data s => Cell
  { cellState :: s
  , cellStep  :: s -> a -> m (b, s)
  }
\end{code}

Such a cell may progress by one step,
consuming an \mintinline{haskell}{a} as input,
and producing, by means of an effect in an arbitrary monad \mintinline{haskell}{m},
not only the updated cell,
but also an output datum \mintinline{haskell}{b}:

\begin{code}
step :: Monad m => Cell m a b -> a -> m (b, Cell m a b)
step Cell { .. } a = do
  (b, cellState') <- cellStep cellState a
  return (b, Cell { cellState = cellState', .. })
\end{code}

As a simple example, consider the following \mintinline{haskell}{Cell} which adds all input and returns the accumulated sum each step:

\begin{code}
sumC :: (Monad m, Num a, Data a) => Cell m a a
sumC = Cell { .. }
  where
    cellState = 0
    cellStep accum a = let accum' = accum + a
      in return (accum', accum')
\end{code}

\fxerror{Possibly put IO later because here we can't explain yet how we'll end up with () input/output}
We recover live programs as the special case of trivial input and output,
with effects in \mintinline{haskell}{IO}:

\begin{code}
type LiveProgram = Cell IO () ()
\end{code}

In case our \mintinline{haskell}{Cell} is in another monad than \mintinline{haskell}{IO},
it is easy to define a function that transports a cell along a monad morphism:

\begin{code}
hoistCell :: (forall x . m1 x -> m2 x) -> Cell m1 a b -> Cell m2 a b
\end{code}

\begin{comment}
\begin{code}
hoistCell morph Cell { .. } = Cell
  { cellStep = \s a -> morph $ cellStep s a
  , ..
  }

liftCell :: (Monad m, MonadTrans t) => Cell m a b -> Cell (t m) a b
liftCell = hoistCell lift
\end{code}
\end{comment}

\mintinline{haskell}{Cell}s can be composed in three directions:
Sequentially and parallely in the data flow sense,
and sequentially in the control flow sense.
We will address the data flow aspects in this section,
investigating control flow later in section \fxfatal{where?}.

By virtue of being an instance of the type class \mintinline{haskell}{Category}
for any fixed monad \mintinline{haskell}{m},
they implement sequential composition:

\begin{spec}
(>>>)
  :: Monad m
  => Cell m a b
  -> Cell m   b c
  -> Cell m a   c
\end{spec}

\begin{comment}
\begin{code}
data Composition state1 state2 = Composition
  { state1 :: state1
  , state2 :: state2
  } deriving (Typeable, Data)

instance Monad m => Category (Cell m) where
  id = Cell
    { cellState = ()
    , cellStep  = \() a -> return (a, ())
    }

  Cell state2 step2 . Cell state1 step1 = Cell { .. }
    where
      cellState = Composition state1 state2
      cellStep (Composition state1 state2) a = do
        (b, state1') <- step1 state1 a
        (c, state2') <- step2 state2 b
        return (c, (Composition state1' state2'))
\end{code}
\end{comment}
For two cells \mintinline{haskell}{cell1} and \mintinline{haskell}{cell2},
the composite \mintinline{haskell}{cell1 >>> cell2} holds the state of both \mintinline{haskell}{cell1} and \mintinline{haskell}{cell2},
but the step function only touches each state variable individually,
the state stays encapsulated.

\mintinline{haskell}{Cell}s can also be made an instance of the \mintinline{haskell}{Arrow} type class,
which allows for parallel composition:
\begin{spec}
(***)
  :: Monad m
  => Cell  m  a      b
  -> Cell  m     c      d
  -> Cell  m (a, c) (b, d)
\end{spec}
Again, the state type of the composed cell is the product type of the constituent states.

The \mintinline{haskell}{Arrow} type class also allows to lift arbitrary functions to \mintinline{haskell}{Cell}s:

\begin{spec}
arr
  :: Monad m
  ->         (a -> b)
  -> Cell  m  a    b
\end{spec}

Beyond standard arrows, a \mintinline{haskell}{Cell} can encode effects in a monad,
so it is not surprising that Kleisli arrows can be lifted:

\begin{spec}
arrM
  :: Monad m
  ->         (a -> m b)
  -> Cell  m  a      b
\end{spec}

\begin{comment}
\begin{code}
data Parallel s1 s2 = Parallel s1 s2
  deriving (Typeable, Data)

instance Monad m => Arrow (Cell m) where
  arr f = Cell
    { cellState = ()
    , cellStep  = \() a -> return (f a, ())
    }

  Cell state1 step1 *** Cell state2 step2 = Cell { .. }
    where
      cellState = Parallel state1 state2
      cellStep (Parallel state1 state2) (a, c) = do
        (b, state1') <- step1 state1 a
        (d, state2') <- step2 state2 c
        return ((b, d), Parallel state1' state2')

arrM :: Functor m => (a -> m b) -> Cell m a b
arrM f = Cell
  { cellState = ()
  , cellStep  = \() a -> (, ()) <$> f a
  }

constM :: Functor m => m b -> Cell m a b
constM = arrM . const
\end{code}
\end{comment}

We would like to have all basic primitives needed to develop standard synchronous signal processing components,
without touching the \mintinline{haskell}{Cell} constructor anymore.
One crucial bit is missing:
Encapsulating state.
The most general such construction is the feedback loop:
\begin{code}
data Feedback s s' = Feedback s s'
  deriving Data

feedback
  :: (Data s, Monad m)
  => s
  -> Cell m (a, s) (b, s)
  -> Cell m  a      b
\end{code}
\begin{comment}
\begin{code}
feedback s (Cell state step) = Cell { .. }
  where
    cellState = Feedback state s
    cellStep (Feedback state s) a = do
      ((b, s'), state') <- step state (a, s)
      return (b, Feedback state' s')
\end{code}
\end{comment}
It enables us to write delays:
\begin{code}
delay :: (Data s, Monad m) => s -> Cell m s s
delay s = feedback s $ arr
  $ \(sNew, sOld) -> (sOld, sNew)
\end{code}
\mintinline{haskell}{feedback} can be used for accumulation of data.
For example, \mintinline{haskell}{sumC} now becomes:
\begin{code}
sumFeedback
  :: (Monad m, Num a, Data a)
  => Cell m a a
sumFeedback = feedback 0 $ arr $ \(a, accum) ->
  let accum' = a + accum in (accum', accum')
\end{code}
\fxwarning{Possibly remark on Data instance of s?}

\subsection{Monadic stream functions and final coalgebras}

\label{sec:msfs and final coalgebras}

As mentioned earlier, our \mintinline{haskell}{Cell}s follow Dunai's monadic stream functions (\mintinline{haskell}{MSF}s) closely.
But can they fill their footsteps completely in terms of expressiveness?
If not, which programs exactly can be represented as \mintinline{haskell}{MSF}s and which can't?
To find the answer to these questions,
let us reexamine both types.

With the help of a simple type synonym,
the \mintinline{haskell}{MSF} definition can be recast in explicit fixpoint form:

\begin{code}
type StateTransition m a b s = a -> m (b, s)

data MSF m a b = MSF (StateTransition m a b (MSF m a b))
\end{code}
This definition tells us that monadic stream functions are so-called \emph{final coalgebras} of the \mintinline{haskell}{StateTransition} functor
(for fixed \mintinline{haskell}{m}, \mintinline{haskell}{a}, and \mintinline{haskell}{b}).
An ordinary coalgebra for this functor is given by some type \mintinline{haskell}{s} and a coalgebra structure map:
\begin{code}
data Coalg m a b where
  Coalg
    :: s
    -> (s -> StateTransition m a b s)
    -> Coalg m a b
\end{code}
But hold on, the astute reader will intercept,
is this not simply the definition of \mintinline{haskell}{Cell}?
Alas, it is not, for it lacks the type class restriction \mintinline{haskell}{Data s},
which we need so dearly for the type migration.
Any cell is a coalgebra,
but only those coalgebras that satisfy this type class are a cell.

Oh, if only there were no such distinction.
By the very property of the final coalgebra,
we can embed every coalgebra therein:
\begin{code}
finality :: Monad m => Coalg m a b -> MSF m a b
finality (Coalg state step) = MSF $ \a -> do
  (b, state') <- step state a
  return (b, finality $ Coalg state' step)
\end{code}
And analogously, every cell can be easily made into an \mintinline{haskell}{MSF} without loss of information:
\begin{code}
finalityC :: Monad m => Cell m a b -> MSF m a b
finalityC Cell { .. } = MSF $ \a -> do
  (b, cellState') <- cellStep cellState a
  return (b, finalityC $ Cell cellState' cellStep)
\end{code}
And the final coalgebra is of course a mere coalgebra itself:
\begin{code}
unMSF
  :: MSF m a b
  -> StateTransition m a b (MSF m a b)
unMSF (MSF f) a = f a
\end{code}
\begin{code}
coalgebra :: MSF m a b -> Coalg m a b
coalgebra msf = Coalg msf unMSF
\end{code}
But we miss the abilty to encode \mintinline{haskell}{MSF}s as \mintinline{haskell}{Cell}s by just the \mintinline{haskell}{Data} type class:
\begin{code}
coalgebraC
  :: Data (MSF m a b)
  => MSF m a b
  -> Cell m a b
coalgebraC msf = Cell msf unMSF
\end{code}
We are out of luck if we would want to derive an instance of \mintinline{haskell}{Data (MSF m a b)}.
Monadic stream functions are, well, functions,
and therefore have no \mintinline{haskell}{Data} instance.
The price of \mintinline{haskell}{Data} is loss of higher-order state.
Just how big this loss is will be demonstrated in the following section.

\section{Control flow}
\label{sec:control flow}

Although we now have the tools to build big signal pathways from single cells,
we have no way yet to let the incoming data decide which of several offered pathways to take.
We are lacking \emph{control flow}.

The primeval arrowized FRP framework Yampa caters for this requirement by means of switching from a signal function to another if an event occurs.
\fxerror{reference}
\fxwarning{Possibly I've mentioned both earlier}
Dunai, taking the monadic aspect seriously,
rediscovers switching as effect handling in the \mintinline{haskell}{Either} monad.
\fxerror{reference}
We shall see that,
although the state of a \mintinline{haskell}{Cell} is strongly restricted by the \mintinline{haskell}{Data} type class,
we can get very close to this powerful approach to control flow.

\subsection{The choice of arrows}

The arrow operator \mintinline{haskell}{(***)} for parallel composition has a dual,
supplied by the \mintinline{haskell}{ArrowChoice} type class:
\begin{spec}
(+++)
  :: ArrowChoice arrow
  => arrow         a            b
  -> arrow           c            d
  -> arrow (Either a c) (Either b d)
\end{spec}
While \mintinline{haskell}{arr1 *** arr2} is expected to execute both arrows,
and to consume input and produce output for both of them,
\mintinline{haskell}{arr1 +++ arr2} executes only one of them,
depending on which input was provided.

\mintinline{haskell}{Cell}s implement this type class:
\mintinline{haskell}{cell1 +++ cell2} holds the state of both cells,
stepping only one of them forward each time.
This enables basic control flow in arrow expressions,
such as \mintinline{haskell}{if}- and \mintinline{haskell}{case}-statements.
We can momentarily switch from one cell to another,
depending on live input.
However, \mintinline{haskell}{ArrowChoice} does not enable us to switch to another cell \emph{permanently}.
This issue will be addressed in the next subsection.

\begin{comment}
\begin{code}
instance Monad m => ArrowChoice (Cell m) where
  left (Cell state step) = Cell { cellState = state, .. }
    where
      cellStep cellState (Left a) = do
        (b, cellState') <- step state a
        return (Left b, cellState')
      cellStep cellState (Right b) = return (Right b, cellState)
\end{code}
\end{comment}

\fxerror{Do we need to talk about this?}
\begin{code}
keepJust
  :: (Monad m, Data a)
  => Cell m (Maybe a) (Maybe a)
keepJust = feedback Nothing $ arr keep
  where
    keep (Nothing, Nothing) = (Nothing, Nothing)
    keep (_, Just a) = (Just a, Just a)
    keep (Just a, Nothing) = (Just a, Just a)
\end{code}
