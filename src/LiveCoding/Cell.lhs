\fxfatal{If more space left, show definitions and explain}
\begin{comment}
\begin{code}
-- | TODO: Proper haddock docs
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
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
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

\end{code}
\end{comment}

\section{Livecoding as arrowized FRP}

\subsection{Arrowized FRP with effects}

Writing out the complete state of the live program explicitly is, of course, tedious.
Worse even, it prevents us from writings programs in a modular fashion.
But composing simple reusable building blocks is a key tenet in functional programming which we would not want to miss.
Indeed, we would want these building blocks to be some flavour of functions!
In our definition of live programs as pairs of state and state steppers,
we can generalise the step functions to an additional input and output type:
\begin{spec}
ioStep :: a -> s -> IO (b, s)
\end{spec}
The reader may have rightfully become weary of the ubiquitous \mintinline{haskell}{IO} monad;
and promoting it to an arbitrary monad will turn out shortly to be a very useful generalisation.

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
sumC :: (Monad m, Num a) => Cell m a a
sumC = Cell { .. }
  where
    cellState = 0
    cellStep accum a = let accum' = accum + a in return (accum', accum')
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
delay :: Data s => s -> Cell m s s
delay s = feedback s $ arr $ \(sNew, sOld) -> (sOld, sNew)
\end{code}
\mintinline{haskell}{feedback} can be used for accumulation of data.
For example, \mintinline{haskell}{sumC} now becomes:
\begin{code}
sumFeedback :: (Monad m, Num a) => Cell m a a
sumFeedback = feedback 0 $ arr $ \(a, accum) -> let accum' in (accum', accum')
\end{code}

\begin{code}
instance Monad m => ArrowChoice (Cell m) where
  left (Cell state step) = Cell { cellState = state, .. }
    where
      cellStep cellState (Left a) = do
        (b, cellState') <- step state a
        return (Left b, cellState')
      cellStep cellState (Right b) = return (Right b, cellState)

data ExceptState e s1 s2
    = NotYetThrown s1 s2
    | Thrown e s2
  deriving (Typeable, Data)

(>>>=)
  :: (Data e1, Monad m)
  => Cell (ExceptT e1             m)  a b
  -> Cell (ReaderT e1 (ExceptT e2 m)) a b
  -> Cell             (ExceptT e2 m)  a b
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

andThen
  :: (Data e1, Monad m)
  => Cell (ExceptT  e1      m) a b
  -> Cell (ExceptT      e2  m) a b
  -> Cell (ExceptT (e1, e2) m) a b
cell1 `andThen` cell2 = cell1 >>>= hoistCell readException cell2
  where
    readException exception = ReaderT $ \e1 -> withExceptT (e1, ) exception

data ForeverE e s = ForeverE
  { lastException :: e
  , initState     :: s
  , currentState  :: s
  }
  deriving Data

foreverE
  :: (Monad m, Data e)
  => e
  -> Cell (ReaderT e (ExceptT e m)) a b
  -> Cell m a b
foreverE e (Cell state step) = Cell { .. }
  where
    cellState = ForeverE
      { lastException = e
      , initState     = state
      , currentState  = state
      }
    cellStep f@ForeverE { .. } a = do
      continueExcept <- runExceptT $ runReaderT (step currentState a) lastException
      case continueExcept of
        Left e' -> cellStep f { lastException = e', currentState = initState } a
        Right (b, state') -> return (b, f { currentState = state' })

foreverC
  :: (Data e, Monad m)
  => Cell (ExceptT e m) a b
  -> Cell            m  a b
foreverC cell = foreverE () $ liftCell $ hoistCell (withExceptT $ const ()) cell

\end{code}
