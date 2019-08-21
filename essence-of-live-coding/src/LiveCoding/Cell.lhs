\fxerror{If more space left, show definitions and explain}
\fxerror{Reorganise in modules properly. For now, don't worry too much.}
\begin{comment}
\begin{code}
-- | TODO: Proper haddock docs
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module LiveCoding.Cell where

-- base
import Control.Arrow
import Control.Category
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Fix
import Data.Data
import Prelude hiding ((.), id)

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- essence-of-live-coding
import LiveCoding.LiveProgram

\end{code}
\end{comment}
\fxerror{
Is it clear that we do this FRP approach because of modularity,
both in the program definitions and also in the state types?
Maybe don't show the definitions of the primitives, but show the state types,
and the custom migrations implemented so that FRP reloads correctly.
Ideally, show the custom migrations as examples how users can add their own migrations.
The main connective could be that Cells build up their state automatically in a way that the migration works well.
(Test)
}
\fxerror{An important point along those lines would also be that the state type becomes a tree,
branching at \mintinline{haskell}{>>>} and \mintinline{haskell}{***} and \mintinline{haskell}{+++},
so individual subtrees are preserved well
}
In ordinary functional programming, the smallest building blocks are functions.
It stands to reason that in live coding, they should also be some flavour of functions,
in fact, \mintinline{haskell}{Arrow}s \cite{Arrows}.
We will see that it is possible to define bigger live programs from reusable components.
Crucially, the library user is disburdened from separating state and step function.
The state type is built up behind the scenes,
in a manner compatible with the automatic state migration.

\subsection{Cells}
\label{sec:cells}

In our definition of live programs as pairs of state and state steppers,
we can generalise the step functions to an additional input and output type.
\begin{comment}
\begin{spec}
mStep :: a -> s -> m (b, s)
\end{spec}
By now, the reader may have rightfully become weary of the ubiquitous \mintinline{haskell}{IO} monad;
and promoting it to an arbitrary monad will turn out shortly to be a very useful generalisation.
\fxerror{This has now been introduced earlier, in the WAI example, as Reader.}

We collect these insights in a definition,
\end{comment}
Live programs are thus generalised to effectful \emph{Mealy machines} \cite{Mealy}.
Let us call them cells, the building blocks of everything live:
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
\begin{comment}
\begin{code}
  | ArrM { runArrM :: a -> m b }
  -- ^ Added to improve performance and keep state types simpler
\end{code}
\end{comment}
\begin{comment}
\begin{code}
-- | Converts every 'Cell' to the 'Cell' constructor.
toCell :: Functor m => Cell m a b -> Cell m a b
toCell cell@Cell {} = cell
toCell ArrM { .. } = Cell
  { cellState = ()
  , cellStep  = const $ fmap (, ()) . runArrM
  }
\end{code}
\end{comment}
Such a cell may progress by one step,
consuming an \mintinline{haskell}{a} as input,
and producing, by means of an effect in some monad \mintinline{haskell}{m},
not only the updated cell,
but also an output datum \mintinline{haskell}{b}:

\begin{code}
step
  :: Monad m
  => Cell m a b
  -> a -> m (b, Cell m a b)
step Cell { .. } a = do
  (b, cellState') <- cellStep cellState a
  return (b, Cell { cellState = cellState', .. })
\end{code}
\begin{comment}
\begin{code}
step cell@ArrM { .. } a = ( , cell) <$> runArrM a
\end{code}
\end{comment}

\begin{comment}
\begin{code}
steps
  :: Monad m
  => Cell m a b
  -> [a]
  -> m ([b], Cell m a b)
steps cell [] = return ([], cell)
steps cell (a : as) = do
  (b, cell') <- step cell a
  (bs, cell'') <- steps cell' as
  return (b : bs, cell'')
\end{code}
\end{comment}

As a simple example, consider the following \mintinline{haskell}{Cell} which adds all input and returns the delayed sum each step:
\begin{code}
sumC :: (Monad m, Num a, Data a) => Cell m a a
sumC = Cell { .. }
  where
    cellState = 0
    cellStep accum a = return (accum, accum + a)
\end{code}

We recover live programs as the special case of trivial input and output:
\begin{code}
liveCell
  :: Functor     m
  => Cell        m () ()
  -> LiveProgram m
liveCell Cell { .. } = LiveProgram
  { liveState = cellState
  , liveStep  = fmap snd . flip cellStep ()
  }
\end{code}
\begin{comment}
\begin{code}
liveCell ArrM { .. } = LiveProgram
  { liveState = ()
  , liveStep  = runArrM
  }
\end{code}
\end{comment}
\begin{comment}
\begin{code}
toLiveCell
  :: Functor     m
  => LiveProgram m
  -> Cell        m () ()
toLiveCell LiveProgram { .. } = Cell
  { cellState = liveState
  , cellStep  = \s () -> ((), ) <$> liveStep s
  }
\end{code}
\end{comment}

\subsection{FRP for automata-based programming}
Effectful Mealy machines, here cells,
offer a wide variety of applications in FRP.
The essential parts of the API,
which is heavily inspired by the FRP library \texttt{dunai}
\cite{Dunai},
is shown here.
%\mintinline{haskell}{Cell}s can be composed in three directions:
%Sequentially and parallely in the data flow sense,
%and sequentially in the control flow sense.
We will address the data flow aspects in this section,
investigating control flow later in Section \ref{sec:control flow}.

\begin{comment}
\begin{code}
hoistCell morph Cell { .. } = Cell
  { cellStep = \s a -> morph $ cellStep s a
  , ..
  }
hoistCell morph ArrM { .. } = ArrM
  { runArrM = morph . runArrM
  }
\end{code}
\end{comment}

\paragraph{Composition}
By being an instance of the type class \mintinline{haskell}{Category}
for any monad \mintinline{haskell}{m},
cells implement sequential composition:
\begin{spec}
(>>>)
  :: Monad m
  => Cell  m a b
  -> Cell  m   b c
  -> Cell  m a   c
\end{spec}

\begin{comment}
\begin{code}
data Composition state1 state2 = Composition
  { state1 :: state1
  , state2 :: state2
  }
  deriving Data

instance Monad m => Category (Cell m) where
  id = ArrM return

  ArrM f . ArrM g = ArrM $ f <=< g
  Cell { .. } . ArrM { .. } = Cell
    { cellStep = \state -> cellStep state <=< runArrM
    , ..
    }
  ArrM { .. } . Cell { .. } = Cell
    { cellStep = \state -> (runKleisli $ first $ Kleisli runArrM) <=< cellStep state
    , ..
    }
  Cell state2 step2 . Cell state1 step1 = Cell { .. }
    where
      cellState = Composition state1 state2
      cellStep (Composition state1 state2) a = do
        (!b, state1') <- step1 state1 a
        (!c, state2') <- step2 state2 b
        return (c, Composition state1' state2')
-- {-# RULES
-- "arrM/>>>" forall (f :: forall a b m . Monad m => a -> m b) g . arrM f >>> arrM g = arrM (f >=> g)
-- #-} -- Don't really need rules here because GHC will inline all that anyways
\end{code}
\end{comment}
For two cells \mintinline{haskell}{cell1} and \mintinline{haskell}{cell2}
with state types \mintinline{haskell}{state1} and \mintinline{haskell}{state2},
the composite \mintinline{haskell}{cell1 >>> cell2} holds a pair of both states:
\fxwarning{Syntax highlighting is not very good here}
\begin{spec}
data Composition state1 state2 = Composition
  { state1 :: state1
  , state2 :: state2
  } deriving Data
\end{spec}
The step function executes the steps of both cells after each other.
They only touch their individual state variable,
the state stays encapsulated.

\fxwarning{Reuse Sensor, SF and Actuator later?}
Composing \mintinline{haskell}{Cell}s sequentially allows us to form live programs out of \emph{sensors}, pure signal functions and \emph{actuators}:
\begin{code}
type Sensor   a   = Cell   IO         () a
type SF       a b = forall m . Cell m    a b
type Actuator   b = Cell   IO              b ()
\end{code}
\begin{code}
buildLiveProg
  :: Sensor   a
  -> SF       a b
  -> Actuator   b
  -> LiveProgram IO
buildLiveProg sensor sf actuator = liveCell
  $ sensor >>> sf >>> actuator
\end{code}
This (optional) division of the reactive program into three such parts is inspired by Yampa \cite{Yampa}.
We conveniently build a whole live program from smaller components.
It is never necessary to specify a big state type manually,
it will be composed from basic building blocks like \mintinline{haskell}{Composition}.

The migration function is easily extended such that it correctly handles the common cases where we extend a cell \mintinline{haskell}{cellMiddle} to the composition \mintinline{haskell}{cellLeft >>> cellMiddle},
or to \mintinline{haskell}{cellMiddle >>> cellRight}.

\paragraph{Arrowized FRP}
\mintinline{haskell}{Cell}s can be made an instance of the \mintinline{haskell}{Arrow} type class,
which allows us to lift pure functions to \mintinline{haskell}{Cell}s:
\begin{spec}
arr
  :: Monad m
  ->         (a -> b)
  -> Cell  m  a    b
\end{spec}
\fxwarning{Would be nice to have the space to explain *** as well!}
Together with the \mintinline{haskell}{ArrowChoice} and \mintinline{haskell}{ArrowLoop} classes
(discussed in the appendix),
cells can be used in \emph{arrow notation} \cite{ArrowNotation} with \mintinline{haskell}{case}-expressions,
\mintinline{haskell}{if then else} constructs and recursion.
The next subsection gives some examples.

An essential aspect of an FRP framework is some notion of \emph{time}.
\fxwarning{Citation?}
As this approach essentially uses the \texttt{dunai} API,
a detailed treatment of time domains and clocks as in \texttt{rhine} \cite{Rhine} could be readily applied here,
but this will be deferred to future work.
For simplicity and explicitness,
assume that we will execute all \mintinline{haskell}{Cell}s at a certain fixed step rate,
say, twenty five steps per second.
Then an Euler integration cell can be defined:
\begin{code}
stepRate :: Num a => a
stepRate = 25
\end{code}
\begin{code}
integrate
  :: (Data a, Fractional a, Monad m)
  => Cell m a a
integrate = arr (/ stepRate) >>> sumC
\end{code}
The time since activation of a cell is then famously \cite[Section 2.4]{Yampa} defined as:
\begin{code}
localTime
  :: (Data a, Fractional a, Monad m)
  => Cell m b a
localTime = arr (const 1) >>> integrate
\end{code}

\fxwarning{I cut a more detailed discussion about ArrowChoice and ArrowLoop here. Put in the appendix?}

\paragraph{Monads and their morphisms}
Beyond standard arrows, a \mintinline{haskell}{Cell} can encode effects in a monad,
so it is not surprising that Kleisli arrows can be lifted:
\begin{spec}
arrM
  :: Monad m
  ->         (a -> m b)
  -> Cell  m  a      b
\end{spec}
\begin{comment}
Mere monadic actions become a special case thereof:
\begin{spec}
constM
  :: Monad m
  ->       m   b
  -> Cell  m a b
\end{spec}
\end{comment}

In case our \mintinline{haskell}{Cell} is in another monad than \mintinline{haskell}{IO},
one can define a function that transports a cell along a monad morphism:
\begin{code}
hoistCell
  :: (forall x . m1 x   ->      m2 x)
  ->        Cell m1 a b -> Cell m2 a b
\end{code}
For example, we may eliminate a \mintinline{haskell}{ReaderT r} context by supplying the environment through the \mintinline{haskell}{runReaderT} monad morphism,
or lift into a monad transformer:
\begin{code}
liftCell
  :: (Monad m, MonadTrans t)
  => Cell         m  a b
  -> Cell      (t m) a b
liftCell = hoistCell lift
\end{code}
As described in \cite[Section 4]{Dunai},
we can successively handle effects
(such as global state, read-only variables, logging, exceptions, and others)
until we arrive at \mintinline{haskell}{IO}.
Then we can execute the live program in the same way as before.

\begin{comment}
\begin{code}
data Parallel stateP1 stateP2 = Parallel
  { stateP1 :: stateP1
  , stateP2 :: stateP2
  }
  deriving Data

instance Monad m => Arrow (Cell m) where
  arr = arrM . (return .)

  -- For efficiency because Arrow desugaring favours 'first'
  first ArrM { .. } = ArrM { runArrM = \(a, c) -> ( , c) <$> runArrM a }
  first Cell { .. } = Cell
    { cellStep = \s (a, c) -> first ((, c) $!) <$> cellStep s a
    , ..
    }

  ArrM f *** ArrM g = ArrM $ runKleisli $ Kleisli f *** Kleisli g
  ArrM { .. } *** Cell { .. } = Cell
    { cellStep = \state (a, c) -> do
      !b <- runArrM a
      (!d, state') <- cellStep state c
      return ((b, d), state')
    , ..
    }
  Cell { .. } *** ArrM { .. } = Cell
    { cellStep = \state (a, c) -> do
      (!b, state') <- cellStep state a
      !d <- runArrM c
      return ((b, d), state')
    , ..
    }
  Cell stateP1 step1 *** Cell stateP2 step2 = Cell { .. }
    where
      cellState = Parallel { .. }
      cellStep (Parallel { .. }) (a, c) = do
        (!b, stateP1') <- step1 stateP1 a
        (!d, stateP2') <- step2 stateP2 c
        return ((b, d), Parallel stateP1' stateP2')

arrM :: (a -> m b) -> Cell m a b
arrM = ArrM

constM :: m b -> Cell m a b
constM = arrM . const

constC :: Monad m => b -> Cell m a b
constC = constM . return
\end{code}
\end{comment}

\begin{comment}
\begin{code}
instance MonadFix m => ArrowLoop (Cell m) where
  loop ArrM { .. } = ArrM
    { runArrM = \a -> do
        rec (b, c) <- (\c' -> runArrM (a, c')) c
        return b
    }
  loop (Cell state step) = Cell { .. }
    where
      cellState = state
      cellStep state a = do
        rec ((b, c), state') <- (\c' -> step state (a, c')) c
        return (b, state')

{-
instance ArrowLoop (Cell Identity) where
  loop (Cell state step) = Cell { .. }
    where
      cellState = state
      changedStep state (a, c) = runIdentity $ step state (a, c)
      cellStep state a = let ((b, c), state') = (\c' -> changedStep state (a, c')) c
        in return (b, state')
-}
\end{code}
\end{comment}

\subsection{A sine generator}
Making use of the \mintinline{haskell}{Arrows} syntax extension,
we can implement a harmonic oscillator that will produce a sine wave with amplitude 10 and given period length:
\fxwarning{Comment on rec and ArrowFix}
\fxerror{I want to add a delay for numerical stability}
\begin{code}
sine
  :: MonadFix m
  => Double -> Cell m () Double
sine t = proc () -> do
  rec
    let acc = - (2 * pi / t) ^ 2 * (pos - 10)
    vel <- integrate -< acc
    pos <- integrate -< vel
  returnA -< pos
\end{code}
By the laws of physics, velocity is the integral of accelleration,
and position is the integral of velocity.
In a harmonic oscillator, the acceleration is in the negative direction of the position,
multiplied by a spring factor depending on the period length,
which can be given as an argument.
The integration arrow encapsulates the current position and velocity of the oscillator as internal state, and returns the position.

The sine generator could in principle be used in an audio or video application.
For simplicity, we choose to visualise the signal on the console instead,
with our favourite Haskell operator varying its horizontal position:
\begin{code}
asciiArt :: Double -> String
asciiArt n = replicate (round n) ' ' ++ ">>="
\end{code}
\begin{code}
printEverySecond :: Cell IO String ()
printEverySecond = proc string -> do
  count <- sumC -< 1 :: Integer
  if count `mod` stepRate == 0
    then arrM putStrLn -< string
    else returnA       -< ()
\end{code}
Our first live program
written in FRP is ready:
\begin{code}
printSine :: Double -> LiveProgram IO
printSine t = liveCell
  $   sine t
  >>> arr asciiArt
  >>> printEverySecond
\end{code}
\fxwarning{Maybe mention that we could use this in gloss, audio or whatever?}

What if we would run it,
and change the period in mid-execution?
%This is exactly what the framework was designed for.
\fxerror{Show Demo.hs as soon as I've explained the runtime in the previous section}
We execute the program such that after a certain time,
the live environment inserts \mintinline{haskell}{printSine} with a different period.
\fxerror{Actually, now that we have those fancy GHCi commands,
We can insert them instead of manually printing stuff.
Increases the immersion.
But it's actually cheating.
}
Let us execute it:\footnote{%
From now on, the GHCi commands will be suppressed.
}
\verbatiminput{../demos/DemoSine.txt}
It is clearly visible how the period of the oscillator changed,
\fxwarning{Only if this doesn't break. Maybe make figures?}
while its position (or, in terms of signal processing, its phase)
did not jump.
If we use the oscillator in an audio application,
we can retune it without hearing a glitch;
if we use it in a video application,
the widget will smoothly change its oscillating velocity without a jolt.

\section{Control flow}
\label{sec:control flow}
\fxerror{Show only stuff where I can show most of the implementation. Reimplement, in a separate file, the API for the newtype, show its code and explain it.}
Although we now have the tools to build big signal pathways from single cells,
we have no way yet to let the incoming data decide which of several offered pathways to take for the rest of the execution.
While we can (due to \mintinline{haskell}{ArrowChoice}) temporarily branch between two cells using \mintinline{haskell}{if then else},
the branching is reevaluated (and the previous choice forgotten) every step.
We are lacking permanent \emph{control flow}.

The primeval arrowized FRP framework Yampa \cite{Yampa} caters for this requirement by means of switching from a signal function to another if an event occurs.
\fxwarning{Possibly I've mentioned both earlier}
Dunai \cite[Section 5.3]{Dunai}, taking the monadic aspect seriously,
\fxwarning{Dunai, Yampa -> \texttt{Dunai} etc.?}
rediscovers switching as effect handling in the \mintinline{haskell}{Either} monad.
\begin{comment}
We shall see that,
although the state of a \mintinline{haskell}{Cell} is strongly restricted by the \mintinline{haskell}{Data} type class,
we can get very close to this powerful approach to control flow.
\end{comment}

\begin{comment}
\begin{code}
-- FIXME Why the hell is my left definition wrong or leads to the wrong instance?
data Choice stateL stateR = Choice
  { choiceLeft  :: stateL
  , choiceRight :: stateR
  }
  deriving Data
instance Monad m => ArrowChoice (Cell m) where
{-
  left (Cell state step) = Cell { cellState = state, .. }
    where
      cellStep cellState (Left a) = do
        (b, cellState') <- step state a
        return (Left b, cellState')
      cellStep cellState (Right b) = return (Right b, cellState)
      -}
  ArrM f +++ ArrM g = ArrM $ runKleisli $ Kleisli f +++ Kleisli g
  ArrM { .. } +++ Cell { .. } = Cell
    { cellStep = \state -> \case
        Left a -> do
          !b <- runArrM a
          return (Left b, state)
        Right c -> do
          (!d, state') <- cellStep state c
          return (Right d, state')
    , ..
    }
  Cell { .. } +++ ArrM { .. } = Cell
    { cellStep = \state -> \case
        Left a -> do
          (!b, state') <- cellStep state a
          return (Left b, state')
        Right c -> do
          !d <- runArrM c
          return (Right d, state)
    , ..
    }
  (Cell stateL stepL) +++ (Cell stateR stepR) = Cell { .. }
    where
      cellState = Choice stateL stateR
      cellStep (Choice stateL stateR) (Left a) = do
        (!b, stateL') <- stepL stateL a
        return (Left b, (Choice stateL' stateR))
      cellStep (Choice stateL stateR) (Right c) = do
        (!d, stateR') <- stepR stateR c
        return (Right d, (Choice stateL stateR'))
\end{code}
\end{comment}
