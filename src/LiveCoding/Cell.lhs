\fxerror{If more space left, show definitions and explain}
\fxerror{Reorganise in modules properly. For now, don't worry too much.}
\begin{comment}
\begin{code}
-- | TODO: Proper haddock docs
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
module LiveCoding.Cell where

-- base
import Control.Arrow
import Control.Category
import Control.Concurrent (threadDelay)
import Control.Monad ((>=>)) -- Only for rewrite rule
import Control.Monad.Fix
import Data.Data
import Prelude hiding ((.), id)

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- essenceoflivecoding
import LiveCoding.LiveProgram

\end{code}
\end{comment}
\fxerror{The whole section, indeed FRP and Cells as a whole are overrepresented and too less motivated.
Make clear that we do this FRP approach because of modularity.
Maybe don't show the definitions of the primitives, but show the state types,
and the custom migrations implemented so that FRP reloads correctly.
Ideally, show the custom migrations as examples how users can add their own migrations.
}
In ordinary functional programming, the smallest building blocks are functions.
It stands to reason that in live coding, they should also be some flavour of functions,
in fact, \mintinline{haskell}{Arrow}s \fxfatal{Cite}.
We will see that it is possible to define bigger live programs from reusable components.

\fxerror{Combine this sentence with first sentence from nextsection}
In our definition of live programs as pairs of state and state steppers,
we can generalise the step functions to an additional input and output type:
\begin{spec}
mStep :: a -> s -> m (b, s)
\end{spec}
\begin{comment}
By now, the reader may have rightfully become weary of the ubiquitous \mintinline{haskell}{IO} monad;
and promoting it to an arbitrary monad will turn out shortly to be a very useful generalisation.
\fxerror{This has now been introduced earlier, in the WAI example, as Reader.}
\end{comment}

\subsection{Cells}
\label{sec:cells}

We collect these insights in a definition,
calling them cells,
the building blocks of everything live:

\fxerror{I haven't cited any state automaton literature.}
\begin{comment}
\begin{code}
-- | The basic building block of a live program.
\end{code}
\end{comment}
\begin{code}
data Cell m a b = forall s . Data s => Cell
  { cellState :: !s
  , cellStep  :: s -> a -> m (b, s)
  }
\end{code}
\fxwarning{Comment on bang patterns if we keep them}

Such a cell may progress by one step,
consuming an \mintinline{haskell}{a} as input,
and producing, by means of an effect in an arbitrary monad \mintinline{haskell}{m},
not only the updated cell,
but also an output datum \mintinline{haskell}{b}:

\begin{code}
step
  :: Monad m
  => Cell m a b
  -> a -> m (b, Cell m a b)
step Cell { .. } a = do
  (!b, !cellState') <- cellStep cellState a
  return (b, Cell { cellState = cellState', .. })
\end{code}

As a simple example, consider the following \mintinline{haskell}{Cell} which adds all input and returns the accumulated sum each step:
\fxwarning{Possibly mention that it's a delayed sum?}
\begin{code}
sumC :: (Monad m, Num a, Data a) => Cell m a a
sumC = Cell { .. }
  where
    cellState = 0
    cellStep accum a = return (accum + a, accum + a)
\end{code}

\fxerror{Possibly put IO later because here we can't explain yet how we'll end up with () input/output}
We recover live programs as the special case of trivial input and output,
with effects in \mintinline{haskell}{IO}:

\fxerror{Also say "we just need to elide the units".}
\begin{code}
liveCell :: Functor m => Cell m () () -> LiveProgram m
liveCell Cell {..} = LiveProgram
  { liveState = cellState
  , liveStep  = fmap snd . flip cellStep ()
  }
\end{code}
\fxerror{Isn't this just step from before?}
\fxwarning{Consider making LiveProgram not a type alias but leave it at its original type and just implement the isomorphism.
That way we can honestly reuse the machinery from section 2.}

In case our \mintinline{haskell}{Cell} is in another monad than \mintinline{haskell}{IO},
it is easy to define a function that transports a cell along a monad morphism:
\begin{code}
hoistCell
  :: (forall x . m1 x   ->      m2 x)
  ->        Cell m1 a b -> Cell m2 a b
\end{code}
For example, we may eliminate a \mintinline{haskell}{ReaderT r} context by supplying the environment,
or lift into a monad transformer:
\begin{code}
runReaderC
  ::               r
  -> Cell (ReaderT r m) a b
  -> Cell            m  a b
runReaderC r = hoistCell $ flip runReaderT r

liftCell
  :: (Monad m, MonadTrans t)
  => Cell         m  a b
  -> Cell      (t m) a b
liftCell = hoistCell lift
\end{code}
\fxerror{I haven't commented on liftCell anywhere?}
This way, we can successively handle effects until we arrive at \mintinline{haskell}{IO},
at which point we can execute the live program in the same fashion as in the last section.
\fxerror{Expand on this, possibly elsewhere. At least cite the relevant sections in the Dunai paper.}

\begin{comment}
\begin{code}
hoistCell morph Cell { .. } = Cell
  { cellStep = \s a -> morph $ cellStep s a
  , ..
  }
\end{code}
\end{comment}

\mintinline{haskell}{Cell}s can be composed in three directions:
Sequentially and parallely in the data flow sense,
and sequentially in the control flow sense.
We will address the data flow aspects in this section,
investigating control flow later in Section \ref{sec:control flow}.

By virtue of being an instance of the type class \mintinline{haskell}{Category}
for any fixed monad \mintinline{haskell}{m},
they implement sequential composition:

\begin{spec}
(>>>)
  :: Monad m
  => Cell  m a b
  -> Cell  m   b c
  -> Cell  m a   c
\end{spec}

\begin{comment}
\begin{code}
{-data Composition state1 state2 = Composition
  { state1 :: state1
  , state2 :: state2
  } deriving Data
-}

newtype Composition state1 state2 = Composition (state1, state2)
  deriving Data

instance Monad m => Category (Cell m) where
  id = Cell
    { cellState = ()
    , cellStep  = \() a -> return (a, ())
    }

  Cell state2 step2 . Cell state1 step1 = Cell { .. }
    where
      cellState = Composition (state1, state2)
      cellStep (Composition (state1, state2)) a = do
        (b, state1') <- step1 state1 a
        (c, state2') <- step2 state2 b
        return (c, Composition (state1', state2'))
{-# RULES
"arrM/>>>" forall (f :: forall a b m . Monad m => a -> m b) g . arrM f >>> arrM g = arrM (f >=> g)
#-} -- Don't really need rules here because GHC will inline all that anyways
\end{code}
\end{comment}
For two cells \mintinline{haskell}{cell1} and \mintinline{haskell}{cell2},
the composite \mintinline{haskell}{cell1 >>> cell2} holds the state of both \mintinline{haskell}{cell1} and \mintinline{haskell}{cell2},
\fxwarning{Syntax highlighting is off}
but the step function only touches each state variable individually,
the state stays encapsulated.

Composing \mintinline{haskell}{Cell}s sequentially allows us to form live programs out of \emph{sensors}, pure signal functions and \emph{actuators}:
\begin{code}
type Sensor   a   = Cell   IO         () a
type SF       a b = forall m . Cell m    a b
type Actuator   b = Cell   IO              b ()

buildLiveProg
  :: Sensor   a
  -> SF       a b
  -> Actuator   b
  -> LiveProgram IO
buildLiveProg sensor sf actuator = liveCell
  $ sensor >>> sf >>> actuator
\end{code}
\fxwarning{If this breaks, the alignment is pointless}
This will conveniently allow us to build a whole live program from smaller components.

\mintinline{haskell}{Cell}s can also be made an instance of the \mintinline{haskell}{Arrow} type class,
which allows us to lift arbitrary functions to \mintinline{haskell}{Cell}s:
\begin{spec}
arr
  :: Monad m
  ->         (a -> b)
  -> Cell  m  a    b
\end{spec}
Let us, just for the sake of the example,
assume that we will execute all \mintinline{haskell}{Cell}s at a certain fixed step rate,
say ten steps per second.
Then an Euler integration cell can be defined:
\begin{code}
type PerSecond a = a

stepRate :: Num a => a
stepRate = 10

integrate
  :: (Data a, Fractional a, Monad m)
  => Cell m a (PerSecond a)
integrate = arr (/ stepRate) >>> sumC
\end{code}

The \mintinline{haskell}{Arrow} type class also allows for data-parallel composition:
\begin{spec}
(***)
  :: Monad m
  => Cell  m  a      b
  -> Cell  m     c      d
  -> Cell  m (a, c) (b, d)
\end{spec}
Again, the state type of the composed cell is the product type of the constituent states.

Beyond standard arrows, a \mintinline{haskell}{Cell} can encode effects in a monad,
so it is not surprising that Kleisli arrows can be lifted:
\begin{spec}
arrM
  :: Monad m
  ->         (a -> m b)
  -> Cell  m  a      b
\end{spec}
Mere monadic actions become a special case thereof:
\begin{spec}
constM
  :: Monad m
  ->       m   b
  -> Cell  m a b
\end{spec}

\begin{comment}
\begin{code}
--data Parallel s1 s2 = Parallel s1 s2
newtype Parallel s1 s2 = Parallel (s1, s2)
  deriving Data

instance Monad m => Arrow (Cell m) where
  arr f = Cell
    { cellState = ()
    , cellStep  = \() a -> return (f a, ())
    }

  Cell state1 step1 *** Cell state2 step2 = Cell { .. }
    where
      cellState = Parallel (state1, state2)
      cellStep (Parallel (state1, state2)) (a, c) = do
        (b, state1') <- step1 state1 a
        (d, state2') <- step2 state2 c
        return ((b, d), Parallel (state1', state2'))

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
\fxerror{This is unclear. Either have to stress that it's maybe not so nice to encapsulate state by explicitly building the Cell, like we have done with the sum, or move feedback at the beginning of the section.}
The most general such construction is the feedback loop:
\begin{code}
-- data Feedback s s' = Feedback s s'
newtype Feedback s s' = Feedback (s, s')
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
    cellState = Feedback (state, s)
    cellStep (Feedback (state, s)) a = do
      ((b, s'), state') <- step state (a, s)
      return (b, Feedback (state', s'))

instance MonadFix m => ArrowLoop (Cell m) where
  loop (Cell state step) = Cell { .. }
    where
      cellState = state
      cellStep state a = do
        rec ((b, c), state') <- step state (a, c)
        return (b, state')
\end{code}
\end{comment}
\fxwarning{Say that ArrowLoop exists and isn't the same as feedback}
It enables us to write delays:
\begin{code}
delay :: (Data s, Monad m) => s -> Cell m s s
delay s = feedback s $ arr swap
  where
    swap (sNew, sOld) = (sOld, sNew)
\end{code}
\mintinline{haskell}{feedback} can be used for accumulation of data.
For example, \mintinline{haskell}{sumC} now becomes:
\begin{code}
sumFeedback
  :: (Monad m, Num a, Data a)
  => Cell m a a
sumFeedback = feedback 0 $ arr
  $ \(a, accum) -> (a + accum, a + accum)
\end{code}
\fxwarning{Depending on implementation use let again (and above)}
\fxwarning{It's more concise maybe if we return accum and not accum'? Then we can elide the let?}
\fxwarning{Possibly remark on Data instance of s?}

Making use of the \mintinline{haskell}{Arrows} syntax extension,
\mintinline{haskell}{feedback} is enough to implement a harmonic oscillator that will produce a sine wave:
\begin{code}
sine
  :: Monad m
  => Double -> Cell m () Double
sine k = feedback 5.5 $ proc ((), pos) -> do
  let acc = - k * pos
  vel <- integrate -< acc
  pos <- integrate -< vel
  returnA -< (pos, pos)
\end{code}
\fxerror{I might have changed the implementation here.
So update readouts. Or just include them as files and have a makefile to update them
Also, just calculate the correct value such that the amplitude is 1.}
By the laws of physics, velocity is the integral of accelleration,
and position is the integral of velocity.
In a harmonic oscillator, the acceleration is in the negative direction of the position,
multiplied by a spring factor which can be given as an argument.
The integration arrow encapsulates the current position and velocity of the oscillator as internal state, and return the position.
By using \mintinline{haskell}{feedback},
we also store the current acceleration,
which is used to give the oscillator initial energy.
\fxwarning{Two aspects mixed up here.}

The sine generator could in principle be used in an audio or video application.
For simplicity, we choose to visualise the signal on the console instead,
with our favourite Haskell operator varying its horizontal position:
\begin{code}
simpleASCIIArt :: Double -> String
simpleASCIIArt n = replicate (round $ 10 * (n + 1)) ' ' ++ ">>="
\end{code}
\fxwarning{The 0.5 and the 10 up in the sine definitions are magical numbers}
Our first complete live program,
written in FRP, is ready:
\begin{code}
printSine :: Double -> LiveProgram IO
printSine k = liveCell
  $   sine k
  -- >>> arr simpleASCIIArt
  -- >>> arrM putStrLn
  >>> arrM print
  >>> constM (threadDelay 100000)
\end{code}
Executing \mintinline{haskell}{printSine 1} gives:
\fxwarning{At least ASCII art. Maybe mention that we could use this in gloss, audio or whatever?}
\begin{verbatim}
0.1
0.19371681469282043
0.27526204294732526
0.33951204696312137
0.38242987992802596
0.4013189348670283
0.39499237745553595
0.363847717019278
0.3098418302867564
0.236368007198161
0.14804274421041758
5.041568127009974e-2
-5.0379092348295504e-2
-0.14800845423837222
-0.2363381706783599
-0.3098183219030279
[...]
\end{verbatim}

What if we want to change the spring factor, and thus the frequency, in mid-execution?
This is exactly what the framework was designed for.
We adjust the setup slightly such that every time,
a line is entered on the console,
\fxerror{Show how this is done}
the live environment inserts alternatingly \mintinline{haskell}{printSine 10} and \mintinline{haskell}{printSine 3}.
Let us execute it:
\begin{verbatim}
0.1
0.13716814692820414
8.815100531717399e-2
-1.625304643605388e-2
-0.11044500793288962
-0.1352423243201989
-7.506438219975914e-2
3.227790225368456e-2
0.11933938258843528
0.13141771739843203
6.092386510233805e-2
-4.78495806005161e-2
-0.12655824812498345

-0.12574802313732233
-0.10123485420816215
-5.763936482294621e-2
-3.179111132609283e-3
5.1880390888476687e-2
9.716066961672634e-2
0.12412659359182965
0.12769520589433989
0.10719383895267245
6.648690939317684e-2
\end{verbatim}
It is clearly visible how the period of the oscillator changed,
while its position (or, in terms of signal processing, its phase)
does not jump!
If we use the oscillator in an audio application,
we can retune it without hearing a glitch.

\subsection{Monadic stream functions and final coalgebras}

\label{sec:msfs and final coalgebras}

As mentioned earlier, our \mintinline{haskell}{Cell}s follow Dunai's monadic stream functions (\mintinline{haskell}{MSF}s) closely.
But can they fill their footsteps completely in terms of expressiveness?
If not, which programs exactly can be represented as \mintinline{haskell}{MSF}s and which can't?
To find the answer to these questions,
let us reexamine both types.

With the help of a simple type synonym,
the \mintinline{haskell}{MSF} definition can be recast in explicit fixpoint form:

\fxwarning{Maybe a record for MSF}
\begin{code}
type StateTransition m a b s = a -> m (b, s)

data MSF m a b = MSF
  { unMSF :: StateTransition m a b (MSF m a b)
  }
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

\begin{comment}
\subsection{Initial algebras}

\begin{code}
type AlgStructure m a b s = StateTransition m a b s -> s
data Alg m a b where
  Alg
    :: s
    -> AlgStructure m a b s
    -> Alg m a b

algMSF :: MSF m a b -> Alg m a b
algMSF msf = Alg msf MSF

-- TODO Could explain better why this is simpler in the coalgebra case
initiality
  :: Functor m
  => AlgStructure m a b s
  -> MSF m a b
  -> s
initiality algStructure = go
  where
    go msf = algStructure $ \a -> second go <$> unMSF msf a

\end{code}
\end{comment}

\section{Control flow}
\label{sec:control flow}

Although we now have the tools to build big signal pathways from single cells,
we have no way yet to let the incoming data decide which of several offered pathways to take.
We are lacking \emph{control flow}.

The primeval arrowized FRP framework Yampa \cite{Yampa} caters for this requirement by means of switching from a signal function to another if an event occurs.
\fxwarning{Possibly I've mentioned both earlier}
Dunai \cite{Dunai}, taking the monadic aspect seriously,
rediscovers switching as effect handling in the \mintinline{haskell}{Either} monad.
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
\end{comment}
