\begin{comment}
\begin{code}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module LiveCoding.Cell.Feedback where

-- base
import Control.Arrow
import Data.Data
import Data.Maybe (fromMaybe)

-- essence-of-live-coding
import LiveCoding.Cell
\end{code}
\end{comment}

We would like to have all basic primitives needed to develop standard synchronous signal processing components,
without touching the \mintinline{haskell}{Cell} constructor anymore.
One crucial bit is missing to achieve this goal:
Encapsulating state.
The most general such construction is the feedback loop:
\begin{code}
feedback
  :: (Monad m, Data s)
  =>                s
  -> Cell   m   (a, s) (b, s)
  -> Cell   m    a      b
\end{code}
Let us have a look at its internal state:
\begin{spec}
data Feedback sPrevious sAdditional = Feedback
  { sPrevious   :: sPrevious
  , sAdditional :: sAdditional
  }
\end{spec}
In \mintinline{haskell}{feedback sAdditional cell},
the \mintinline{haskell}{cell} has state \mintinline{haskell}{sPrevious},
and to this state we add \mintinline{haskell}{sAdditional}.
The additional state is received by \mintinline{haskell}{cell} as explicit input,
and \mintinline{haskell}{feedback} hides it.

Note that \mintinline{haskell}{feedback} and \mintinline{haskell}{loop} are different.
While \mintinline{haskell}{loop} provides immediate recursion, it doesn't add new state.
\mintinline{haskell}{feedback} requires an initial state and delays it,
but in turn it is always safe to use since it does not use \mintinline{haskell}{mfix}.

\fxwarning{Possibly remark on Data instance of s?}
\begin{comment}
\begin{code}
newtype Feedback s s' = Feedback (s, s')
  deriving Data

feedback s (Cell state step) = Cell { .. }
  where
    cellState = Feedback (state, s)
    cellStep (Feedback (state, s)) a = do
      ((b, s'), state') <- step state (a, s)
      return (b, Feedback (state', s'))
feedback cellState (ArrM f) = Cell { .. }
  where
    cellStep state a = f (a, state)
\end{code}
\end{comment}
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
  $ \(a, accum) -> (accum, a + accum)
\end{code}

\fxerror{Mention keepJust and keep}
\begin{comment}
\begin{code}
keepJust
  :: (Monad m, Data a)
  => Cell m (Maybe a) (Maybe a)
keepJust = feedback Nothing $ arr keep
  where
    keep (Nothing, Nothing) = (Nothing, Nothing)
    keep (_, Just a) = (Just a, Just a)
    keep (Just a, Nothing) = (Just a, Just a)

-- | Initialise with a value 'a'.
--   If the input is 'Nothing', @keep a@ will output the stored indefinitely.
--   A new value can be stored by inputting 'Maybe a'.
keep :: (Data a, Monad m) => a -> Cell m (Maybe a) a
keep a = feedback a $ proc (ma, aOld) -> do
  let !aNew = fromMaybe aOld ma
  returnA -< (aNew, aNew)
\end{code}
\end{comment}
