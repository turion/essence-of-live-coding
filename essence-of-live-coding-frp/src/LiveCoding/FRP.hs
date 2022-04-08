{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : LiveCoding.FRP
-- Description : FRP continuous-time combinators for essence-of-live-coding.
-- Copyright   : (c) Iván Perez, Manuel Bärenz, and Miguel Negrão, 2022
-- Maintainer  : miguel.negrao@friendlyvirus.org
-- Stability   : experimental
-- Portability : POSIX, Windows
--
-- This package is a port of [bearriver](https://hackage.haskell.org/package/bearriver) to
-- [essence-of-livecoding](https://hackage.haskell.org/package/essence-of-live-coding).
-- Bearriver is a reimplementation of [Yampa](https://hackage.haskell.org/package/Yampa)
-- using monadic stream functions, built on top of [dunai](https://hackage.haskell.org/package/dunai).
-- essence-of-livecoding can be thought of as dunai with live-coding capabilities.
--
-- This module provides many combinators needed for FRP. It also provides combinators for doing
-- continuous-time FRP, such as 'integral' and 'time'. Time-based 'Cell's are implemented
-- using a @'ReaderT' Time m@ monad layer.
--
-- In this library events are represented by the 'Maybe' type instead of a custom @Event@ type.
-- This was done in order to allow using all the knowledge that most Haskell programmers already
-- have regarding the 'Maybe' type.
--
-- Correspondence between some of Yampa's tag combinators and Maybe's combinators:
--
-- +-------------------------------------+-------------+
-- | Yampa                               | Data.Maybe  |
-- +=====================================+=============+
-- |@tag :: Event a -> b -> Event b@     | @$>@        |
-- +-------------------------------------+-------------+
-- |@tagWith :: b -> Event a -> Event b@ | @<$@        |
-- +-------------------------------------+-------------+
--
-- Function names with a capital E usually involve 'Cell's which have as input or output a value of type 'Maybe'.
-- 
-- In some cases, an event can be any type @'Traversable' f => f e@, such as @'Maybe' e@, or @[e]@. This generalization
-- of some functions allows dealing with situations where only one event can happen on each tick (@'Maybe' e@), or multiple
-- events can happen in one tick (@[e]@). These functions internally use 'traverse''.
module LiveCoding.FRP
  ( -- * Basic definitions
    Time,
    ClockInfoT,

    -- * Signal functions

    -- ** Tuples
    dup,

    -- ** Initialization
    (-->),
    (>--),
    (>=-),
    initially,

    -- * Events

    --

    -- | Events in this library use the 'Maybe' type which should be familiar to all Haskell programmers.
    -- @'Just' a@ represents an event with value @a@, and 'Nothing' represents the lack of event on the current tick.

    -- ** Relation to other types
    boolToMaybe,

    -- ** Pointwise functions on events
    noEventFst,
    noEventSnd,
    attach,
    lMerge,
    rMerge,
    merge,
    mergeBy,
    mapMerge,
    mergeEvents,
    catEvents,
    joinE,
    splitE,
    filterE,
    mapFilterE,
    gate,

    -- ** Event related Cells
    never,
    now,
    after,
    repeatedly,
    afterEach,
    afterEachCat,

    -- ** Hybrid Cell m combinators
    holdM,
    arrEM,
    edge,
    edgeTag,
    edgeJust,
    edgeBy,
    edgeFrom,

    -- ** Stateful event suppression
    notYet,
    once,
    takeEvents,
    dropEvents,

    -- * Discrete to continuous-time signal functions

    -- ** Accumulators
    traverseHold,
    accum,
    accumHold,
    accumBy,
    accumHoldBy,

    -- ** Delayed versions

    --

    -- | Accumulators which only output the new state value on a state change on the next tick.
    daccum,
    daccumHold,
    daccumBy,
    daccumHoldBy,

    -- * Time, integration and differentiation
    time,
    integral,
    integralFrom,
    derivative,
    runClockInfoC,

    -- * Noise (random signal) sources and stochastic event sources
    getRandomRS,
    occasionally,

    -- * Switching
    catchC,
    throwEC,
    dthrowEC,
    switch,
    dSwitch,

    -- * VectorSpace
    module X,
  )
where

-- Some to this code is adapted from code in
-- https://github.com/ivanperez-keera/dunai/tree/develop/dunai-frp-bearriver

-- The license for code copied from beariver is:

-- Copyright (c) 2016, Iván Perez and Manuel Bärenz

-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:

--       Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.

--       Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.

--        Neither the name of Iván Perez and Manuel Bärenz nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{- ORMOLU_DISABLE -}

-- base
import Control.Applicative ( Applicative(liftA2) )
import qualified Control.Category as Category
import Control.Monad.IO.Class ( MonadIO(..) )
import qualified Control.Monad.Random as R
import Control.Monad.Trans.Reader ( ask, ReaderT )
import Data.Foldable (find)
import qualified Data.List.NonEmpty as NE

-- simple-affine-space
-- exported for convenience
import Data.VectorSpace as X

-- clock
import System.Clock
  ( Clock (Monotonic),
    TimeSpec (TimeSpec),
    diffTimeSpec,
    getTime,
    toNanoSecs,
  )

-- arrow-utils
import Control.Arrow.Utils (constantly)

-- essence-of-live-coding
import LiveCoding hiding (edge, once)

{- ORMOLU_ENABLE -}

infixr 0 -->, >--, >=-

-- * Basic definitions

type Time = Double

type ClockInfoT m = ReaderT Time m

-- | Due to parametricity there's only one way this function can be implemented.
-- Yup, it does what you think it does.
dup :: b -> (b, b)
dup b = (b, b)

-- Initialization

-- | Initialization operator (cf. Lustre/Lucid Synchrone).
--
-- The output on the first tick is the first argument, and from
-- that point on it behaves like the cell passed as
-- second argument.
--
-- Identical to
--
-- @
-- cell >>> 'initially' b0
-- @
(-->) :: Monad m => b -> Cell m a b -> Cell m a b
b0 --> cell = cell >>> initially b0

-- | Input initialization operator.
--
-- The input on the first tick is the first argument, and from
-- that point on it behaves like the signal function passed as
-- second argument.
--
-- Identical to
--
-- @
-- 'initially' a0 >>> cell
-- @
(>--) :: Monad m => a -> Cell m a b -> Cell m a b
a0 >-- cell = initially a0 >>> cell

-- | Transform initial input value.
--
-- Applies a transformation f only to the first input value on the first tick.
(>=-) :: Monad m => (a -> a) -> Cell m a b -> Cell m a b
f >=- cell = dSwitch (arr f >>> cell >>> arr (,Just ())) (const cell)

-- | On first tick output @a@, on subsequent ticks act as the identity arrow.
initially :: Monad m => a -> Cell m a a
initially a = dSwitch (arr $ const (a, Just ())) (const $ arr id)

-- | Event source that never occurs.
--
-- Identical to
--
-- @
-- 'constantly' Nothing
-- @
never :: Monad m => Cell m a (Maybe b)
never = constantly Nothing

-- | Event source with a single occurrence on the firs tick. The value of the event
-- is given by the function argument.
now :: Monad m => b -> Cell m a (Maybe b)
now b0 = Just b0 --> never

after ::
  Monad m =>
  -- | The time /q/ after which the event should be produced
  Time ->
  -- | Value to produce at that time
  b ->
  Cell (ClockInfoT m) a (Maybe b)
after waitTime x = Cell {cellState = (False, waitTime), ..}
  where
    cellStep (False, t) _ = do
      dt <- ask
      let t' = t - dt
      return $ if t > 0 && t' < 0 then (Just x, (True, t')) else (Nothing, (False, t'))
    cellStep (True, _) _ = return (Nothing, (False, 0))

-- | Event source with repeated occurrences with interval q. Note: If the interval is too short w.r.t.
-- the sampling intervals, the result will be that events occur at every sample. However, no more than
-- one event results from any sampling interval, thus avoiding an "event backlog" should sampling become
-- more frequent at some later point in time.
repeatedly :: (Monad m, Data b) => Time -> b -> Cell (ClockInfoT m) a (Maybe b)
repeatedly q x
  | q > 0 = afterEach qxs
  | otherwise = error "bearriver: repeatedly: Non-positive period."
  where
    qxs = (q, x) : qxs

-- | Event source with consecutive occurrences at the given intervals.
-- Should more than one event be scheduled to occur in any sampling interval,
-- only the first will in fact occur to avoid an event backlog.

-- After all, after, repeatedly etc. are defined in terms of afterEach.
afterEach :: (Monad m, Data b) => [(Time, b)] -> Cell (ClockInfoT m) a (Maybe b)
afterEach qxs = afterEachCat qxs >>> arr (fmap head)

-- | Event source with consecutive occurrences at the given intervals.
-- Should more than one event be scheduled to occur in any sampling interval,
-- the output list will contain all events produced during that interval.
afterEachCat :: (Monad m, Data b) => [(Time, b)] -> Cell (ClockInfoT m) a (Maybe [b])
afterEachCat = afterEachCat' 0
  where
    afterEachCat' :: (Monad m, Data b) => Time -> [(Time, b)] -> Cell (ClockInfoT m) a (Maybe [b])
    afterEachCat' _ [] = never
    afterEachCat' t qxs = Cell {cellState = (qxs, t), ..}
    cellStep (qxs, t) _ = do
      dt <- ask
      let t' = t + dt
          (qxsNow, qxsLater) = span (\p -> fst p <= t') qxs
          ev = if null qxsNow then Nothing else Just (map snd qxsNow)
      return (ev, (qxsLater, t'))

-- Events

-- Relation to other types

-- | defined as
--
-- @
-- boolToMaybe True = Just ()
-- boolToMaybe False = Nothing
-- @
boolToMaybe :: Bool -> Maybe ()
boolToMaybe True = Just ()
boolToMaybe False = Nothing

-- Hybrid Cell m combinators

-- | Similar to @hold@ but first value is obtained via an action in the @m@ 'Monad'.
holdM :: (Monad m, Data a) => Cell m (m a, Maybe a) a
holdM = Cell {cellState = Nothing, ..}
  where
    f x = (x, Just x)
    cellStep _ (_, Just x) = (return . f) x
    cellStep Nothing (initAction, Nothing) = do
      newState <- initAction
      (return . f) newState
    cellStep (Just state) (_, Nothing) = (return . f) state

-- | Runs the action in the @m@ 'Monad' if the input contains a value and does nothing otherwise.
arrEM ::
  (Monad m, Traversable f) =>
  (a -> m b) ->
  Cell m (f a) (f b)
arrEM = traverse' . arrM

-- | A rising edge detector. Useful for things like detecting key presses. It is initialised as up,
--  meaning that events occuring at time 0 will not be detected.
edge :: Monad m => Cell m Bool (Maybe ())
edge = edgeFrom True

-- | Like 'edge', but parameterized on the tag value.
edgeTag :: Monad m => a -> Cell m Bool (Maybe a)
edgeTag a = edge >>> arr (a <$)

-- | Edge detector particularized for detecting transtitions
--   on a 'Maybe' signal from 'Nothing' to 'Just'.
edgeJust :: (Monad m, Data a) => Cell m (Maybe a) (Maybe a)
edgeJust = edgeBy isJustEdge (Just undefined)
  where
    isJustEdge Nothing Nothing = Nothing
    isJustEdge Nothing ma@(Just _) = ma
    isJustEdge (Just _) (Just _) = Nothing
    isJustEdge (Just _) Nothing = Nothing

-- | Edge detector parameterized on the edge detection function and initial state, i.e., the
-- previous input sample. The first argument to the edge detection function is the previous
-- sample, the second the current one.
edgeBy :: (Monad m, Data a) => (a -> a -> Maybe b) -> a -> Cell m a (Maybe b)
edgeBy isEdge a_init = proc a -> do
  a_prev <- delay a_init -< a
  returnA -< isEdge a_prev a

-- | A rising edge detector that can be initialized as up (True, meaning that events
-- occurring at time 0 will not be detected) or down (False, meaning that events ocurring
-- at time 0 will be detected).
edgeFrom :: Monad m => Bool -> Cell m Bool (Maybe ())
edgeFrom init = proc a -> do
  prev <- delay init -< a
  let res
        | prev = Nothing
        | a = Just ()
        | otherwise = Nothing
  returnA -< res

-- Stateful event suppression

-- | Suppression of initial (at local time 0) event.
notYet :: Monad m => Cell m (Maybe a) (Maybe a)
notYet = initially Nothing

-- | Suppress all but the first event.
once :: (Monad m, Data a, Finite a) => Cell m (Maybe a) (Maybe a)
once = takeEvents 1

-- | Suppress all but the first n events.
takeEvents :: (Monad m, Data a) => Int -> Cell m (Maybe a) (Maybe a)
takeEvents n | n <= 0 = never
takeEvents n = dSwitch (arr (\e -> (e, () <$ e))) (const (takeEvents (n - 1)))

-- | Suppress first n events.

-- Here dSwitch or switch does not really matter.
dropEvents :: (Monad m, Data a, Finite a) => Int -> Cell m (Maybe a) (Maybe a)
dropEvents n | n <= 0 = Category.id
dropEvents n =
  dSwitch
    (never &&& Category.id)
    (const (dropEvents (n - 1)))

-- Pointwise functions on events

-- | Suppress any event in the first component of a pair.
noEventFst :: (Maybe a, b) -> (Maybe c, b)
noEventFst (_, b) = (Nothing, b)

-- | Suppress any event in the second component of a pair.
noEventSnd :: (a, Maybe b) -> (a, Maybe c)
noEventSnd (a, _) = (a, Nothing)

-- | Attaches an extra value to the value of an occurring event.
attach :: Maybe a -> b -> Maybe (a, b)
e `attach` b = fmap (,b) e

-- | Left-biased event merge (always prefer left event, if present).
lMerge :: Maybe a -> Maybe a -> Maybe a
lMerge = mergeBy const

-- | Right-biased event merge (always prefer right event, if present).
rMerge :: Maybe a -> Maybe a -> Maybe a
rMerge = flip lMerge

-- | Unbiased event merge: simultaneous occurrence is an error.
merge :: Maybe a -> Maybe a -> Maybe a
merge = mergeBy $ error "LiveCoding.FRP: merge: Simultaneous event occurrence."

-- | Event merge parameterized by a conflict resolution function.
mergeBy :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
mergeBy = mapMerge id id

-- | A generic event merge-map utility that maps event occurrences,
-- merging the results. The first three arguments are mapping functions,
-- the third of which will only be used when both events are present.
-- Therefore, 'mergeBy' = 'mapMerge' 'id' 'id'
mapMerge ::
  (a -> c) ->
  (b -> c) ->
  (a -> b -> c) ->
  Maybe a ->
  Maybe b ->
  Maybe c
mapMerge _ _ _ Nothing Nothing = Nothing
mapMerge lf _ _ (Just l) Nothing = Just (lf l)
mapMerge _ rf _ Nothing (Just r) = Just (rf r)
mapMerge _ _ lrf (Just l) (Just r) = Just (lrf l r)

-- | Merge a list of events; foremost event has priority.
mergeEvents :: [Maybe a] -> Maybe a
mergeEvents = foldr lMerge Nothing

-- | Collect simultaneous event occurrences; no event if none.
catEvents :: Traversable t => t (Maybe a) -> Maybe (t a)
catEvents e = if null e then Nothing else sequenceA e

-- | Join (conjunction) of two events. Only produces an event
-- if both events exist.
joinE :: Maybe a -> Maybe b -> Maybe (a, b)
joinE = liftA2 (,)

-- | Split event carrying pairs into two events.
splitE :: Maybe (a, b) -> (Maybe a, Maybe b)
splitE = NE.unzip

------------------------------------------------------------------------------
-- Maybe filtering
------------------------------------------------------------------------------

-- | Filter out events that don't satisfy some predicate.
filterE :: (a -> Bool) -> Maybe a -> Maybe a
filterE = find

-- | Combined event mapping and filtering. Note: since 'Maybe' is a 'Functor',
-- see 'fmap' for a simpler version of this function with no filtering.
mapFilterE :: (a -> Maybe b) -> Maybe a -> Maybe b
mapFilterE = (=<<)

-- | Enable/disable event occurences based on an external condition.
gate :: Maybe a -> Bool -> Maybe a
_ `gate` False = Nothing
e `gate` True = e

-- Switching

-- Basic switchers

-- | If the cell in the first argument throws an exception, then the function in the second argument is used
-- to provide a new 'Cell' to switch to. This is a more general version of 'switch'.
catchC :: (Monad m, Data e, Finite e) => Cell (ExceptT e m) a b -> (e -> Cell m a b) -> Cell m a b
catchC cell f = safely $ do
  e <- try cell
  safe $ f e

-- | Throw an exception on an event.
throwEC :: (Monad m, Traversable f) => Cell (ExceptT e m) (f e) ()
throwEC = traverse' throwC >>> constantly ()

-- | Throw an exception on an event, but only on the next tick.
-- Usefull when used with 'foreverE'.
dthrowEC :: (Monad m, Data e, Data (f e), Traversable f, Monoid (f e)) => Cell (ExceptT e m) (f e) ()
dthrowEC = delay mempty >>> traverse' throwC >>> constantly ()

-- | Basic switch.
--
-- By default, the first cell runs. Whenever the second value in the pair
-- actually is an event, the value carried by the event is used to obtain a new signal function
-- to be applied *at that time and at future times*. Until that happens, the first value in the
-- pair is produced in the output signal.
--
-- Important note: at the time of switching, the second cell is applied immediately.
-- If that second cell can also switch at time zero, then a double (nested) switch might take place.
-- If the second cell refers to the first one, the switch might take place infinitely many times and
-- never be resolved.
switch :: (Monad m, Data c, Finite c) => Cell m a (b, Maybe c) -> (c -> Cell m a b) -> Cell m a b
switch cell = catchC ef
  where
    ef = proc a -> do
      (b, me) <- liftCell cell -< a
      throwEC -< me
      returnA -< b

-- | Switch with delayed observation.
--
--  By default, the first cell will run.
--
--  Whenever the second value in the pair actually is an event, the value carried by the event is
--  used to obtain a new cell to be applied *at future times*.
--
--  Until that happens, the first value in the pair is produced in the output signal.
dSwitch :: (Monad m, Data c, Finite c) => Cell m a (b, Maybe c) -> (c -> Cell m a b) -> Cell m a b
dSwitch cell = catchC ef
  where
    ef = feedback Nothing $ proc (a, me) -> do
      throwEC -< me
      liftCell cell -< a

-- Parallel composition and switching

-- Parallel composition and switching over collections with broadcasting

-- #if (  (4) <  4 ||   (4) == 4 && (8) <  14 ||   (4) == 4 && (8) == 14 && (0) <= 3)
-- parB :: (Monad m) => [Cell m a b] -> Cell m a [b]
-- #else
-- parB :: (Functor m, Monad m) => [Cell m a b] -> Cell m a [b]
-- #endif
-- parB = widthFirst . sequenceS

-- dpSwitchB ::
--   (Functor m, Monad m, Traversable col) =>
--   col (Cell m a b) ->
--   Cell m (a, col b) (Maybe c) ->
--   (col (Cell m a b) -> c -> Cell m a (col b)) ->
--   Cell m a (col b)
-- dpSwitchB sfs sfF sfCs = Cell $ \a -> do
--   res <- T.mapM (`unMSF` a) sfs
--   let bs   = fmap fst res
--       sfs' = fmap snd res
--   (e,sfF') <- unMSF sfF (a, bs)
--   ct <- case e of
--           Maybe c -> snd <$> unMSF (sfCs sfs c) a
--           Nothing -> return (dpSwitchB sfs' sfF' sfCs)
--   return (bs, ct)

-- Discrete to continuous-time signal functions

-- Accumulators

-- | Identical to
--
-- @
-- 'traverse'' cell >>> 'hold' a
-- @
traverseHold :: (Monad m, Data c) => Cell m a c -> c -> Cell m (Maybe a) c
traverseHold cell a = traverse' cell >>> hold a

-- | Similar to 'foldC'' but the state changing function is received as a cell input. The B
--  stands for behavior, from the classical FRP nomenclature.
accumB :: (Monad m, Data a) => a -> Cell m (a -> a) a
accumB = foldC' ($)

-- | Given an initial value in an accumulator, it returns a cell that processes an event
--  carrying transformation functions. Every time an event is received, the function inside
--  it is applied to the accumulator, whose new value is outputted in an event.
accum :: (Monad m, Data a, Traversable f) => a -> Cell m (f (a -> a)) (f a)
accum = traverse' . accumB

-- | Like 'accum' but returns a behaviour with initial value @a@.
accumHold :: (Monad m, Data a) => a -> Cell m (Maybe (a -> a)) a
accumHold a = traverseHold (accumB a) a

-- | Identical to
--
-- @
-- 'traverse'' . 'foldC'' f
-- @
--
-- where
--
-- @
-- 'foldC'' :: (Data b, Monad m) => (a -> b -> b) -> b -> Cell m a b
-- @
--
-- is essence-of-live-coding's non-delayed accumulator cell.
accumBy :: (Monad m, Data b, Traversable f) => (a -> b -> b) -> b -> Cell m (f a) (f b)
accumBy f = traverse' . foldC' f

-- | Like 'accumBy' but returns a behaviour with initial value @a@.
accumHoldBy :: (Monad m, Data b) => (a -> b -> b) -> b -> Cell m (Maybe a) b
accumHoldBy f b = traverseHold (foldC' f b) b

-- Delayed versions

daccumB :: (Monad m, Data a) => a -> Cell m (a -> a) a
daccumB = foldC ($)

daccum :: (Monad m, Data a, Traversable f) => a -> Cell m (f (a -> a)) (f a)
daccum = traverse' . daccumB

daccumHold :: (Monad m, Data a) => a -> Cell m (Maybe (a -> a)) a
daccumHold a = traverseHold (daccumB a) a

daccumBy :: (Monad m, Data b, Traversable f) => (a -> b -> b) -> b -> Cell m (f a) (f b)
daccumBy f = traverse' . foldC f

daccumHoldBy :: (Monad m, Data b) => (a -> b -> b) -> b -> Cell m (Maybe a) b
daccumHoldBy f b = traverseHold (foldC' f b) b

-- Time

-- | Returns elapsed time since Cell started running in seconds.
time :: Monad m => Cell (ClockInfoT m) a Time
time = constantly 1.0 >>> integral

-- | Integration using the rectangle rule. Integrates starting from zero.
integral :: (Monad m, VectorSpace a s, Data a) => Cell (ClockInfoT m) a a
integral = integralFrom zeroVector

-- | Integrates starting from a given value using the trapezoidal rule. On first tick outputs
-- the zero vector.
integralFrom :: (Monad m, VectorSpace a s, Data a) => a -> Cell (ClockInfoT m) a a
integralFrom a0 = proc a -> do
  dt <- constM ask -< ()
  aPrev <- delay zeroVector -< a
  foldC' (^+^) a0 -< (realToFrac dt / 2) *^ (a ^+^ aPrev)

-- | A very crude version of a derivative. It simply divides the value difference
-- by the time difference. Outputs 'zeroVector' on first tick.
derivative :: (Monad m, VectorSpace a s, Data a) => Cell (ClockInfoT m) a a
derivative =
  zeroVector
    --> ( proc a -> do
            dt <- constM ask -< ()
            aOld <- delay zeroVector -< a
            returnA -< (a ^-^ aOld) ^/ realToFrac dt
        )

deriving instance Data TimeSpec

-- | On each tick obtains the current time using the 'Monotonic' clock
--  from the clock package. Subsequentaly calculates the diference to
--  the last clock value and sends that to the Reader Monad. The value
--  passed to 'ClockInfoT' is in seconds. On the first tick the value for
--  dt is 0.
--
--  This uses the 'getTime function from the clock library to obtain the
--  current time at each tick. This is why @m@ must be an instance of 'MonadIO'.
runClockInfoC :: MonadIO m => Cell (ClockInfoT m) a b -> Cell m a b
runClockInfoC cell = proc a -> do
  dt <- 0 --> dtC -< ()
  runReaderC' cell -< (dt, a)
  where
    dtC = proc () -> do
      t <- constM (liftIO $ getTime Monotonic) -< ()
      tPrev <- delay 0 -< t
      returnA -< fromIntegral (toNanoSecs (diffTimeSpec t tPrev)) / 1e9 :: Double

-- Noise

getRandomRS :: (R.MonadRandom m, R.Random b) => (b, b) -> Cell m a b
getRandomRS range = arrM (const (R.getRandomR range))

occasionally ::
  R.MonadRandom m =>
  -- | The time /q/ after which the event should be produced on average
  Time ->
  -- | Value to produce at time of event
  b ->
  Cell (ClockInfoT m) a (Maybe b)
occasionally tAvg b
  | tAvg <= 0 = error "bearriver: Non-positive average interval in occasionally."
  | otherwise = proc _ -> do
    r <- getRandomRS (0, 1) -< ()
    dt <- timeDelta -< ()
    let p = 1 - exp (- (dt / tAvg))
    returnA -< if r < p then Just b else Nothing
  where
    timeDelta :: Monad m => Cell (ClockInfoT m) a Time
    timeDelta = constM ask
