{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module LiveCoding.HandlingState where

-- base
import Control.Arrow (returnA, arr, (>>>), second)
import Control.Monad.IO.Class
import Data.Data
import Data.Foldable (traverse_)
import Data.Functor (($>))
import qualified Data.List as List

-- transformers
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Writer.Strict ( WriterT(runWriterT, WriterT), mapWriter, mapWriterT )
import Control.Monad.Trans.Accum
    ( add, look, runAccumT, AccumT(..) )

-- mtl
import Control.Monad.Writer.Class

-- containers
import Data.IntMap
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Cell.Monad
import LiveCoding.Cell.Monad.Trans
import LiveCoding.LiveProgram
import LiveCoding.LiveProgram.Monad.Trans
import LiveCoding.HandlingState.AccumTOrphan
import Control.Monad.Trans.State.Strict (StateT (StateT), runStateT, evalStateT, modify, get, put)
import Control.Monad.Morph (hoist, MFunctor)
import Control.Monad.Trans.Has

import LiveCoding.HandlingState.AccumTOrphan

data Handling h where
  Handling
    :: { key    :: Key
       , handle :: h
       }
    -> Handling h
  Uninitialized :: Handling h

type Destructors m = IntMap (Destructor m)

-- | Hold a map of registered handle keys and destructors
data HandlingState m = HandlingState
  { destructors :: Destructors m
  , registered :: [Key] -- TODO Make it an intset?
  }
  deriving Data

instance Semigroup (HandlingState m) where
  handlingState1 <> handlingState2 = HandlingState
    { destructors = destructors handlingState1 <> destructors handlingState2
    , registered = registered handlingState1 `List.union` registered handlingState2
    }

instance Monoid (HandlingState m) where
  mempty = HandlingState
    { destructors = IntMap.empty
    , registered = []
    }

-- FIXME see whether this gets easier with lenses
hoistHandlingState ::
  (forall x . m x -> n x) ->
  HandlingState m ->
  HandlingState n
hoistHandlingState morph HandlingState { .. } = HandlingState
  { destructors = (\Destructor { .. } -> Destructor { action = morph action, .. }) <$> destructors
  , ..
  }

newtype Registry = Registry
  { nHandles :: Key
  }
  deriving Data

instance Semigroup Registry where
  registry1 <> registry2 = Registry $ nHandles registry1 + nHandles registry2

instance Monoid Registry where
  mempty = Registry 0

{-
instance Monad m => Monad (MyHandlingStateT m) where
  return a = MyHandlingStateT $ return MyHandlingState
    { handlingState = mempty
    , registered = []
    }
  action >>= continuation = MyHandlingStateT $ do
    firstState <- unMyHandlingStateT action
    continuationState <- unMyHandlingStateT $ continuation $ value firstState
    let registeredLater = registered continuationState
        handlingStateEarlier = handlingState firstState <> handlingState continuationState
        handlingStateLater = handlingStateEarlier
          { destructors = destructors handlingStateEarlier `restrictKeys` IntSet.fromList registeredLater }
    return MyHandlingState
      { handlingState = handlingStateLater
      , registered = registeredLater
      }
-}

-- | In this monad, handles can be registered,
--   and their destructors automatically executed.
--   It is basically a monad in which handles are automatically garbage collected.
newtype HandlingStateT m a = HandlingStateT
  { unHandlingStateT :: AccumT Registry (WriterT (HandlingState m) m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans HandlingStateT where
  lift = HandlingStateT . lift . lift

instance MFunctor HandlingStateT where
  hoist morph
    = HandlingStateT
    . hoist (mapWriterT $ morph . fmap (second $ hoistHandlingState morph))
    . unHandlingStateT

instance Monad m => MonadWriter (HandlingState m) (HandlingStateT m) where
  writer = HandlingStateT . writer
  listen = HandlingStateT . listen . unHandlingStateT
  pass = HandlingStateT . pass . unHandlingStateT

writerToState :: (Semigroup w, Has (StateT w) m, Monad m) => WriterT w m a -> m a
writerToState (WriterT action) = do
  (a, w) <- action
  liftH $ modify (<> w)
  return a

accumToState :: (Monad m, Semigroup w, Has (StateT w) m) => AccumT w m a -> m a
accumToState (AccumT action) = do
  w <- liftH get
  (a, w') <- action w
  liftH $ put $ w <> w'
  return a
-- accumToState :: (Functor m, Semigroup w) => AccumT w m a -> StateT w m a
-- accumToState (AccumT action) = StateT $ \w -> second (w <>) <$> action w

-- FIXME rewrite with lenses
zoomStateL :: Functor m => StateT s m a -> StateT (s, s') m a
zoomStateL (StateT action) = StateT $ \(s, s') -> second (, s') <$> action s
zoomStateR :: Functor m => StateT s m a -> StateT (s', s) m a
zoomStateR (StateT action) = StateT $ \(s', s) -> second (s', ) <$> action s

-- FIXME Rename. To SimpleHandlingState? Or maybe a newtype?
-- Also this might be the right place for the division between one m and two
type HandlingStateStateT m a = AccumT Registry (StateT (HandlingState m) m) a

handlingStateWriterToState :: Monad m => HandlingStateT m a -> HandlingStateStateT m a
handlingStateWriterToState = unHandlingStateT >>> hoist (hoist lift >>> writerToState)

runHandlingStateStateT :: Monad m => HandlingStateStateT m a -> m a
runHandlingStateStateT
  = flip evalStateT mempty
  . fmap fst
  . flip runAccumT mempty

-- | Handle the 'HandlingStateT' effect _without_ garbage collection.
--   Apply this to your main loop after calling 'foreground'.
--   Since there is no garbage collection, don't use this function for live coding.
runHandlingStateT
  :: Monad m
  => HandlingStateT m a
  -> m a
runHandlingStateT
  = runHandlingStateStateT
  . handlingStateWriterToState

{- | Apply this to your main live cell before passing it to the runtime.

On the first tick, it initialises the 'HandlingState' at "no handles".

On every step, it does:

1. Unregister all handles
2. Register currently present handles
3. Destroy all still unregistered handles
   (i.e. those that were removed in the last tick)
-}
runHandlingStateC
  :: forall m a b .
     (Monad m, Typeable m)
  => Cell (HandlingStateT m) a b
  -> Cell                 m  a b
-- runHandlingStateC = hoistCell $ runHandlingStateStateT . garbageCollected . handlingStateWriterToState
runHandlingStateC
  = flip runStateC_ mempty
  . flip runStateC_ mempty
  . hoistCell (accumToState . hoist (hoist (lift @(StateT Registry))) . garbageCollected . handlingStateWriterToState)
-- runHandlingStateC cell = flip runStateC_ mempty
--   $ hoistCellOutput garbageCollected cell

-- | Like 'runHandlingStateC', but for whole live programs.
runHandlingState
  :: (Monad m, Typeable m)
  => LiveProgram (HandlingStateT m)
  -> LiveProgram                 m
-- runHandlingState = hoistLiveProgram $ runHandlingStateStateT . garbageCollected . handlingStateWriterToState
-- runHandlingState LiveProgram { .. } = flip runStateL mempty LiveProgram
--   { liveStep = garbageCollected . liveStep
--   , ..
--   }
runHandlingState LiveProgram { .. } = flip runStateL mempty $ flip runStateL mempty $ LiveProgram
  { liveStep = accumToState . hoist (hoist (lift @(StateT Registry))) . garbageCollected . handlingStateWriterToState . liveStep
  -- { liveStep = garbageCollected . handlingStateWriterToState . liveStep
  , ..
  }


-- This could simply be an action in the monad
-- Now I need mtl -- nonsense
garbageCollected
  :: Monad m
  -- => HandlingStateT m a
  -- -> HandlingStateT m a
  => HandlingStateStateT m a
  -> HandlingStateStateT m a
{-
garbageCollected actionHS = pass $ do
  (a, HandlingState { .. }) <- listen actionHS
  let registeredKeys = IntSet.fromList registered
      registeredConstructors = restrictKeys destructors registeredKeys
      unregisteredConstructors = withoutKeys destructors registeredKeys
  lift $ traverse_ action unregisteredConstructors
  return (a, const HandlingState { destructors = registeredConstructors, registered = [] })
-- garbageCollected action = unregisterAll >> action <* destroyUnregistered
garbageCollected actionHS
  = HandlingStateT
  $ AccumT
  $ \registry
  -> WriterT
  $ do
    (aAndRegistry, HandlingState { .. }) <- runWriterT $ flip runAccumT registry $ unHandlingStateT actionHS
    let registeredKeys = IntSet.fromList registered
        registeredConstructors = restrictKeys destructors registeredKeys
        unregisteredConstructors = withoutKeys destructors registeredKeys
    traverse_ action unregisteredConstructors
    return (aAndRegistry, HandlingState { destructors = registeredConstructors, registered = [] })
-}
garbageCollected actionHS = do
  a <- actionHS
  HandlingState { .. } <- liftH get
  let registeredKeys = IntSet.fromList registered
      registeredConstructors = restrictKeys destructors registeredKeys
      unregisteredConstructors = withoutKeys destructors registeredKeys
  lift $ lift $ traverse_ action unregisteredConstructors
  liftH $ put HandlingState { destructors = registeredConstructors, registered = [] }
  return a


data Destructor m = Destructor
  { isRegistered :: Bool -- TODO we don't need this anymore
  , action       :: m ()
  }

register
  :: Monad m
  => m () -- ^ Destructor
  -> HandlingStateT m Key
register action = HandlingStateT $ do
  Registry { nHandles = key } <- look
  add $ Registry 1
  tell HandlingState
    { destructors = singleton key Destructor { isRegistered = True, action }
    , registered = [key]
    }
  return key

reregister
  :: Monad m
  => m ()
  -> Key
  -> HandlingStateT m ()
reregister action key = HandlingStateT $ tell HandlingState
  { destructors = singleton key Destructor { isRegistered = True, action }
  , registered = [key]
  }

  -- Doesn't work as a single action
{-
unregisterAll
  :: Monad m
  => HandlingStateT m ()
unregisterAll = _ {- do
  HandlingState { .. } <- get
  let newDestructors = IntMap.map (\destructor -> destructor { isRegistered = False }) destructors
  put HandlingState { destructors = newDestructors, .. }
-}

destroyUnregistered
  :: Monad m
  => HandlingStateT m ()
destroyUnregistered = do
  HandlingState { .. } <- get
  let
      (registered, unregistered) = partition isRegistered destructors
  traverse_ (lift . action) unregistered
  put HandlingState { destructors = registered, .. }
-}

-- * 'Data' instances

dataTypeHandling :: DataType
dataTypeHandling = mkDataType "Handling" [handlingConstr, uninitializedConstr]

handlingConstr :: Constr
handlingConstr = mkConstr dataTypeHandling "Handling" [] Prefix

uninitializedConstr :: Constr
uninitializedConstr = mkConstr dataTypeHandling "Uninitialized" [] Prefix

instance (Typeable h) => Data (Handling h) where
  dataTypeOf _ = dataTypeHandling
  toConstr Handling { .. } = handlingConstr
  toConstr Uninitialized = uninitializedConstr
  gunfold _cons nil constructor = nil Uninitialized

dataTypeDestructor :: DataType
dataTypeDestructor = mkDataType "Destructor" [ destructorConstr ]

destructorConstr :: Constr
destructorConstr = mkConstr dataTypeDestructor "Destructor" [] Prefix

instance Typeable m => Data (Destructor m) where
  dataTypeOf _ = dataTypeDestructor
  toConstr Destructor { .. } = destructorConstr
  gunfold _ _ = error "Destructor.gunfold"
