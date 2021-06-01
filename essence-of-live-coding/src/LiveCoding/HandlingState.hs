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
module LiveCoding.HandlingState where

-- base
import Control.Arrow (arr, returnA, (>>>))
import Control.Monad.IO.Class
import Data.Data
import Data.Foldable (traverse_)
import Data.Functor (($>))
import qualified Data.List as List

-- transformers
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Writer.Strict ( WriterT(runWriterT) )
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

data Handling h = Handling
  { key :: Key
  , handle :: h
  }

type Destructors m = IntMap (Destructor m)

-- | Hold a map of registered handle keys and destructors
data HandlingState m = HandlingState
  { destructors :: Destructors m
  , registered :: [Key] -- TODO Make it an intset?
  }
  deriving (Data)

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

newtype Registry = Registry
  { nHandles :: Key
  }

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


instance Monad m => MonadWriter (HandlingState m) (HandlingStateT m) where
  writer = HandlingStateT . writer
  listen = HandlingStateT . listen . unHandlingStateT
  pass = HandlingStateT . pass . unHandlingStateT

-- | Handle the 'HandlingStateT' effect _without_ garbage collection.
--   Apply this to your main loop after calling 'foreground'.
--   Since there is no garbage collection, don't use this function for live coding.
runHandlingStateT
  :: Monad m
  => HandlingStateT m a
  -> m a
runHandlingStateT = fmap fst . runWriterT . fmap fst . flip runAccumT mempty . unHandlingStateT

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
runHandlingStateC = hoistCell $ runHandlingStateT . garbageCollected
-- runHandlingStateC cell = flip runStateC_ mempty
--   $ hoistCellOutput garbageCollected cell

-- | Like 'runHandlingStateC', but for whole live programs.
runHandlingState
  :: (Monad m, Typeable m)
  => LiveProgram (HandlingStateT m)
  -> LiveProgram                 m
runHandlingState = hoistLiveProgram $ runHandlingStateT . garbageCollected
-- runHandlingState LiveProgram { .. } = flip runStateL mempty LiveProgram
--   { liveStep = garbageCollected . liveStep
--   , ..
--   }

-- Now I need mtl
garbageCollected
  :: Monad m
  => HandlingStateT m a
  -> HandlingStateT m a
garbageCollected actionHS = pass $ do
  (a, HandlingState { .. }) <- listen actionHS
  let registeredKeys = IntSet.fromList registered
      registeredConstructors = restrictKeys destructors registeredKeys
      unregisteredConstructors = withoutKeys destructors registeredKeys
  lift $ traverse_ action unregisteredConstructors
  return (a, const HandlingState { destructors = registeredConstructors, registered = [] })
-- garbageCollected action = unregisterAll >> action <* destroyUnregistered

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

destroyUnregistered ::
  Monad m =>
  HandlingStateT m ()
destroyUnregistered = do
  HandlingState {..} <- get
  let
    (registered, unregistered) = partition isRegistered destructors
  traverse_ (lift . action) unregistered
  put HandlingState { destructors = registered, .. }
-}

-- * 'Data' instances
dataTypeDestructor :: DataType
dataTypeDestructor = mkDataType "Destructor" [destructorConstr]

destructorConstr :: Constr
destructorConstr = mkConstr dataTypeDestructor "Destructor" [] Prefix

instance Typeable m => Data (Destructor m) where
  dataTypeOf _ = dataTypeDestructor
  toConstr Destructor {..} = destructorConstr
  gunfold _ _ = error "Destructor.gunfold"
