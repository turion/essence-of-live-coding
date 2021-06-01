{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module LiveCoding.HandlingState where

-- base
import Control.Arrow (returnA, arr, (>>>))
import Data.Data

-- transformers
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.State.Strict
import Data.Foldable (traverse_)

-- containers
import Data.IntMap
import qualified Data.IntMap as IntMap

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Cell.Monad
import LiveCoding.Cell.Monad.Trans
import LiveCoding.LiveProgram
import LiveCoding.LiveProgram.Monad.Trans
import qualified Data.IntSet as IntSet

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
  { nHandles    :: Key
  , destructors :: Destructors m
  }
  deriving Data

-- | In this monad, handles can be registered,
--   and their destructors automatically executed.
--   It is basically a monad in which handles are automatically garbage collected.
type HandlingStateT m = StateT (HandlingState m) m

instance Semigroup (HandlingState m) where
  handlingState1 <> handlingState2 = HandlingState
    { nHandles = nHandles handlingState1 `max` nHandles handlingState2
    , destructors = destructors handlingState1 <> destructors handlingState2
    }

data MyHandlingState m a = MyHandlingState
  { handlingState :: HandlingState m
  , registered :: [Key]
  , value :: a
  }
  deriving Functor

newtype MyHandlingStateT m a = MyHandlingStateT
  { unMyHandlingStateT :: m (MyHandlingState m a) }
  deriving Functor

instance Monad m => Monad (MyHandlingStateT m) where
  return a = MyHandlingStateT $ return MyHandlingState
    { handlingState = initHandlingState
    , registered = []
    , value = a
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
      , value = value continuationState
      }

initHandlingState :: HandlingState m
initHandlingState = HandlingState
  { nHandles = 0
  , destructors = IntMap.empty
  }

-- | Handle the 'HandlingStateT' effect _without_ garbage collection.
--   Apply this to your main loop after calling 'foreground'.
--   Since there is no garbage collection, don't use this function for live coding.
runHandlingStateT
  :: Monad m
  => HandlingStateT m a
  -> m a
runHandlingStateT = flip evalStateT initHandlingState

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
runHandlingStateC cell = flip runStateC_ initHandlingState
  $ hoistCellOutput garbageCollected cell

-- | Like 'runHandlingStateC', but for whole live programs.
runHandlingState
  :: (Monad m, Typeable m)
  => LiveProgram (HandlingStateT m)
  -> LiveProgram                 m
runHandlingState LiveProgram { .. } = flip runStateL initHandlingState LiveProgram
  { liveStep = garbageCollected . liveStep
  , ..
  }

garbageCollected
  :: Monad m
  => HandlingStateT m a
  -> HandlingStateT m a
garbageCollected action = unregisterAll >> action <* destroyUnregistered

data Destructor m = Destructor
  { isRegistered :: Bool
  , action       :: m ()
  }


register
  :: Monad m
  => m () -- ^ Destructor
  -> HandlingStateT m Key
register destructor = do
  HandlingState { .. } <- get
  let key = nHandles + 1
  put HandlingState
    { nHandles = key
    , destructors = insertDestructor destructor key destructors
    }
  return key

reregister
  :: Monad m
  => m ()
  -> Key
  -> HandlingStateT m ()
reregister action key = do
  HandlingState { .. } <- get
  put HandlingState { destructors = insertDestructor action key destructors, .. }

insertDestructor
  :: m ()
  -> Key
  -> Destructors m
  -> Destructors m
insertDestructor action key destructors =
  let destructor = Destructor { isRegistered = True, .. }
  in  insert key destructor destructors

unregisterAll
  :: Monad m
  => HandlingStateT m ()
unregisterAll = do
  HandlingState { .. } <- get
  let newDestructors = IntMap.map (\destructor -> destructor { isRegistered = False }) destructors
  put HandlingState { destructors = newDestructors, .. }

destroyUnregistered
  :: Monad m
  => HandlingStateT m ()
destroyUnregistered = do
  HandlingState { .. } <- get
  let
      (registered, unregistered) = partition isRegistered destructors
  traverse_ (lift . action) unregistered
  put HandlingState { destructors = registered, .. }

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
