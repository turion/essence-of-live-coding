{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module LiveCoding.HandlingState where

-- base
import Control.Arrow (arr, returnA, (>>>))
import Data.Data

-- transformers
import Control.Monad.Trans.Class (MonadTrans (lift))
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

data Handling h = Handling
  { key :: Key
  , handle :: h
  }

type Destructors m = IntMap (Destructor m)

-- | Hold a map of registered handle keys and destructors
data HandlingState m = HandlingState
  { nHandles :: Key
  , destructors :: Destructors m
  }
  deriving (Data)

{- | In this monad, handles can be registered,
   and their destructors automatically executed.
   It is basically a monad in which handles are automatically garbage collected.
-}
type HandlingStateT m = StateT (HandlingState m) m

initHandlingState :: HandlingState m
initHandlingState =
  HandlingState
    { nHandles = 0
    , destructors = IntMap.empty
    }

{- | Handle the 'HandlingStateT' effect _without_ garbage collection.
   Apply this to your main loop after calling 'foreground'.
   Since there is no garbage collection, don't use this function for live coding.
-}
runHandlingStateT ::
  Monad m =>
  HandlingStateT m a ->
  m a
runHandlingStateT = flip evalStateT initHandlingState

{- | Apply this to your main live cell before passing it to the runtime.

On the first tick, it initialises the 'HandlingState' at "no handles".

On every step, it does:

1. Unregister all handles
2. Register currently present handles
3. Destroy all still unregistered handles
   (i.e. those that were removed in the last tick)
-}
runHandlingStateC ::
  forall m a b.
  (Monad m, Typeable m) =>
  Cell (HandlingStateT m) a b ->
  Cell m a b
runHandlingStateC cell =
  flip runStateC_ initHandlingState $
    hoistCellOutput garbageCollected cell

-- | Like 'runHandlingStateC', but for whole live programs.
runHandlingState ::
  (Monad m, Typeable m) =>
  LiveProgram (HandlingStateT m) ->
  LiveProgram m
runHandlingState LiveProgram {..} =
  flip
    runStateL
    initHandlingState
    LiveProgram
      { liveStep = garbageCollected . liveStep
      , ..
      }

garbageCollected ::
  Monad m =>
  HandlingStateT m a ->
  HandlingStateT m a
garbageCollected action = unregisterAll >> action <* destroyUnregistered

data Destructor m = Destructor
  { isRegistered :: Bool
  , action :: m ()
  }

register ::
  Monad m =>
  -- | Destructor
  m () ->
  HandlingStateT m Key
register destructor = do
  HandlingState {..} <- get
  let key = nHandles + 1
  put
    HandlingState
      { nHandles = key
      , destructors = insertDestructor destructor key destructors
      }
  return key

reregister ::
  Monad m =>
  m () ->
  Key ->
  HandlingStateT m ()
reregister action key = do
  HandlingState {..} <- get
  put HandlingState {destructors = insertDestructor action key destructors, ..}

insertDestructor ::
  m () ->
  Key ->
  Destructors m ->
  Destructors m
insertDestructor action key destructors =
  let destructor = Destructor {isRegistered = True, ..}
   in insert key destructor destructors

unregisterAll ::
  Monad m =>
  HandlingStateT m ()
unregisterAll = do
  HandlingState {..} <- get
  let newDestructors = IntMap.map (\destructor -> destructor {isRegistered = False}) destructors
  put HandlingState {destructors = newDestructors, ..}

destroyUnregistered ::
  Monad m =>
  HandlingStateT m ()
destroyUnregistered = do
  HandlingState {..} <- get
  let
    (registered, unregistered) = partition isRegistered destructors
  traverse_ (lift . action) unregistered
  put HandlingState {destructors = registered, ..}

-- * 'Data' instances
dataTypeDestructor :: DataType
dataTypeDestructor = mkDataType "Destructor" [destructorConstr]

destructorConstr :: Constr
destructorConstr = mkConstr dataTypeDestructor "Destructor" [] Prefix

instance Typeable m => Data (Destructor m) where
  dataTypeOf _ = dataTypeDestructor
  toConstr Destructor {..} = destructorConstr
  gunfold _ _ = error "Destructor.gunfold"
