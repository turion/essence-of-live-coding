{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LiveCoding.Handle
  ( Handle (..)
  , handling
  , HandlingState (..)
  , HandlingStateT
  , isRegistered
  , runHandlingState
  , runHandlingStateC
  , runHandlingStateT
  )
  where

-- base
import Control.Arrow (returnA, arr, (>>>))
import Data.Data

-- containers
import Data.IntMap
import qualified Data.IntMap as IntMap

-- transformers
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.State.Strict

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Cell.Monad
import LiveCoding.Cell.Monad.Trans
import LiveCoding.LiveProgram
import LiveCoding.LiveProgram.Monad.Trans

{- | Container for unserialisable values,
such as 'IORef's, threads, 'MVar's, pointers, and device handles.

In a 'Handle', you can store a mechanism to create and destroy a value that survives live coding even if does not have a 'Data' instance.
Using the function 'handling', you can create a cell that will
automatically initialise your value,
and register it in the 'HandlingStateT' monad transformer,
which takes care of automatically destroying it (if necessary) when it does not occur anymore in a later revision of your live program.

Have a look at 'LiveCoding.Handle.Examples' for some ready-to-use implementations.

In short, 'Handle' is an opaque, automatically constructing and garbage collecting container for arbitrary values in the live coding environment.
-}
data Handle m h = Handle
  { create :: m h
  , destroy :: h -> m ()
  }

{- | Combine two handles to one.

'Handle's are not quite 'Monoid's because of the extra type parameter,
but it is possible to combine them.
In the combined handle, the first handle is created first and destroyed last.

Note: 'Handle' is not an 'Applicative' because it is not a 'Functor'
(because the destructor is contravariant in @h@).
-}
combineHandles :: Applicative m => Handle m h1 -> Handle m h2 -> Handle m (h1, h2)
combineHandles handle1 handle2 = Handle
  { create = ( , ) <$> create handle1 <*> create handle2
  , destroy = \(h1, h2) -> destroy handle2 h2 *> destroy handle1 h1
  }

data Handling h where
  Handling
    :: { id     :: Key
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

{- | Hide a handle in a cell,
taking care of initialisation and destruction.

Upon the first tick (or directly after migration),
the 'create' method of the 'Handle' is called,
and the result stored.
This result is then not changed anymore until the cell is removed again.
Once it is removed, the destructor will be called on the next tick.

Migrations will by default not inspect the interior of a 'handling' cell.
This means that handles are only migrated if they have exactly the same type.
-}
handling
  :: ( Typeable h
     , Monad m
    --  , MonadBase m m
    --  , MonadState (HandlingState m) n
    --  , MonadBase m n
     )
  => Handle m h
  -> Cell (HandlingStateT m) () h
handling handleImpl@Handle { .. } = Cell
  { cellState = Uninitialized
  , cellStep = \state () -> case state of
      !handling@Handling { .. } -> do
        reregister handleImpl handling
        handle `seq` return (handle, handling)
      Uninitialized -> do
        handle <- lift create
        id <- register handleImpl handle
        return (handle, Handling { .. })
  }

register
  :: Monad m
  => Handle m h
  -> h
  -> HandlingStateT m Key
register handleImpl handle = do
  HandlingState { .. } <- get
  let id = nHandles + 1
  put HandlingState
    { nHandles = id
    , destructors = insertDestructor handleImpl id handle destructors
    }
  return id

reregister
  :: Monad m
  => Handle m h
  -> Handling h
  -> HandlingStateT m ()
reregister handleImpl Handling { .. } = do
  HandlingState { .. } <- get
  put HandlingState { destructors = insertDestructor handleImpl id handle destructors, .. }

insertDestructor
  :: Handle m h
  -> Key
  -> h
  -> Destructors m
  -> Destructors m
insertDestructor Handle { .. } id handle destructors =
  let destructor = Destructor { isRegistered = True, action = destroy handle }
  in  insert id destructor destructors

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
  traverse (lift . action) unregistered
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
  gunfold _ _ = error "Handling.gunfold"

dataTypeDestructor :: DataType
dataTypeDestructor = mkDataType "Destructor" [ destructorConstr ]

destructorConstr :: Constr
destructorConstr = mkConstr dataTypeDestructor "Destructor" [] Prefix

instance Typeable m => Data (Destructor m) where
  dataTypeOf _ = dataTypeDestructor
  toConstr Destructor { .. } = destructorConstr
  gunfold _ _ = error "Destructor.gunfold"
