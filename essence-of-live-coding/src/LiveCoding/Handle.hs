{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LiveCoding.Handle where

-- base
import Data.Data

-- transformers
import Control.Monad.Trans.Class (MonadTrans(lift))

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.HandlingState
import Control.Arrow ((>>>), arr)

{- | Container for unserialisable values,
such as 'IORef's, threads, 'MVar's, pointers, and device handles.

In a 'Handle', you can store a mechanism to create and destroy a value
that survives reloads occuring during live coding
even if does not have a 'Data' instance.
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
     )
  => Handle m h
  -> Cell (HandlingStateT m) arbitrary h
handling handle = arr (const ()) >>> handlingParametrised (toParametrised handle)

{- | Generalisation of 'Handle' carrying an additional parameter which may change at runtime.

Like in a 'Handle', the @h@ value of a 'ParametrisedHandle' is preserved through live coding reloads.
Additionally, the parameter @p@ value can be adjusted,
and triggers a destruction and reinitialisation whenever it changes.
-}
data ParametrisedHandle m p h = ParametrisedHandle
  { createParametrised :: p -> m h
  , changeParametrised :: p -> p -> h -> m h
  , destroyParametrised :: p -> h -> m ()
  }

-- | Given the methods 'createParametrised' and 'destroyParametrised',
--   build a fitting method for 'changeParametrised' which
defaultChange :: (Eq p, Monad m) => (p -> m h) -> (p -> h -> m ()) -> p -> p -> h -> m h
defaultChange creator destructor pOld pNew h
  | pOld == pNew = return h
  | otherwise    = do
      destructor pOld h
      creator pNew

-- | Like 'combineHandles', but for 'ParametrisedHandle's.
combineParametrisedHandles
  :: Applicative m
  => ParametrisedHandle m p1 h1
  -> ParametrisedHandle m p2 h2
  -> ParametrisedHandle m (p1, p2) (h1, h2)
combineParametrisedHandles handle1 handle2 = ParametrisedHandle
  { createParametrised = \(p1, p2) -> ( , ) <$> createParametrised handle1 p1 <*> createParametrised handle2 p2
  , changeParametrised = \(pOld1, pOld2) (pNew1, pNew2) (h1, h2) -> ( , ) <$> changeParametrised handle1 pOld1 pNew1 h1 <*> changeParametrised handle2 pOld2 pNew2 h2
  , destroyParametrised = \(p1, p2) (h1, h2) -> destroyParametrised handle1 p1 h1 *> destroyParametrised handle2 p2 h2
  }

{- | Hide a 'ParametrisedHandle' in a cell,
taking care of initialisation and destruction.

Upon the first tick, directly after migration, and after each parameter change,
the 'create' method of the 'Handle' is called,
and the result stored.
This result is then not changed anymore until the cell is removed again, or the parameter changes.
A parameter change triggers the destructor immediately,
but if the cell is removed, the destructor will be called on the next tick.

Migrations will by default not inspect the interior of a 'handling' cell.
This means that parametrised handles are only migrated if they have exactly the same type.
-}
handlingParametrised
  :: ( Typeable h, Typeable p
     , Monad m
     , Eq p
     )
  => ParametrisedHandle m p h
  -> Cell (HandlingStateT m) p h
handlingParametrised handleImpl@ParametrisedHandle { .. } = Cell { .. }
  where
    cellState = Uninitialized
    cellStep Uninitialized parameter = do
      mereHandle <- lift $ createParametrised parameter
      let handle = (mereHandle, parameter)
      key <- register $ destroyParametrised parameter mereHandle
      return (mereHandle, Handling { handle = handle, .. })
    cellStep handling@Handling { handle = (mereHandle, lastParameter), .. } parameter
      | parameter == lastParameter = do
          reregister (destroyParametrised parameter mereHandle) key
          return (mereHandle, handling)
      | otherwise = do
          mereHandle <- lift $ changeParametrised lastParameter parameter mereHandle
          reregister (destroyParametrised parameter mereHandle) key
          return (mereHandle, Handling { handle = (mereHandle, parameter), .. })

-- | Every 'Handle' is trivially a 'ParametrisedHandle'
--   when the parameter is the trivial type.
toParametrised :: Monad m => Handle m h -> ParametrisedHandle m () h
toParametrised Handle { .. } = ParametrisedHandle
  { createParametrised = const create
  , changeParametrised = const $ const return
  , destroyParametrised = const destroy
  }
