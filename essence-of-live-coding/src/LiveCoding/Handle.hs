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
handling handleImpl@Handle { .. } = Cell
  { cellState = Uninitialized
  , cellStep = \state input -> case state of
      handling@Handling { .. } -> do
        reregister (destroy handle) key handle
        return (handle, state)
      Uninitialized -> do
        handle <- lift create
        key <- register (destroy handle) handle
        return (handle, Handling { .. })
  }
