module LiveCoding.Handle.Examples where

-- base
import Control.Concurrent
import Data.Data
import Data.IORef
-- essence-of-live-coding
import LiveCoding.Handle

-- | Create an 'IORef', with no special cleanup action.
ioRefHandle :: a -> Handle IO (IORef a)
ioRefHandle a =
  Handle
    { create = newIORef a,
      destroy = const $ return () -- IORefs are garbage collected
    }

-- | Create an uninitialised 'MVar', with no special cleanup action.
emptyMVarHandle :: Handle IO (MVar a)
emptyMVarHandle =
  Handle
    { create = newEmptyMVar,
      destroy = const $ return () -- MVars are garbage collected
    }

-- | Create an 'MVar' initialised to some value @a@,
--   with no special cleanup action.
newMVarHandle :: a -> Handle IO (MVar a)
newMVarHandle a =
  Handle
    { create = newMVar a,
      destroy = const $ return () -- MVars are garbage collected
    }

-- | Launch a thread executing the given action
--   and kill it when the handle is removed.
threadHandle :: IO () -> Handle IO ThreadId
threadHandle action =
  Handle
    { create = forkIO action,
      destroy = killThread
    }
