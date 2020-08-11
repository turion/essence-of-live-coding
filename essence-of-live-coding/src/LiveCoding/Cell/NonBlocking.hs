{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module LiveCoding.Cell.NonBlocking
  ( nonBlocking
  )
  where

-- base
import Control.Concurrent
import Control.Monad ((>=>), void, when)
import Data.Data

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Handle
import LiveCoding.Handle.Examples

threadVarHandle :: Handle IO (MVar ThreadId)
threadVarHandle = Handle
  { create = newEmptyMVar
  , destroy = tryTakeMVar >=> mapM_ killThread
  }

{- | Wrap a cell in a non-blocking way.
Every incoming sample of @nonBlocking cell@ results in an immediate output,
either @Just b@ if the value was computed since the last poll,
or @Nothing@ if no new value was computed yet.
The resulting cell can be polled by sending 'Nothing'.
The boolean flag controls whether the current computation is aborted and restarted when new data arrives.
-}
nonBlocking
  :: Typeable b
  => Bool
  -- ^ Pass 'True' to abort the computation when new data arrives. 'False' discards new data.
  -> Cell IO a b
  -> Cell (HandlingStateT IO) (Maybe a) (Maybe b)
nonBlocking abort Cell { .. } = proc aMaybe -> do
  threadVar <- handling threadVarHandle            -< ()
  resultVar <- handling emptyMVarHandle            -< ()
  liftCell Cell { cellStep = nonBlockingStep, .. } -< (aMaybe, threadVar, resultVar)
    where
      nonBlockingStep s (Nothing, threadVar, resultVar) = do
        bsMaybe <- tryTakeMVar resultVar
        case bsMaybe of
          Just (!b, !s') -> do
            threadId <- takeMVar threadVar
            killThread threadId
            return (Just b, s')
          Nothing -> return (Nothing, s)
      nonBlockingStep s (Just a, threadVar, resultVar) = do
        noThreadRunning <- if abort
            -- Abort the current computation if it is still running
          then do
            maybeThreadId <- tryTakeMVar threadVar
            mapM_ killThread maybeThreadId
            return True
          -- No computation currently running
          else isEmptyMVar threadVar
        when noThreadRunning $ do
          threadId <- forkIO $ putMVar resultVar =<< cellStep s a
          putMVar threadVar threadId
        nonBlockingStep s (Nothing, threadVar, resultVar)

-- It would have been nice to refactor this with 'hoistCellKleisli',
-- but that would expose the existential state type to the handle.
nonBlocking abort noCell = nonBlocking abort $ toCell noCell
