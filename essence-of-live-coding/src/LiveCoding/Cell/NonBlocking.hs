{-# LANGUAGE RecordWildCards #-}

module LiveCoding.Cell.NonBlocking
  ( nonBlocking
  )
  where

-- base
import Control.Concurrent
import Control.Monad (void, when)

-- essence-of-live-coding
import LiveCoding.Cell

{- | Wrap a cell in a non-blocking way.

Every incoming sample of @nonBlocking cell@ results in an immediate output,
either @Just b@ if the value was computed since the last poll,
or @Nothing@ if no new value was computed yet.
The resulting cell can be polled by sending 'Nothing'.

The boolean flag controls whether the current computation is aborted and restarted when new data arrives.
-}
nonBlocking
  :: Bool
  -- ^ Pass 'True' to abort the computation when new data arrives. 'False' discards new data.
  ->     Cell IO        a         b
  -> IO (Cell IO (Maybe a) (Maybe b))
nonBlocking abort Cell { .. } = do
  threadVar <- newEmptyMVar
  resultVar <- newEmptyMVar
  let nonBlockingStep s Nothing = do
        bsMaybe <- tryTakeMVar resultVar
        case bsMaybe of
          Just (b, s') -> do
            threadId <- takeMVar threadVar
            killThread threadId
            return (Just b, s')
          Nothing -> return (Nothing, s)
      nonBlockingStep s (Just a) = do
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
        nonBlockingStep s Nothing
  return Cell { cellStep = nonBlockingStep, .. }

-- FIXME That's a wart. I should write this out with a general wrapper that sends Kleisli arrow transformations to cell transformations
nonBlocking abort noCell = nonBlocking abort $ toCell noCell
