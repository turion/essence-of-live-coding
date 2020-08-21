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
import Debug.Trace
import GHC.Exts

-- time
import Data.Time.Clock

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Handle
import LiveCoding.Handle.Examples

threadVarHandle :: Handle IO (MVar ThreadId)
threadVarHandle = Handle
  { create = newEmptyMVar
  , destroy = tryTakeMVar >=> mapM_ killThread
  }

printTime :: String -> IO ()
printTime msg = return ()
-- printTime msg = putStrLn =<< (((take 16 msg) ++) . show) <$> getCurrentTime

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
        printTime "Try take result "
        bsMaybe <- tryTakeMVar resultVar
        printTime "Maybe took result "
        case bsMaybe of
          Just (!b, !s') -> do
            printTime "Taking threadId "
            threadId <- takeMVar threadVar
            printTime "Killing thread  "
            killThread threadId
            printTime "Killed thread   "
            return (Just b, s')
          Nothing -> return (Nothing, s)
      nonBlockingStep s (Just a, threadVar, resultVar) = do
        printTime "Is noThreadRunning?"
        noThreadRunning <- if abort
            -- Abort the current computation if it is still running
          then do
            maybeThreadId <- tryTakeMVar threadVar
            mapM_ killThread maybeThreadId
            return True
          -- No computation currently running
          else do
            printTime "Checking isEmpty"
            isEmptyMVar threadVar
        -- print noThreadRunning
        printTime "Done noThreadRunning"
        when noThreadRunning $ do
          printTime "Forking new     "
          threadId <- forkIO $ putMVar resultVar =<< (lazy $ cellStep s a)
          printTime "Putting threadVar"
          putMVar threadVar threadId
        printTime "Returning to Nothing"
        nonBlockingStep s (Nothing, threadVar, resultVar)

-- It would have been nice to refactor this with 'hoistCellKleisli',
-- but that would expose the existential state type to the handle.
nonBlocking abort noCell = nonBlocking abort $ toCell noCell
