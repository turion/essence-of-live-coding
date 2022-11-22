{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module LiveCoding.LiveProgram.Monad.Trans where

-- base
import Data.Data

-- transformers
import Control.Monad.Trans.State.Strict

-- essence-of-live-coding

import LiveCoding.Cell.Monad.Trans
import LiveCoding.LiveProgram

{- | Remove a stateful effect from the monad stack by supplying the initial state.
   This state then becomes part of the internal live program state,
   and is subject to migration as any other state.
   Live programs are automatically migrated to and from applications of 'runStateL'.
-}
runStateL ::
  (Data stateT, Monad m) =>
  LiveProgram (StateT stateT m) ->
  stateT ->
  LiveProgram m
runStateL LiveProgram {..} stateT =
  LiveProgram
    { liveState = State {stateInternal = liveState, ..}
    , liveStep = \State {..} -> do
        (stateInternal, stateT) <- runStateT (liveStep stateInternal) stateT
        return State {..}
    }
