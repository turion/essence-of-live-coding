{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module LiveCoding.Migrate.Monad.Trans where

-- base
import Data.Data

-- essence-of-live-coding
import LiveCoding.Cell.Monad.Trans
import LiveCoding.Migrate.Migration

maybeMigrateToState
  :: (Typeable stateInternal', Typeable stateInternal)
  => State stateT stateInternal
  -> stateInternal'
  -> Maybe (State stateT stateInternal)
maybeMigrateToState State { stateT } stateInternal' = do
  stateInternal <- cast stateInternal'
  return State { .. }

-- | Tries to cast the current state into the joint state of a program
--   where a state effect has been absorbed into the internal state with 'runStateL' or 'runStateC'.
migrationToState :: Migration
migrationToState = migrationTo2 maybeMigrateToState

maybeMigrateFromState
  :: (Typeable stateInternal', Typeable stateInternal)
  => State stateT stateInternal
  -> Maybe              stateInternal'
maybeMigrateFromState State { stateInternal } = cast stateInternal

-- | Try to extract a state from the current joint state of a program wrapped with 'runStateL' or 'runStateC'.
migrationFromState :: Migration
migrationFromState = constMigrationFrom2 maybeMigrateFromState

-- | Combines 'migrationToState' and 'migrationFromState'.
migrationState :: Migration
migrationState = migrationToState <> migrationFromState
