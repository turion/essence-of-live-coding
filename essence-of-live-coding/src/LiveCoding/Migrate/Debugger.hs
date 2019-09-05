{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module LiveCoding.Migrate.Debugger where

-- base
import Data.Data

-- essence-of-live-coding
import LiveCoding.Debugger
import LiveCoding.Migrate.Migration

maybeMigrateToDebugging
  :: (Typeable state', Typeable state)
  => Debugging dbgState state
  -> state'
  -> Maybe (Debugging dbgState state)
maybeMigrateToDebugging Debugging { dbgState } state' = do
  state <- cast state'
  return Debugging { .. }

-- | Tries to cast the current state into the joint state of debugger and program.
--   Will cast to the program state if possible, or else try to cast to the debugger state.
migrationToDebugging :: Migration
migrationToDebugging = migrationTo2 maybeMigrateToDebugging

maybeMigrateFromDebugging
  :: (Typeable state', Typeable state)
  => Debugging dbgState state
  -> Maybe              state'
maybeMigrateFromDebugging Debugging { state } = cast state

-- | Try to extract a state from the current joint state of debugger and program.
migrationFromDebugging :: Migration
migrationFromDebugging = constMigrationFrom2 maybeMigrateFromDebugging

-- | Combines 'migrationToDebugging' and 'migrationFromDebugging'.
migrationDebugging :: Migration
migrationDebugging = migrationToDebugging <> migrationFromDebugging
