{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module LiveCoding.Migrate.Debugger where

-- base
import Control.Monad (guard)
import Data.Data
import Data.Maybe

-- essence-of-live-coding
import LiveCoding.Debugger
import LiveCoding.Migrate.Migration

migrateToDebugging
  :: Debugging dbgState state
  ->                    state
  -> Debugging dbgState state
migrateToDebugging Debugging { dbgState } state = Debugging { .. }

-- | Tries to cast the current state into the joint state of debugger and program.
--   Will cast to the program state if possible, or else try to cast to the debugger state.
migrationToDebugging :: Migration
migrationToDebugging = Migration $ \a b -> do
  guard $ ("Debugging" ==) $ dataTypeName $ dataTypeOf a
  gmapMo (const $ cast b) a

-- | Try to extract a state from the current joint state of debugger and program.
migrationFromDebugging :: Migration
migrationFromDebugging = Migration $ \_ b -> do
  guard $ ("Debugging" ==) $ dataTypeName $ dataTypeOf b
  listToMaybe $ catMaybes $ (gmapQ cast) b

migrateFromDebugging
  ::                    state
  -> Debugging dbgState state
  ->                    state
migrateFromDebugging _state Debugging { state } = state

-- | Combines 'migrationToDebugging' and 'migrationFromDebugging'.
migrationDebugging :: Migration
migrationDebugging = migrationToDebugging <> migrationFromDebugging
