module LiveCoding.Migrate.Debugger where

-- essenceoflivecoding
import LiveCoding.Debugger
import LiveCoding.Migrate.Migration

-- TODO Break import cycle so this module can be used


migrateToDebugging
  :: (Data dbgState, Data state)
  => Debugging dbgState state
  ->                    state
  -> Debugging dbgState state
migrateToDebugging Debugging { dbgState } state = Debugging { .. }

migrateFromDebugging
  :: (Data dbgState, Data state)
  =>                    state
  -> Debugging dbgState state
  ->                    state
migrateFromDebugging state Debugging { dbgState } = Debugging { .. }

userMigrate
  :: (Data a, Data b, Typeable c, Typeable d)
  => (c -> d)
  -> a -> b -> a
userMigrate specific a = userMigrate' specific `extendMigration` migrateToDebugging `extendMigration` migrateFromDebugging
