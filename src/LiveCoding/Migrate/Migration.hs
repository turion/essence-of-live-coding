module LiveCoding.Migrate.Migration where

newtype Migration a b = Migration
  { unMigration :: a -> b -> a }

