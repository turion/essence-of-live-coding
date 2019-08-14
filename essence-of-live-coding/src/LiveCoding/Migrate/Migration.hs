{-# LANGUAGE RankNTypes #-}

module LiveCoding.Migrate.Migration where

-- base
import Control.Monad (guard)
import Data.Data
import Data.Maybe (fromMaybe)
import Data.Monoid

-- syb
import Data.Generics.Schemes (glength)

data Migration = Migration
  { runMigration :: forall a b . (Data a, Data b) => a -> b -> Maybe a }

-- | Run a migration and insert the new initial state in case of failure.
runSafeMigration
  :: (Data a, Data b)
  => Migration
  -> a -> b -> a
runSafeMigration migration a b = fromMaybe a $ runMigration migration a b

-- | If both migrations would succeed, the result from the first is used.
instance Semigroup Migration where
  migration1 <> migration2 = Migration $ \a b -> getFirst
    $  (First $ runMigration migration1 a b)
    <> (First $ runMigration migration2 a b)

instance Monoid Migration where
  mempty = Migration $ const $ const Nothing

castMigration :: Migration
castMigration = Migration $ const cast

newtypeMigration :: Migration
newtypeMigration = Migration $ \a b -> do
  -- Is it an algebraic datatype with a single constructor?
  AlgRep [_constr] <- return $ dataTypeRep $ dataTypeOf a
  -- Does the constructor have a single argument?
  guard $ glength a == 1
  -- Try to cast the single child to b
  gmapM (const $ cast b) a

-- | If you have a specific type that you would like to be migrated to a specific other type,
--   you can create a migration for this.
--   For example: @userMigration (toInteger :: Int -> Integer)@
userMigration
  :: (Typeable c, Typeable d)
  => (c -> d)
  -> Migration
userMigration specific = Migration $ \_a b -> cast =<< specific <$> cast b
