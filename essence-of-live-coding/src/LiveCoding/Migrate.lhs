\begin{comment}
\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
module LiveCoding.Migrate where

-- base
import Data.Data
import Data.Functor ((<&>))
import Data.Maybe
import Prelude hiding (GT)

-- syb
import Data.Generics.Aliases
import Data.Generics.Twins

-- essence-of-live-coding
import LiveCoding.Migrate.Debugger
import LiveCoding.Migrate.Migration
\end{code}
\end{comment}

\begin{code}
-- | The standard migration solution, recursing into the data structure and applying 'standardMigration'.
migrate :: (Data a, Data b) => a -> b -> a
migrate = migrateWith standardMigration

-- | Still recurse into the data structure, but apply your own given migration.
--   Often you will want to call @migrateWith (standardMigration <> yourMigration)@.
migrateWith :: (Data a, Data b) => Migration -> a -> b -> a
migrateWith specific = runSafeMigration $ treeMigration specific

-- | Covers standard cases such as matching types, to and from debuggers, to newtypes.
standardMigration :: Migration
standardMigration = castMigration <> migrationDebugging <> newtypeMigration

-- | Wrapping 'treeMigrateWith' in the newtype.
treeMigration :: Migration -> Migration
treeMigration migration = Migration $ treeMigrateWith migration

-- | The standard migration working horse.
--   Tries to apply the given migration,
--   and if this fails, tries to recurse into the data structure.
treeMigrateWith
  :: (Data a, Data b)
  => Migration
  -> a -> b -> Maybe a

-- Maybe the specified user migration works?
treeMigrateWith specific a b
  | Just a' <- runMigration specific a b
  = Just a'

-- Maybe it's an algebraic datatype.
-- Let's try and match the structure as well as possible.
treeMigrateWith specific a b
  |  isAlgType typeA  && isAlgType typeB
  && show typeA == show typeB
  && showConstr constrA == showConstr constrB
  = Just migrateSameConstr
  where
    typeA = dataTypeOf a
    typeB = dataTypeOf b
    constrA = toConstr a
    constrB = toConstr b
    constrFieldsA = constrFields constrA
    constrFieldsB = constrFields constrB
    migrateSameConstr
      -- We have records, we can match on the field labels
      |  (not $ null constrFieldsA)
      && (not $ null constrFieldsB)
      = setChildren getFieldSetters a
      -- One of the two is not a record, just try to match 1-1 as far as possible
      | otherwise = setChildren (getChildrenSetters specific b) a
    settersB = zip constrFieldsB $ getChildrenSetters specific b
    getFieldSetters = constrFieldsA <&>
      \field -> fromMaybe (GT id)
        $ lookup field settersB

-- Defeat. No migration worked.
treeMigrateWith _ _ _ = Nothing

getChildrenSetters :: Data a => Migration -> a -> [GenericT']
getChildrenSetters specific = gmapQ $ \child -> GT $ flip (runSafeMigration $ treeMigration specific) child

setChildren :: Data a => [GenericT'] -> a -> a
setChildren updates a = snd $ gmapAccumT f updates a
  where
    f [] e = ([], e)
    f (update : updates) e = (updates, unGT update $ e)

\end{code}
