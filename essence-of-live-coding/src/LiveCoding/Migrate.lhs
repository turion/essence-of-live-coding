\begin{comment}
\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
module LiveCoding.Migrate where

-- base
import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Data
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe
import Prelude hiding (GT)

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

-- syb
import Data.Generics.Aliases
import Data.Generics.Twins

-- essence-of-live-coding
import LiveCoding.Migrate.Debugger
import LiveCoding.Migrate.Cell
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
standardMigration
  =  castMigration
  <> migrationDebugging
  <> migrationCell
  <> newtypeMigration

-- | The standard migration working horse.
--   Tries to apply the given migration,
--   and if this fails, tries to recurse into the data structure.
treeMigration :: Migration -> Migration
treeMigration specific
-- Maybe the specified user migration works?
  = specific
-- Maybe it's an algebraic datatype.
-- Let's try and match the structure as well as possible.
  <> sameConstructorMigration specific
  <> constructorMigration specific

matchingAlgebraicDataTypes :: (Data a, Data b) => a -> b -> Bool
matchingAlgebraicDataTypes a b
  = isAlgType typeA
  && isAlgType typeB
  && dataTypeName typeA == dataTypeName typeB
  where
    typeA = dataTypeOf a
    typeB = dataTypeOf b

-- | Assuming that both are algebraic data types, possibly the constructor names match.
--   In that case, we will try and recursively migrate as much data as possible onto the new constructor.
sameConstructorMigration :: Migration -> Migration
sameConstructorMigration specific = Migration $ \a b -> do
  guard $ matchingAlgebraicDataTypes a b
  let
    constrA = toConstr a
    constrB = toConstr b
  guard $ showConstr constrA == showConstr constrB
  let
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
  return migrateSameConstr

-- | Still assuming that both are algebraic data types, but the constructor names don't match.
--   In that case, we will try and recursively fill all the fields new constructor.
--   If this doesn't work, fail.
constructorMigration :: Migration -> Migration
constructorMigration specific = Migration $ \a b -> do
  let
    constrB = toConstr b
    constrFieldsB = constrFields constrB
  guard $ matchingAlgebraicDataTypes a b
  matchingConstructor <- dataTypeOf a
    & dataTypeConstrs
    & map (show &&& id)
    & lookup (showConstr constrB)
  let matchingConstructorFields = constrFields matchingConstructor
  fieldSetters <- if null constrFieldsB || null matchingConstructorFields
    -- We don't have record. Try to cast each field.
    then
      return $ getChildrenMaybe b
    -- We have records. Sort by all field names and try to cast
    else
      getChildrenMaybe b
        & zip constrFieldsB
        & flip lookup
        & flip map matchingConstructorFields
        & sequence
  flip evalStateT fieldSetters $ fromConstrM tryOneField matchingConstructor

tryOneField :: Data a => StateT [GenericR' Maybe] Maybe a
tryOneField = do
  (field : fields) <- get
  put fields
  lift $ unGR field --lift field

getChildrenSetters :: Data a => Migration -> a -> [GenericT']
getChildrenSetters specific = gmapQ $ \child -> GT $ flip (runSafeMigration $ treeMigration specific) child

newtype GenericR' m = GR { unGR :: GenericR m }

getChildrenMaybe :: Data a => a -> [GenericR' Maybe]
getChildrenMaybe = gmapQ $ \child -> GR $ cast child

setChildren :: Data a => [GenericT'] -> a -> a
setChildren updates a = snd $ gmapAccumT f updates a
  where
    f [] e = ([], e)
    f (update : updates) e = (updates, unGT update $ e)
\end{code}
