\begin{comment}
\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
module LiveCoding.Migrate where

-- base
import Control.Monad (guard)
import Data.Data
import Data.Foldable (asum)
import Data.Functor ((<&>))
import Data.Maybe
import Prelude hiding (GT)

-- syb
import Data.Generics.Aliases
import Data.Generics.Schemes (glength)
import Data.Generics.Twins

-- TODO Add special cases for:
-- * data Foo = Foo -> data Foo = Foo | Bar!
-- * Adding custom user cases first (example Int -> Integer)
-- * Adding custom user renames
-- * Exception handling! Make my own Either type
-- * a -> (a, b) and back (for feedback)
-- * a -> Maybe a and back
-- * a >>> b to a or b and back
\end{code}
\end{comment}

\begin{code}
newtype Migration a b = Migration { unMigration :: a -> b -> a }

migrate :: (Data a, Data b) => a -> b -> a
migrate = userMigrate (\() -> ())

{-
extendMigration
  :: (Data a, Data b)
  => (a -> b -> a)
  -> (forall c d . (Data c, Data d) => c -> d -> c)
  ->  a -> b -> a
extendMigration oldMigration newMigration = unMigration $ Migration oldMigration `extendMigration'` Migration newMigration
extendMigration'
  :: (Data a, Data b)
  => Migration a b
  -> (forall c d . (Data c, Data d) => Migration c d)
  ->  Migration a b
extendMigration' def ext = maybe def id (dataCast2 ext)

ext2 :: (Data a, Typeable t)
     => c a
     -> (forall d1 d2. (Data d1, Data d2) => c (t d1 d2))
     -> c a
ext2 def ext = maybe def id (dataCast2 ext)
-}
-- TODO Port all cases to asum?
userMigrate
  :: (Data a, Data b, Typeable c, Typeable d)
  => (c -> d)
  -> a -> b -> a
userMigrate specific a b
  |  isAlgType typeA  && isAlgType typeB
  && show typeA == show typeB
  && showConstr constrA == showConstr constrB
  = migrateSameConstr
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

userMigrate specific a b = fromMaybe a $ asum
  -- Migration to newtype
  [ do
      -- Is it an algebraic datatype with a single constructor?
      AlgRep [_constr] <- return $ dataTypeRep $ dataTypeOf a
      -- Does the constructor have a single argument?
      guard $ glength a == 1
      -- Try to cast the single child to b
      gmapM (const $ cast b) a
  , (cast `extQ` (cast . specific)) b -- TODO Can I split this into two cases in the list?
  ]

getChildrenSetters :: (Data a, Typeable c, Typeable d) => (c -> d) -> a -> [GenericT']
getChildrenSetters specific = gmapQ $ \child -> GT $ flip (userMigrate specific) child

setChildren :: Data a => [GenericT'] -> a -> a
setChildren updates a = snd $ gmapAccumT f updates a
  where
    f [] e = ([], e)
    f (update : updates) e = (updates, unGT update $ e)

\end{code}
