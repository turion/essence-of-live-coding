\begin{comment}
\begin{code}
module LiveCoding.Migrate where

-- base
import Data.Data
import Data.Functor ((<&>))
import Data.Maybe
import Prelude hiding (GT)

-- syb
import Data.Generics.Aliases
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
migrate :: (Data a, Data b) => a -> b -> a
migrate = userMigrate (\() -> ())

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

--userMigrate specific a b = fromMaybe a $ listToMaybe $ catMaybes $ fromMaybe a <$> ($ a) <$> [cast . specific, cast]
userMigrate specific a b = fromMaybe a $ (cast `extQ` (cast . specific)) b
--userMigrate specific a b = (fromMaybe a . cast . specific) `extQ` (fromMaybe a . cast) $ a

getChildrenSetters :: (Data a, Typeable c, Typeable d) => (c -> d) -> a -> [GenericT']
getChildrenSetters specific = gmapQ $ \child -> GT $ flip (userMigrate specific) child

setChildren :: Data a => [GenericT'] -> a -> a
setChildren updates a = snd $ gmapAccumT f updates a
  where
    f [] e = ([], e)
    f (update : updates) e = (updates, unGT update $ e)

\end{code}
