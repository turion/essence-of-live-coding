\begin{comment}
\begin{code}
module LiveCoding.Migrate where

-- base
import Data.Data
import Data.Maybe
import Prelude hiding (GT)

-- syb
import Data.Generics.Aliases
import Data.Generics.Twins
\end{code}
\end{comment}

\begin{code}
migrate :: (Data a, Data b) => a -> b -> a
migrate a b
  |  isAlgType (dataTypeOf a)
  && isAlgType (dataTypeOf b)
  && show (dataTypeOf a) == show (dataTypeOf b)
  && showConstr (toConstr a) == showConstr (toConstr b)
  = setChildren (getChildrenSetters b) a
-- TODO Add special cases for:
-- * Records
-- * Exception handling! Make my own Either type
-- * a -> (a, b) and back (for feedback)
-- * a -> Maybe a and back
-- * a >>> b to a or b and back
migrate a b = fromMaybe a $ cast b

getChildrenSetters :: Data a => a -> [GenericT']
getChildrenSetters = gmapQ $ \child -> GT $ flip migrate child

setChildren :: Data a => [GenericT'] -> a -> a
setChildren updates a = snd $ gmapAccumT f updates a
  where
    f [] e = ([], e)
    f (update : updates) e = (updates, unGT update $ e)
\end{code}
