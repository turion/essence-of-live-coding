module Data.Data.Util where

-- base
import Control.Monad (join)
import Data.Data

(%.%) :: (Data a, Typeable b) => a -> String -> Maybe b
a %.% fieldName =
  let fields = gmapQ cast a
      fieldNames = constrFields $ toConstr a
      fieldsWithNames = zip fieldNames fields
  in join $ lookup fieldName fieldsWithNames
