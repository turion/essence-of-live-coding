{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module LiveCoding.JSON where

-- json
import Text.JSON.Generic hiding (decodeJSON)

-- essence-of-live-coding
import LiveCoding

-- * JSON Utilities

data DataBox where
  DataBox :: Data a => a -> DataBox

withJSON :: (DataBox -> b) -> JSValue -> Maybe b
withJSON f val = fmap f $ resultToMaybe $ fromJSON val
  where
    resultToMaybe (Ok value) = Just value
    resultToMaybe (Error _) = Nothing

-- FIXME Actually I need to reimplement fromJSON_generic and insert default values
