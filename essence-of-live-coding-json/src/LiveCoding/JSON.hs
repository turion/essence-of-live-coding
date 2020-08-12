module LiveCoding.JSON where

-- json
import Text.JSON.Generic hiding (decodeJSON)

-- essence-of-live-coding
import LiveCoding

-- * JSON Utilities

-- | Decode a string as a value,
--   throwing an error if the JSON is invalid or doesn't match the type.
decodeJSON :: Data a => String -> Result a
decodeJSON = runGetJSON readJSValue >=> fromJSON

-- FIXME Actually I need to reimplement fromJSON_generic and insert default values
