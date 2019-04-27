{-# LANGUAGE DeriveDataTypeable #-}

-- base
import Control.Arrow
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

-- TODO Fix imports to a single one
-- essenceoflivecoding
import Examples
import LiveCoding
import LiveCoding.Cell
import LiveCoding.Bind (printSineChange)
import LiveCoding.RuntimeIO
import LiveCoding.Forever

main = do
  putStrLn "Let's go!"
  --var <- launch $ printSinesForever
  -- var <- launch $ printSine 1
  var <- launch $ printSine 3
  forever $ do
    _ <- getLine
    update var $ printSine 10
    -- threadDelay $ 17 * 100000
    _ <- getLine
    update var $ printSine 3
