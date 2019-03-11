{-# LANGUAGE DeriveDataTypeable #-}
-- base
import Control.Arrow
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

-- TODO Fix imports to a single one
-- essenceoflivecoding
import LiveCoding
import LiveCoding.Cell

main = do
  putStrLn "Let's go!"
  var <- launch $ example1 10
  forever $ do
    n <- readLn
    update var $ example2 n
    n <- readLn
    update var $ example1 n
