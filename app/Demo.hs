{-# LANGUAGE DeriveDataTypeable #-}
-- base
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

-- dunai-live
import Data.MonadicStreamFunction
import Control.Monad.Trans.MSF.Except hiding (forever)

-- essenceoflivecoding
import LiveCoding

prog n = saw n >>> arrM print >>> constM (threadDelay 1000000)

main = do
  putStrLn "Let's go!"
  var <- launch $ prog 4
  forever $ do
    n <- readLn
    update var $ prog n
