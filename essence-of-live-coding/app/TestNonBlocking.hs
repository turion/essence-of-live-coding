{-# LANGUAGE Arrows #-}

module Main
  ( module Main
  , module X
  ) where

-- base
import Control.Arrow
import Control.Concurrent

-- essence-of-live-coding
import LiveCoding
import LiveCoding.GHCi as X

-- | An identity function that takes a long time to pass on its value.
slowId :: Cell IO a a
slowId = proc a -> do
  arrM threadDelay -< 1000000
  returnA -< a

main :: IO ()
main = do
  putStrLn "Push return to start a slow calculation."
  mainCell <- mkMainCell
  foreground $ liveCell mainCell

-- | Constantly count the number of ticks passed since program start.
--   Whenever the keyboard return key is pressed,
--   this number is printed, and passed into a slow "computation" in a separate thread,
--   while the foreground thread is not blocked.
--   When the background thread returns, the number is printed again.
mkMainCell :: IO (Cell IO () ())
mkMainCell = do
  keyboard <- nonBlocking False $ constM getLine -- Only poll, never abort
  mySlowId <- nonBlocking True slowId -- Abort and restart when new data arrives
  return $ proc _ -> do
    n <- count                    -< ()
    lineMaybe <- keyboard         -< Just ()
    let nString = show n <$ lineMaybe
    resampleMaybe (arrM putStrLn) -< ("Calculating " ++) <$> nString
    resultMaybe <- mySlowId       -< nString
    resampleMaybe (arrM putStrLn) -< ("Calculated "  ++) <$> resultMaybe
    arrM threadDelay              -< 1000 -- Don't hog CPU
