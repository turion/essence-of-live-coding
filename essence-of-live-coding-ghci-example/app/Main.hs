module Main where

-- base
import Control.Concurrent (threadDelay)
-- essence-of-live-coding
import LiveCoding

liveProgram :: LiveProgram (HandlingStateT IO)
liveProgram =
  liveCell $
    handling
      Handle
        { create = threadDelay 10000 >> putStrLn "Creating",
          destroy = const $ putStrLn "Destroying"
        }

main :: IO ()
main = liveMain liveProgram
