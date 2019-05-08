-- | Call e.g. with `stack ghci essenceoflivecoding-ghci\:exe\:GHCiTest`
--   and make sure the file `.ghci` is in the directory you're calling from.

-- essenceoflivecoding
import LiveCoding.LiveProgram
import LiveCoding.RuntimeIO

liveProgram = LiveProgram
  { liveState = 0 :: Int
  , liveStep  = \n -> print n >> return (n - 1)
  }

main = return ()
