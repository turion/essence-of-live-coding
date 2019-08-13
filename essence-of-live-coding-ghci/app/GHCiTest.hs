-- | Call e.g. with `stack ghci essence-of-live-coding-ghci\:exe\:GHCiTest`
--   and make sure the file `.ghci` is in the directory you're calling from.

-- essence-of-live-coding
import LiveCoding

liveProgram = LiveProgram
  { liveState = 0 :: Int
  , liveStep  = \n -> print n >> return (n - 1)
  }

main = return ()
