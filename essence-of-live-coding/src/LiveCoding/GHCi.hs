{- | Support functions to call common live coding functionalities like launching and reloading
from a @ghci@ or @cabal repl@ session.

You typically don't need to import this module in your code,
but you should load it in your interactive session,
ideally by copying the file `essence-of-live-coding/.ghci` to your project,
adjusting it to your needs and launching @cabal repl@.
-}
module LiveCoding.GHCi where

-- base
import Control.Concurrent

-- foreign-store
import Foreign.Store

-- essence-of-live-coding
import LiveCoding.LiveProgram
import LiveCoding.RuntimeIO
import LiveCoding.RuntimeIO.Launch

livelaunch _ = return $ unlines
  [ "launchedProgram <- launch liveProgram"
  , "save launchedProgram"
  ]

livestop _ = return "stop launchedProgram"

-- | Load a 'LiveProgram' of a given type from the store.
--   The value of the given 'LiveProgram' is not used,
--   it only serves as a proxy for m.
load :: Launchable m => LiveProgram m -> IO (LaunchedProgram m)
load _ = readStore $ Store 0

-- | Save a 'LiveProgram' to the store.
save :: Launchable m => LaunchedProgram m -> IO ()
save = writeStore $ Store 0

-- TODO Could also parametrise this and all other commands by the 'liveProgram'

liveinit _ = return $ unlines
  [ "programVar <- newMVar liveProgram"
  , "threadId <- myThreadId"
  , "save LaunchedProgram { .. }"
  ]

livereload _ = return $ unlines
  [ ":reload"
  , "launchedProgram <- load liveProgram"
  , "update launchedProgram liveProgram"
  ]

livestep _ = return "stepLaunchedProgram launchedProgram"
