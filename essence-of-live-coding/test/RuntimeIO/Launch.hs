module RuntimeIO.Launch where

-- base
import Data.IORef

-- hunit
import Test.HUnit

-- test-framework-hunit
import Test.Framework.Providers.HUnit

-- essence-of-live-coding
import LiveCoding
import Control.Concurrent (threadDelay)

loggingHandle :: IORef [String] -> Handle IO ()
loggingHandle ref = Handle
  { create = modifyIORef ref ("Created handle" :)
  , destroy = const $ modifyIORef ref ("Destroyed handle" :)
  }

testProgram :: IORef [String] -> LiveProgram (HandlingStateT IO)
testProgram ref = liveCell $ handling $ loggingHandle ref

test = testCase "HandlingStateT destroys all handles" $ do
  ref <- newIORef []
  launchedProgram <- launch mempty
  assertRefContains ref []
  update launchedProgram $ testProgram ref
  assertRefContains ref ["Created handle"]
  stop launchedProgram
  assertRefContains ref ["Destroyed handle", "Created handle"]

assertRefContains ref messagesExpected = do
  threadDelay 100000
  messagesRead <- readIORef ref
  messagesRead @?= messagesExpected
