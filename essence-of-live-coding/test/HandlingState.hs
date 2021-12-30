module HandlingState where

-- base
import Control.Monad.Identity

-- transformers
import Control.Monad.Trans.Writer.Strict

-- test-framework
import Test.Framework

-- HUnit
import Test.HUnit hiding (Test)

-- test-framework-hunit
import Test.Framework.Providers.HUnit

-- essence-of-live-coding
import LiveCoding.HandlingState
import Control.Monad.Trans.Accum
import Data.IntMap

extractHandlingStateEffect :: HandlingStateT (WriterT [String] Identity) a -> [String]
extractHandlingStateEffect = runIdentity . execWriterT . runHandlingStateT

test :: Test
test = testGroup "HandlingState"
  [ testCase "Registered action doesn't get triggered"
      $ [] @=? extractHandlingStateEffect (register $ tell ["clean up"])
  , testCase "Reregistering avoids trigger" $ do
      let action = do
            key <- garbageCollected $ register $ tell ["clean up"]
            reregister (tell ["reregistered"]) key
      [] @=? extractHandlingStateEffect action
  , testCase "Not reregistered action gets triggered" $ do
      let action = do
            register $ tell ["clean up"]
            garbageCollected $ return ()
      ["clean up"] @=? extractHandlingStateEffect action
  , testCase "Reregistered action gets triggered" $ do
      let action = do
            key <- register $ tell ["clean up"]
            garbageCollected $ reregister (tell ["reregistered clean up"]) key
            garbageCollected $ return ()
      ["reregistered clean up"] @=? extractHandlingStateEffect action
  ]
