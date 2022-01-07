{-# LANGUAGE ScopedTypeVariables #-}

module HandlingState where

-- base
import Control.Monad.Identity

-- containers
import Data.IntMap

-- transformers
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.State.Strict

-- mmorph
import Control.Monad.Morph

-- test-framework
import Test.Framework

-- HUnit
import Test.HUnit hiding (Test)

-- test-framework-hunit
import Test.Framework.Providers.HUnit

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2

-- QuickCheck
import Test.QuickCheck

-- essence-of-live-coding
import LiveCoding.HandlingState

extractHandlingStateEffect :: HandlingStateT (WriterT [String] Identity) a -> [String]
extractHandlingStateEffect = runIdentity . execWriterT . runHandlingStateT

runAccumDirectly :: Monoid w => Accum w a -> (a, w)
runAccumDirectly = flip runAccum mempty

runAccumViaState :: Monoid w => Accum w a -> (a, w)
runAccumViaState = flip runState mempty . accumToState . hoist lift

runWriterViaState :: Monoid w => Writer w a -> (a, w)
runWriterViaState = flip runState mempty . writerToState . hoist lift

test :: Test
test = testGroup "HandlingState"
  [ testGroup "Monad morphisms"
    [ testGroup "AccumT -> StateT"
      [ testProperty "Combinations of add are preserved"
          $ \(values :: [[Int]]) ->
            let action = mapM_ add values
                lhs = runAccumViaState action
                rhs = runAccumDirectly action
            in lhs === rhs
      , testCase "A combination of add and look is preserved"
          $ let action = do
                  add [1]
                  log1 <- look
                  add [2]
                  log12 <- look
                  add [3]
            in runAccumDirectly action @=? runAccumViaState action
      ]
    , testGroup "WriterT -> StateT"
      [ testProperty "Combinations of tell are preserved"
          $ \(values :: [[Int]]) ->
            let action = mapM_ tell values
            in runWriter action === runWriterViaState action
      ]
    ]
  ]
{-
  [ testGroup "HandlingStateT"
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
      ["reregistered clean up"] @=? extractHandlingStateEffect action]
  , testGroup "HandlingState" [ testCase "Registering causes the destructor to appear in the state" $ do
      let (((key, registry), handlingState), log) = runWriter $ runWriterT $ flip runAccumT mempty $ unHandlingStateT $ register $ tell ["clean up"]
      singleton key ((), ["clean up"]) @=? runWriter . action <$> destructors handlingState
      [] @=? log
      [key] @=? registered handlingState
  , testCase "Reregistering causes the destructor to appear in the state" $ do
      let (((key, registry), handlingState), log) = runWriter $ runWriterT $ flip runAccumT mempty $ unHandlingStateT $ do
            key <- register $ tell ["clean up"]
            reregister (tell ["clean up"]) key
            return key
      singleton key ((), ["clean up"]) @=? runWriter . action <$> destructors handlingState
      [] @=? log
      [key] @=? registered handlingState
  , testCase "Garbage collection leaves registered destructors in place and unregisters them" $ do
      let (((key, registry), handlingState), log) = runWriter $ runWriterT $ flip runAccumT mempty $ unHandlingStateT $ garbageCollected $ register $ tell ["clean up"]
      singleton key ((), ["clean up"]) @=? runWriter . action <$> destructors handlingState
      [] @=? log
      [] @=? registered handlingState
  , testCase "Garbage collection leaves reregistered destructors in place and unregisters them" $ do
      let (((key, registry), handlingState), log) = runWriter $ runWriterT $ flip runAccumT mempty $ unHandlingStateT $ garbageCollected $ do
            key <- register $ tell ["clean up"]
            reregister (tell ["reregister clean up"]) key
            return key
      singleton key ((), ["reregister clean up"]) @=? runWriter . action <$> destructors handlingState
      [] @=? log
      [] @=? registered handlingState
  ]]
-}
