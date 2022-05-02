{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Handle.LiveProgram where

-- base
import Control.Arrow
import Control.Monad.Identity (Identity)

-- containers
import qualified Data.IntMap as IntMap

-- transformers
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.RWS.Strict (RWS, tell)
import qualified Control.Monad.Trans.RWS.Strict as RWS
import Control.Monad.Trans.State.Strict

-- transformers-base
import Control.Monad.Base

-- test-framework
import Test.Framework

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2

-- essence-of-live-coding
import LiveCoding
import LiveCoding.Handle
import Util.LiveProgramMigration

testHandle :: Handle (RWS () [String] Int) String
testHandle = Handle
  { create = do
      n <- RWS.get
      let msg = "Handle #" ++ show n
      tell ["Creating " ++ msg]
      return msg
  , destroy = const $ tell ["Destroyed handle"]
  }

instance {-# OVERLAPPING #-} MonadBase (RWS.RWST () [[Char]] a Identity) (RWS.RWST () [[Char]] a Identity) where
  liftBase = id

test = testGroup "Handle.LiveProgram"
  [ testProperty "Trigger destructors in live program" LiveProgramMigration
    { liveProgram1 = runHandlingState $ liveCell
        $ handling testHandle >>> arrM (lift . tell . return) >>> constM inspectHandlingState
    , liveProgram2 = runHandlingState mempty
    , input1 = replicate 3 ()
    , input2 = replicate 3 ()
    , output1 = ["Creating Handle #0", "Handle #0", "Handles: 1", "Destructors: (1,True)"]
        : replicate 2 ["Handle #0", "Handles: 1", "Destructors: (1,True)"]
    , output2 = [["Destroyed handle"], [], []]
    , initialState = 0
    }
  ]
    where
      inspectHandlingState = do
        HandlingState { .. } <- get
        lift $ tell
          [ "Handles: " ++ show nHandles
          , "Destructors: " ++ unwords (show . second isRegistered <$> IntMap.toList destructors)
          ]
