{-# LANGUAGE RecordWildCards #-}

module Handle.LiveProgram where

-- base
import Control.Arrow
-- containers

-- transformers
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.RWS.Strict (RWS, tell)
import qualified Control.Monad.Trans.RWS.Strict as RWS
import Control.Monad.Trans.State.Strict
import qualified Data.IntMap as IntMap
-- test-framework

-- test-framework-quickcheck2

-- essence-of-live-coding
import LiveCoding
import LiveCoding.Handle
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Util.LiveProgramMigration

testHandle :: Handle (RWS () [String] Int) String
testHandle =
  Handle
    { create = do
        n <- RWS.get
        let msg = "Handle #" ++ show n
        tell ["Creating " ++ msg]
        return msg,
      destroy = const $ tell ["Destroyed handle"]
    }

test =
  testGroup
    "Handle.LiveProgram"
    [ testProperty
        "Trigger destructors in live program"
        LiveProgramMigration
          { liveProgram1 =
              runHandlingState $
                liveCell $
                  handling testHandle >>> arrM (lift . tell . return) >>> constM inspectHandlingState,
            liveProgram2 = runHandlingState mempty,
            input1 = replicate 3 (),
            input2 = replicate 3 (),
            output1 =
              ["Creating Handle #0", "Handle #0", "Handles: 1", "Destructors: (1,True)"] :
              replicate 2 ["Handle #0", "Handles: 1", "Destructors: (1,True)"],
            output2 = [["Destroyed handle"], [], []],
            initialState = 0
          }
    ]
  where
    inspectHandlingState = do
      HandlingState {..} <- get
      lift $
        tell
          [ "Handles: " ++ show nHandles,
            "Destructors: " ++ unwords (show . second isRegistered <$> IntMap.toList destructors)
          ]
