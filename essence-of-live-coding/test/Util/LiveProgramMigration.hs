{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Util.LiveProgramMigration where

-- transformers
import Control.Monad.Trans.RWS.Strict (RWS, runRWS)

-- QuickCheck

-- essence-of-live-coding
import LiveCoding
import Test.QuickCheck

data LiveProgramMigration a b
  = forall s.
  LiveProgramMigration
  { liveProgram1 :: LiveProgram (RWS a b s)
  , liveProgram2 :: LiveProgram (RWS a b s)
  , initialState :: s
  , input1 :: [a]
  , input2 :: [a]
  , output1 :: [b]
  , output2 :: [b]
  }

stepLiveProgramRWS :: (Monoid b) => LiveProgram (RWS a b s) -> a -> s -> (LiveProgram (RWS a b s), s, b)
stepLiveProgramRWS liveProg = runRWS (stepProgram liveProg)

stepsLiveProgramRWS :: (Monoid b) => LiveProgram (RWS a b s) -> s -> [a] -> (LiveProgram (RWS a b s), s, [b])
stepsLiveProgramRWS liveProg s [] = (liveProg, s, [])
stepsLiveProgramRWS liveProg s (a : as) =
  let (liveProg', s', b) = stepLiveProgramRWS liveProg a s
   in (liveProg', s', b : third (stepsLiveProgramRWS liveProg' s' as))

third :: (a, b, c) -> c
third (a, b, c) = c

instance (Monoid b, Eq b, Show b) => Testable (LiveProgramMigration a b) where
  property LiveProgramMigration {..} =
    let (liveProg', s', output1') = stepsLiveProgramRWS liveProgram1 initialState input1
        liveProg2 = hotCodeSwap liveProgram2 liveProg'
        (_, _, output2') = stepsLiveProgramRWS liveProg2 s' input2
     in output1 === output1' .&&. output2 === output2'
