{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
module LiveCoding.Vivid where

-- base
import Data.Foldable (traverse_)

-- vivid
import Vivid

-- essence-of-live-coding
import LiveCoding.Handle
import LiveCoding

data SynthState
  = Started
  | Stopped
  deriving (Eq)

-- fixme release instead of free?
vividHandleParametrised
  :: (VividAction m, Eq params, VarList params, Subset (InnerVars params) args)
  => ParametrisedHandle m (params, SynthDef args, SynthState) (Maybe (Synth args))
vividHandleParametrised = ParametrisedHandle { .. }
  where
    createParametrised (params, synthDef, Started) = Just <$> synth synthDef params
    createParametrised (params, synthDef, Stopped) = defineSD synthDef >> pure Nothing

    destroyParametrised _ synthMaybe = traverse_ free synthMaybe

    -- Only the synth parameters changed and it's still running.
    -- So simply set new parameters without stopping it.
    changeParametrised (paramsOld, synthDefOld, Started) (paramsNew, synthDefNew, Started) (Just synth)
      | paramsOld /= paramsNew && synthDefOld == synthDefNew = do
          set synth paramsNew
          return $ Just synth
    -- Synthdef or start/stop state changed, need to release and reinitialise
    changeParametrised old new synth = defaultChange createParametrised destroyParametrised old new synth

deriving instance Eq (SynthDef args)

liveSynth
  :: (VividAction m, VarList params, Subset (InnerVars params) args, Typeable args, Data params, Eq params, VarList (Synth args))
  => Cell (HandlingStateT m) (params, SynthDef args, SynthState) (Maybe (Synth args))
liveSynth = handlingParametrised vividHandleParametrised
