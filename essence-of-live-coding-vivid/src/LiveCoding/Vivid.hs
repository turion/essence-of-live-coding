{- | Support for [@vivid@](https://hackage.haskell.org/package/vivid),
a Haskell library for [SuperCollider](https://supercollider.github.io/).

With this module, you can create cells corresponding to synthesizers.

The synthesizers automatically start and stop on reload.
-}

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

{- | Whether a synthesizer should currently be running or not.

Typically, you will either statically supply the value and change it in the code to start and stop the synth,
or you can connect another cell to it.
-}
data SynthState
  = Started
  | Stopped
  deriving (Eq)

{- | A 'ParametrisedHandle' corresponding to one @vivid@/@SuperCollider@ synthesizer.

* The input of type 'SynthState' represent whether the synthesizer should currently be running or not.
  * When it is started, @'Just' synth@ represents the running synthesizer.
  * When it is stopped, 'Nothing' is returned.
* A change in @params@ will reload the synthesizer quickly,
  unless the types of the parameters change.
* A change in the 'SynthDef' or the types of the @params@ will 'release' the synthesizer and start a new one.
-}
vividHandleParametrised
  :: (VividAction m, Eq params, VarList params, Subset (InnerVars params) args, Elem "gate" args)
  => ParametrisedHandle (params, SynthDef args, SynthState) m (Maybe (Synth args))
vividHandleParametrised = ParametrisedHandle { .. }
  where
    createParametrised (params, synthDef, Started) = Just <$> synth synthDef params
    createParametrised (params, synthDef, Stopped) = defineSD synthDef >> pure Nothing

    destroyParametrised _ synthMaybe = traverse_ release synthMaybe

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
  :: (VividAction m, VarList params, Subset (InnerVars params) args, Typeable args, Data params, Eq params, VarList (Synth args), Elem "gate" args)
  => Cell (HandlingStateT m) (params, SynthDef args, SynthState) (Maybe (Synth args))
liveSynth = handlingParametrised vividHandleParametrised
