{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Support for [@vivid@](https://hackage.haskell.org/package/vivid),
-- a Haskell library for [SuperCollider](https://supercollider.github.io/).
--
-- With this module, you can create cells corresponding to synthesizers.
--
-- The synthesizers automatically start and stop on reload.
module LiveCoding.Vivid where

-- base
import Data.Foldable (traverse_)
import GHC.TypeLits (KnownSymbol)
-- vivid

-- essence-of-live-coding

import LiveCoding
import LiveCoding.Handle
import Vivid

-- | Whether a synthesizer should currently be running or not.
--
-- Typically, you will either statically supply the value and change it in the code to start and stop the synth,
-- or you can connect another cell to it.
data SynthState
  = Started
  | Stopped
  deriving (Eq)

-- | A 'ParametrisedHandle' corresponding to one @vivid@/@SuperCollider@ synthesizer.
--
-- Usually, you will want to use 'liveSynth' instead, it is easier to handle.
vividHandleParametrised ::
  (VividAction m, Eq params, VarList params, Subset (InnerVars params) args, Elem "gate" args) =>
  ParametrisedHandle (params, SynthDef args, SynthState) m (Maybe (Synth args))
vividHandleParametrised = ParametrisedHandle {..}
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

deriving instance Data SynthState

deriving instance KnownSymbol a => Data (I a)

-- | Create a synthesizer.
--
-- When you add 'liveSynth' to your live program,
-- it will be started upon reload immediately.
--
-- Feed the definition of the synthesizer and its current intended state to this cell.
-- The input has the form @(params, sdbody, synthState)@.
--
-- * A change in @params@ will reload the synthesizer quickly,
--  unless the types of the parameters change.
-- * A change in the @sdbody :: 'SDBody' ...@ or the _types_ of the @params@ will 'release' the synthesizer and start a new one.
-- * The input @synthState :: 'SynthState'@ represent whether the synthesizer should currently be running or not.
--  Changes in it quickly start or stop it.
--  * When it is started, @'Just' synth@ is returned, where @synth@ represents the running synthesizer.
--  * When it is stopped, 'Nothing' is returned.
--
-- You have to use 'envGate' in your @sdbody@,
-- or another way of gating your output signals
-- in order to ensure release of the synths without clipping.
--
-- For an example, have a look at the source code of 'sine'.
liveSynth ::
  ( VividAction m,
    Eq params,
    Typeable params,
    VarList params,
    Typeable (InnerVars params),
    Subset (InnerVars params) (InnerVars params),
    Elem "gate" (InnerVars params),
    Data params
  ) =>
  Cell
    (HandlingStateT m)
    (params, SDBody' (InnerVars params) [Signal], SynthState)
    (Maybe (Synth (InnerVars params)))
liveSynth = proc (params, sdbody, synthstate) -> do
  paramsFirstValue <- holdFirst -< params
  handlingParametrised vividHandleParametrised -< (params, sd paramsFirstValue sdbody, synthstate)

-- | Example sine synthesizer that creates a sine wave at the given input frequency.
sine :: (VividAction m) => Cell (HandlingStateT m) Float ()
sine = proc frequency -> do
  liveSynth
    -<
      ( ( 1 :: I "gate",
          2 :: I "fadeSecs",
          I frequency :: I "freq"
        ),
        out (0 :: Int) [envGate ~* sinOsc (freq_ (V :: V "freq"))],
        Started
      )
  returnA -< ()
