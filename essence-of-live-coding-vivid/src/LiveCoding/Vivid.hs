{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
module LiveCoding.Vivid where

-- base
import Data.Foldable (traverse_)
import GHC.TypeLits ( KnownSymbol )

-- vivid
import Vivid

-- essence-of-live-coding
import LiveCoding.Handle
import LiveCoding

data SynthState
  = Started
  | Stopped
  deriving (Eq)

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
deriving instance Data SynthState
deriving instance KnownSymbol a => Data (I a)

holdFirstValue :: (Data a, Monad m) => Cell m a a
holdFirstValue = Cell { .. }
  where
    cellState = Nothing
    cellStep Nothing x = return (x, Just x)
    cellStep (Just s) _ = return (s, Just s)

liveSynth
  :: (VividAction m, VarList params, Subset (InnerVars params) (InnerVars params),
    Typeable params, Typeable (InnerVars params),
    Eq params, Elem "gate" (InnerVars params), Data params)
  => Cell (HandlingStateT m) (params, SDBody' (InnerVars params) [Signal], SynthState) (Maybe (Synth (InnerVars params)))
liveSynth = proc (params, sdbody, synthstate) -> do
  paramsFirstValue <- holdFirstValue -< params
  handlingParametrised vividHandleParametrised -< (params, sd paramsFirstValue sdbody, synthstate)
