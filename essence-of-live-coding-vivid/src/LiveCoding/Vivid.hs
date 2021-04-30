{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
module LiveCoding.Vivid where

-- vivid
import Vivid

-- essence-of-live-coding
import LiveCoding.Handle
import Data.Foldable (traverse_)

vividHandle
  :: (VividAction m, VarList params, Subset (InnerVars params) args)
  => SynthDef args
  -> params
  -> Handle m (Synth args)
vividHandle synthDef params = Handle
  { create  = synth synthDef params
  , destroy = free
  }

vividHandleRelease
  :: (VividAction m, VarList params, Elem "gate" args, Subset (InnerVars params) args)
  => SynthDef args
  -> params
  -> Handle m (Synth args)
vividHandleRelease synthDef params = Handle
  { create  = synth synthDef params
  , destroy = release
  }

data SynthState
  = NotYetStarted
  | Started
  | Released

vividHandleParametrised
  :: (VividAction m, VarList params, Subset (InnerVars params) args)
  => SynthDef args
  -> ParametrisedHandle m (params, SynthState) (Maybe (Synth args))
vividHandleParametrised synthDef = ParametrisedHandle { .. }
  where
    createParametrised (params, NotYetStarted) = defineSD synthDef >> pure Nothing
    createParametrised (params, Started) = Just <$> synth synthDef params
    createParametrised (params, Released) = pure Nothing
    destroyParametrised _ synthMaybe = traverse_ free synthMaybe
