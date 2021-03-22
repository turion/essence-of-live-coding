{-# LANGUAGE DataKinds #-}
module LiveCoding.Vivid where

-- vivid
import Vivid

-- essence-of-live-coding
import LiveCoding.Handle

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
