{-# LANGUAGE FlexibleContexts #-}
module LiveCoding.PortMidi.Internal where

-- base
import Control.Monad ( void )
import Control.Monad.IO.Class

-- transformers-base
import Control.Monad.Base

-- PortMidi
import Sound.PortMidi

-- essence-of-live-coding
import LiveCoding.Handle

-- | A marker witnessing that PortMidi was initialized
data PortMidiHandle = PortMidiHandle

portMidiHandle :: MonadBase IO m => Handle m PortMidiHandle
portMidiHandle = Handle
  { create = do
      liftBase initialize
      return PortMidiHandle
  , destroy = const $ liftBase $ void terminate
  }
