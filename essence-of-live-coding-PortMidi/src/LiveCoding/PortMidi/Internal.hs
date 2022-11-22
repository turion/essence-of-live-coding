module LiveCoding.PortMidi.Internal where

-- base
import Control.Monad (void)
import Control.Monad.IO.Class

-- PortMidi
import Sound.PortMidi

-- essence-of-live-coding
import LiveCoding.Handle

-- | A marker witnessing that PortMidi was initialized
data PortMidiHandle = PortMidiHandle

portMidiHandle :: MonadIO m => Handle m PortMidiHandle
portMidiHandle =
  Handle
    { create = do
        liftIO initialize
        return PortMidiHandle
    , destroy = const $ liftIO $ void terminate
    }
