module LiveCoding.PortMidi.Internal where

-- base
import Control.Monad (void)
import Control.Monad.IO.Class
-- PortMidi

-- essence-of-live-coding
import LiveCoding.Handle
import Sound.PortMidi

-- | A marker witnessing that PortMidi was initialized
data PortMidiHandle = PortMidiHandle

portMidiHandle :: MonadIO m => Handle m PortMidiHandle
portMidiHandle =
  Handle
    { create = do
        liftIO initialize
        return PortMidiHandle,
      destroy = const $ liftIO $ void terminate
    }
