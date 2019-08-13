module LiveCoding.Pulse.GHCi where

-- base
import Control.Concurrent

-- foreign-store
import Foreign.Store

-- essence-of-live-coding
import LiveCoding.LiveProgram

-- essence-of-live-coding-pulse
import LiveCoding.Pulse

livepulse "" = livepulse "pulseCell"
livepulse pulseCell = return $ unlines
  [ "var <- playPulseCell " ++ pulseCell
  , "savePulse var"
  ]
livereloadpulse "" = livereloadpulse "pulseCell"
livereloadpulse pulseCell  = return $ unlines
  [ ":reload"
  , "var <- loadPulse"
  , "updatePulse var " ++ pulseCell
  ]
loadPulse :: IO (MVar PulseCell)
loadPulse = readStore (Store 0)
savePulse :: MVar PulseCell -> IO ()
savePulse var = writeStore (Store 0) var
