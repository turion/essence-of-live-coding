module LiveCoding.Gloss.GHCi where

-- base
import Control.Concurrent

-- foreign-store
import Foreign.Store

-- essenceoflivecoding
import LiveCoding.LiveProgram

-- essenceoflivecoding-gloss
import LiveCoding.Gloss

livegloss "" = livegloss "glossCell"
livegloss glossCell = return $ unlines
  [ "var <- playCell " ++ glossCell
  , "saveGloss var"
  ]
livereloadgloss "" = livereloadgloss "glossCell"
livereloadgloss glossCell  = return $ unlines
  [ ":reload"
  , "var <- loadGloss"
  , "updateGloss var " ++ glossCell
  ]
loadGloss :: IO (MVar GlossCell)
loadGloss = readStore (Store 0)
saveGloss :: MVar GlossCell -> IO ()
saveGloss var = writeStore (Store 0) var
