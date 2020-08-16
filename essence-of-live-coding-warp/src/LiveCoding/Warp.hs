{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module LiveCoding.Warp
  ( runWarpC
  , runWarpC_
  , module X
  ) where

-- base
import Control.Concurrent
import Control.Monad.IO.Class

-- http-types
import Network.HTTP.Types as X

-- wai
import Network.Wai as X

-- warp
import Network.Wai.Handler.Warp

-- essence-of-live-coding
import LiveCoding

data WaiHandle = WaiHandle
  { requestVar  :: MVar Request
  , responseVar :: MVar Response
  , appThread   :: ThreadId
  }

waiHandle :: Port -> Handle IO WaiHandle
waiHandle port = Handle
  { create = do
      requestVar <- newEmptyMVar
      responseVar <- newEmptyMVar
      let app request respond = do
              putMVar requestVar request
              response <- takeMVar responseVar
              respond response
      appThread <- forkIO $ run port app
      return WaiHandle { .. }
  , destroy = \WaiHandle { .. } -> killThread appThread
  }

{- | Run a 'Cell' as a WARP application.

1. Starts a WARP application on the given port in a background thread
2. Block until the next request arrives
3. Supplies the cell with the input and the current request
4. Serve the response and return the output

Keep in mind that the resulting cell is blocking.
For a non-blocking cell, use 'LiveCoding.NonBlocking'.
-}

runWarpC
  :: Port
  -> Cell IO (a, Request) (b, Response)
  -> Cell (HandlingStateT IO) a b
runWarpC port cell = proc a -> do
  WaiHandle { .. } <- handling $ waiHandle port -< ()
  request <- arrM $ liftIO . takeMVar           -< requestVar
  (b, response) <- liftCell cell                     -< (a, request)
  arrM $ liftIO . uncurry putMVar               -< (responseVar, response)
  returnA                                       -< b


runWarpC_
  :: Port
  -> Cell IO Request Response
  -> Cell (HandlingStateT IO) () ()
runWarpC_ port cell = runWarpC port $ arr snd >>> cell >>> arr ((), )
