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

-- I believe there is a bug here where a request is missed if the app blocks because the requestVar isn't emptied, or the response not filled.

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
2. Waits until the next request arrives, outputting 'Nothing' in the meantime
3. Supplies the cell with the input and the current request
4. Serve the response and return the output
-}

runWarpC
  :: Port
  -> Cell IO (a, Request) (b, Response)
  -> Cell (HandlingStateT IO) a (Maybe b)
runWarpC port cell = proc a -> do
  WaiHandle { .. } <- handling $ waiHandle port -< ()
  requestMaybe <- arrM $ liftIO . tryTakeMVar   -< requestVar
  case requestMaybe of
    Just request -> do
      (b, response) <- liftCell cell  -< (a, request)
      arrM $ liftIO . uncurry putMVar -< (responseVar, response)
      returnA                         -< Just b
    Nothing -> do
      arrM $ liftIO . threadDelay     -< 1000 -- Prevent too much CPU load
      returnA                         -< Nothing

runWarpC_
  :: Port
  -> Cell IO Request Response
  -> Cell (HandlingStateT IO) () ()
runWarpC_ port cell = runWarpC port (arr snd >>> cell >>> arr ((), )) >>> arr (const ())
