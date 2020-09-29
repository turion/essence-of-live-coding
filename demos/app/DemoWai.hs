{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- base
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- bytestring
import Data.ByteString.Lazy.Char8 (pack)

-- wai
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)

-- essence-of-live-coding
import LiveCoding

import DemoWai.Env
import DemoWai.DemoWai1 (oldServer)
import DemoWai.DemoWai2 (newServer)

app :: Env -> Application
app Env { .. } request respond = do
  putMVar requestVar request
  response <- takeMVar responseVar
  respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        $ pack response

main :: IO ()
main = do
  putStrLn "Let's go!"
  responseVar <- newEmptyMVar
  requestVar <- newEmptyMVar
  let env = Env { .. }
  launchedProgram <- launch $ hoistLiveProgram (flip runReaderT env) oldServer
  forkIO $ run 8080 $ app env
  _ <- getLine
  update launchedProgram $ hoistLiveProgram (flip runReaderT env) newServer
  _ <- getLine
  putStrLn "That's it"
