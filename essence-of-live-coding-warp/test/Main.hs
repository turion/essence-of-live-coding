{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

-- base
import System.Exit

-- http-client
import Network.HTTP.Client hiding (Response)

-- essence-of-live-coding
import LiveCoding

-- essence-of-live-coding-warp
import LiveCoding.Warp

response :: Response
response = responseLBS status200 [("Content-Type", "text/plain")] "hai"

main :: IO ()
main = do
  launch $ liveCell $ runHandlingStateC $ runWarpC_ 8080 $ arr $ const response
  manager <- newManager $ defaultManagerSettings
  request <- parseRequest "http://localhost:8080"
  response <- httpLbs request manager
  if responseBody response == "hai"
    then return ()
    else exitFailure
