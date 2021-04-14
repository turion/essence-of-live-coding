{-# LANGUAGE OverloadedStrings #-}

-- base
import Control.Monad (unless)
import System.Exit

-- bytestring
import Data.ByteString.Lazy

-- http-client
import Network.HTTP.Client hiding (Response)

-- essence-of-live-coding
import LiveCoding

-- essence-of-live-coding-warp
import LiveCoding.Warp

response :: ByteString -> Response
response = responseLBS status200 [("Content-Type", "text/plain")]

liveProgram :: ByteString -> LiveProgram (HandlingStateT IO)
liveProgram = liveCell . runWarpC_ 8080 . arr . const . response

testRequest :: Manager -> ByteString -> IO ()
testRequest manager expected = do
  request <- parseRequest "http://localhost:8080"
  response <- httpLbs request manager
  unless (responseBody response == expected) exitFailure

main :: IO ()
main = do
  launchedProgram <- launch $ liveProgram "hai"
  manager <- newManager defaultManagerSettings
  testRequest manager "hai"
  update launchedProgram $ liveProgram "hellooo"
  testRequest manager "hellooo"
