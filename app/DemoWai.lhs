\begin{comment}
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- base
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- bytestring
import Data.ByteString.Lazy.Char8 hiding (putStrLn, getLine)

-- wai
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

-- essenceoflivecoding
import LiveCoding.LiveProgram
import LiveCoding.RuntimeIO

app :: Env -> Application
app Env { .. } request respond = do
  putMVar requestVar request
  response <- takeMVar responseVar
  respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        response

main :: IO ()
main = do
  putStrLn "Let's go!"
  responseVar <- newEmptyMVar
  requestVar <- newEmptyMVar
  let env = Env { .. }
  liveProgVar <- launch $ runReaderT oldServer env
  forkIO $ run 8080 $ app env
  _ <- getLine
  update liveProgVar $ runReaderT newServer env
  _ <- getLine
  putStrLn "That's it"
\end{code}
\end{comment}

\fxfatal{Write text for WAI demo}

\subsection{Livecoding a webserver}
\begin{code}
data Env = Env
  { requestVar  :: MVar Request
  , responseVar :: MVar ByteString
  }

server :: String -> LiveProgram (ReaderT Env IO)
server name = LiveProgram
  { state = 0 :: Integer
  , step  = \nVisitorsOld -> do
      _ <- lift takeMVar =<< asks requestVar
      let nVisitorsNew = nVisitorsOld + 1
      asks responseVar >>= lift putMVar $ pack
        $  "This is " ++ name ++ ".\n"
        ++ "You are visitor #"
        ++ show nVisitorsNew ++ "."
      return nVisitorsNew
  }

oldServer = server "Ye Olde Server"
newServer = server "Fancy Nu $3rv3r"
\end{code}
