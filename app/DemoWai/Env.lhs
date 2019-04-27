\begin{comment}
\begin{code}
module DemoWai.Env where

-- base
import Control.Concurrent.MVar

-- bytestring
import Data.ByteString.Lazy.Char8

-- wai
import Network.Wai
\end{code}
\end{comment}

The server logic is shown in Figure \ref{fig:DemoWai1}.
It is initialised at 0 visitors.
The step function receives the number of past visitors and blocks on an \mintinline{haskell}{MVar} until a request (which is discarded) to the server arrives.
The number of visitors is incremented by 1,
and sent back by storing it in an \mintinline{haskell}{MVar} as response.
Finally, the updated state (the incremented number of visitors)
is returned,
and passed to the next step.

We then modify the server logic as in Figure \ref{fig:DemoWai2}.
Additionally to the number visitors, we also store the last user agent (an HTTP header)
in the state, if it was sent.
For this, one more record field is added to the state type.

\fxwarning{Test reloading with a single file at least once.}

\begin{figure}
\begin{code}
data Env = Env
  { requestVar  :: MVar Request
  , responseVar :: MVar ByteString
  }
\end{code}
\caption{DemoWai.lhs}
\label{fig:DemoWai}
\end{figure}

Let us run the old server,
and switch to the new server during execution.
From the console, we access the running server:
\begin{verbatim}
$ curl localhost:8080
This is Ye Olde Server.
You are visitor #1.
$ curl localhost:8080
This is Fancy Nu $3rv3r!
You are visitor #2.
$ curl localhost:8080
This is Fancy Nu $3rv3r!
You are visitor #3.
Last agent: curl/7.64.0
\end{verbatim}
It correctly remembered the number of past visitors upon reload and initialised the last user agent with the value \mintinline{haskell}{Nothing}.
When accessing the new server again,
it stored the user agent as expected.
