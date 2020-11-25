\begin{comment}
\begin{code}
module DemoWai.Env where

-- base
import Control.Concurrent.MVar

-- wai
import Network.Wai
\end{code}
\end{comment}

The server logic is shown in Figure \ref{fig:DemoWai1}.
It is initialised at 0 visitors.
The step function receives the number of past visitors and blocks on an \mintinline{haskell}{MVar} until a request (which is discarded) to the server arrives.
The number of visitors is incremented by 1,
and baked into a response,
which is in another \mintinline{haskell}{MVar}.
Finally, the updated state (the incremented number of visitors)
is returned,
and passed to the next step.

We then modify\footnote{%
The function \mintinline{haskell}{unpack} from the \texttt{bytestring} package converts between different kinds of strings.
\mintinline{haskell}{requestHeaders} from the \texttt{wai} package extracts HTTP headers,
such as the user agent name,
from a request.}
the server logic as in Figure \ref{fig:DemoWai2}.
Additionally to the number of visitors,
we also store the last user agent name
in the state, if it was sent.
For this, one more record field is added to the state type.

\begin{figure}
\begin{code}
data Env = Env
  { requestVar  :: MVar Request
  , responseVar :: MVar String
  }
\end{code}
\caption{DemoWai.lhs}
\label{fig:DemoWai}
\end{figure}

Let us run the old server,
and switch to the new one during execution.
From a console, we access the running server:
\begin{verbatim}
$ curl localhost:8080
Ye Olde Server greets visitor #1.
$ curl localhost:8080
Fancy Nu $3rv3r says HI to #2.
$ curl localhost:8080
Fancy Nu $3rv3r says HI to #3.
Last agent: curl/7.72.0
\end{verbatim}
It correctly remembered the number of past visitors upon reload and initialised the last user agent with the value \mintinline{haskell}{Nothing}.
When accessing the new server again,
it stored the user agent as expected.
