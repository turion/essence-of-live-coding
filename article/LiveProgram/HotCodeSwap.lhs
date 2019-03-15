\begin{figure}
\begin{code}
hotCodeSwap
  :: (s -> s')
  -> LiveProgram s'
  -> LiveProgram s
  -> LiveProgram s'
hotCodeSwap migrate newProgram oldProgram
  = LiveProgram
    { liveState = migrate $ liveState oldProgram
    , liveStep  = liveStep newProgram
    }
\end{code}
\caption{\texttt{HotCodeSwap.lhs}}
\label{fig:hot code swap}
\end{figure}
\fxwarning{The thing with the MVar doesn't work on the spot anymore. But it can still work with a "typed" handle. Every time you swap, you get a new handle that carries the currently saved type. Worth commenting upon?
It's getting even more complicated: We have to kill the old MVar and create a new one every time we update. Then we also have to kill the old server, or update the ticking function}
\begin{comment}
\begin{code}
type LiveRef s = (MVar (LiveProgram s), MVar (IO ()))
launch :: LiveProgram s -> IO (LiveRef s)
launch liveProg = do
  var <- newMVar liveProg
  var <- newMVar $ tick var
  forkIO $ forever $ do
    action <- takeMVar var
    action
    tryPutMVar var action
  return (var, var)

tick :: MVar (LiveProgram s) -> IO ()
tick var = do
  LiveProgram {..} <- takeMVar var
  liveState' <- liveStep liveState
  putMVar var LiveProgram { liveState = liveState', .. }

swapWith :: (s -> s') -> LiveProgram s' -> LiveRef s -> IO (LiveRef s')
swapWith migrate (LiveProgram _newState newStep) (progVar, actionVar) = do
  _ <- takeMVar actionVar
  LiveProgram oldState oldStep <- takeMVar progVar
  let newProg = LiveProgram (migrate oldState) newStep
  newProgVar <- newMVar newProg
  putMVar actionVar $ tick newProgVar
  return (newProgVar, actionVar)
\end{code}
\end{comment}
