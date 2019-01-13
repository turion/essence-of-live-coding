\begin{comment}
\begin{code}
{-# LANGUAGE RankNTypes #-}

module LiveCoding.RuntimeIO where

-- base
import Control.Arrow
import Control.Monad
import Control.Concurrent
import Data.Data

-- essenceoflivecoding
import LiveCoding.Cell
import LiveCoding.Debugger
import LiveCoding.Migrate

\end{code}
\end{comment}

\begin{code}
-- type LiveProg = Cell IO () ()
type Debugger = forall s . Data s => s -> IO ()

launch :: LiveProgram -> IO (MVar LiveProgram)
launch cell = do
  var <- newMVar cell
  forkIO $ background var
  return var

debug :: Debugger -> LiveProgram -> IO ()
debug debugger (Cell state _) = debugger state

backgroundWithDebugger :: MVar LiveProgram -> Debugger -> IO ()
backgroundWithDebugger var debugger = forever $ do
  cell <- takeMVar var
  ((), cell') <- step cell ()
  debug debugger cell'
  combine var cell'

background :: MVar LiveProgram -> IO ()
background var = backgroundWithDebugger var $ stateShow >>> putStrLn

combine :: MVar LiveProgram -> LiveProgram -> IO ()
combine var prog = do
  success <- tryPutMVar var prog
  unless success $ do
    newProg <- takeMVar var
    combine var $ combineLiveProgram prog newProg

combineLiveProgram :: LiveProgram -> LiveProgram -> LiveProgram
combineLiveProgram (Cell oldState oldStep) (Cell newState newStep) = Cell (newState `migrate` oldState) newStep

update :: MVar LiveProgram -> LiveProgram -> IO ()
update var cell = void $ forkIO $ putMVar var cell
\end{code}
