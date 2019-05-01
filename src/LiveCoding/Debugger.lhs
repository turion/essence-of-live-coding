\begin{comment}
\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module LiveCoding.Debugger where

-- base
import Data.Data
import Data.Maybe (fromMaybe, fromJust)
import Data.Proxy
import Data.Typeable
import Unsafe.Coerce

-- essenceoflivecoding
import LiveCoding.Cell
import LiveCoding.Exceptions
import LiveCoding.Forever

-- syb
import Data.Generics.Aliases
import Data.Generics.Text (gshow)
\end{code}
\end{comment}

\begin{code}
newtype Debugger = Debugger
  { debugState
      :: forall s . Data s => s -> IO s
  }

instance Semigroup Debugger where
  debugger1 <> debugger2 = Debugger $ \s -> debugState debugger1 s >>= debugState debugger2

instance Monoid Debugger where
  mempty = noDebugger

noDebugger = Debugger $ return
statePrint = Debugger $ \s -> putStrLn (stateShow s) >> return s
\end{code}
