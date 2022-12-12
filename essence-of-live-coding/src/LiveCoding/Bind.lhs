\begin{comment}
\begin{code}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module LiveCoding.Bind where

-- base
import Control.Arrow
import Control.Concurrent (threadDelay)
import Data.Data
import Data.Void

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.CellExcept
import LiveCoding.Exceptions
import LiveCoding.LiveProgram
\end{code}
\end{comment}

\begin{comment}
%After this long excursion,
We can finally return to the example.
Let us again change the period of the oscillator,
only this time not manually,
but at the moment the position reaches 0:

\begin{code}
throwWhen0
  :: Monad m
  => Cell (ExceptT () m) Double Double
throwWhen0 = proc pos ->
  if pos < 0
  then throwC  -< ()
  else returnA -< pos

sineChangeE = do
  try $ sine 6 >>> throwWhen0
  try $ (constM $ lift $ putStrLn "I changed!")
      >>> throwC
  safe $ sine 10
\end{code}
\end{comment}

\begin{code}
sineWait
  :: Double -> CellExcept () String IO Void
sineWait t = do
  try $ arr (const "Waiting...") >>> wait 2
  safe $ sine t >>> arr asciiArt
\end{code}
This \mintinline{haskell}{do}-block can be read intuitively.
Initially, the first cell is executed,
which returns the message \mintinline{haskell}{"Waiting..."} every second.
After three seconds, it throws an exception,
which is handled by activating the sine generator.
Since all exceptions have been handled,
we leave the \mintinline{haskell}{CellExcept} context and run the resulting program:
\begin{code}
printSineWait :: LiveProgram IO
printSineWait = liveCell
  $   safely (sineWait 8)
  >>> printEverySecond
\end{code}
\verbatiminput{../demos/DemoSineWait.txt}
The crucial advantage of handling control flow this way
is that the \emph{control state}
-- that is, the information which exceptions have been thrown and which cell is currently active --
is encoded completely in the overall state of the live program,
and can thus be migrated automatically.
Let us rerun the above example,
but after the first \mintinline{haskell}{try} statement has already passed control to the sine generator
we shorten the period length of the sine wave and reload:
\verbatiminput{../demos/DemoSineWaitChange.txt}
The migrated program did not restart and wait again,
but remembered to immediately continue executing the sine generator from the same phase as before.
This is in contrast to simplistic approaches to live coding in which the control flow state is forgotten upon reload,
and restarted each time.

In most other programming languages where control flow is builtin,
this would typically require reworking the compiler or interpreter,
but in Haskell, we succeed entirely within the language.
