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
import Control.Monad
import Data.Data
import Data.Either (fromRight)
import Data.Void
-- import Prelude hiding ((>>=), return, (>>))

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

-- essenceoflivecoding
import LiveCoding.Commutable
import LiveCoding.Cell
-- import LiveCoding.CellExcept
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
  :: Double
  -> CellExcept IO () String Void
sineWait t = do
  try  $   arr (const "Waiting...")
       >>> wait 2
  safe $   sine t
       >>> arr asciiArt
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
  $   safely (sineWait 10)
  >>> printEverySecond
\end{code}
\verbatiminput{../DemoSineWait.txt}
The crucial advantage of handling control flow this way
is that the \emph{control state}
-- that is, the information which exceptions have been thrown and which cell is currently active --
is encoded completely in the overall state of the live program,
and can thus be migrated automatically.
Let us rerun the above example,
but after the first \mintinline{haskell}{try} statement has already passed control to the sine generator
we shorten the period length of the sine wave and reload:
\verbatiminput{../DemoSineWaitChange.txt}
The migrated program did not restart and wait again,
but remembered to immediately continue executing the sine generator from the same phase as before.
This is in contrast to simplistic approaches to live coding in which the control flow state is forgotten upon reload,
and restarted each time.

\fxerror{Move the following commented code into appendix and show it instead of the specs there}
\begin{comment}
\begin{code}

instance Monad m => Functor (CellExcept m a b) where
  fmap = liftM

instance Monad m => Applicative (CellExcept m a b) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (CellExcept m a b) where
  return = Return
  (>>=) = Bind

-- Rewrite with Operational
-- TODO Actually want to import
runCellExcept
  :: Monad           m
  => CellExcept      m  a b e
  -> Cell (ExceptT e m) a b
runCellExcept (Return e) = constM $ throwE e
runCellExcept (Try cell) = cell
runCellExcept (Bind (Try cell) g) = cell >>>== commute (runCellExcept . g)
runCellExcept (Bind (Return e) f) = runCellExcept $ f e
runCellExcept (Bind (Bind ce f) g) = runCellExcept $ Bind ce $ \e -> Bind (f e) g

try :: (Data e, Commutable e) => Cell (ExceptT e m) a b -> CellExcept m a b e
try = Try
safely
  :: Monad      m
  => CellExcept m a b Void
  -> Cell       m a b
safely = hoistCell discardVoid . runCellExcept
discardVoid
  :: Functor      m
  => ExceptT Void m a
  ->              m a
discardVoid
  = fmap (either absurd id) . runExceptT
safe :: Monad m => Cell m a b -> CellExcept m a b Void
safe cell = try $ liftCell cell
\end{code}
\end{comment}
