\begin{comment}
\begin{code}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module LiveCoding.Exceptions
  ( module LiveCoding.Exceptions
  , module Control.Monad.Trans.Except
  ) where
-- TODO Don't export newtype CellExcept and Functor here and haddock mark it

-- base
import Control.Arrow
import Data.Data

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Cell.Monad.Trans

\end{code}
\end{comment}

\paragraph{Throwing Exceptions}
No new concepts beyond the function \mintinline{haskell}{throwE :: Monad m => e -> ExceptT e m a}
from the package \texttt{transformers} \cite{jones1995functional, transformers} are needed:
\begin{code}
throwC
  :: Monad m
  => Cell (ExceptT e m) e arbitrary
throwC = arrM throwE
\end{code}
The above function simply throws the incoming exception.
To do this only if a condition is satisfied,
\mintinline{haskell}{if}-constructs can be used.
For example, this cell forwards its input for a given number of seconds,
and then throws an exception:
\begin{code}
wait
  :: Monad            m
  => Double
  -> Cell (ExceptT () m) a a
wait tMax = proc a -> do
  t <- localTime -< ()
  if t >= tMax
    then throwC  -< ()
    else returnA -< a
\end{code}

\begin{comment}
\begin{code}
throwIf :: Monad m => (a -> Bool) -> e -> Cell (ExceptT e m) a a
throwIf condition e = proc a -> do
  if condition a
  then throwC  -< e
  else returnA -< a

throwIf_ :: Monad m => (a -> Bool) -> Cell (ExceptT () m) a a
throwIf_ condition = throwIf condition ()
\end{code}
\end{comment}

\paragraph{Handling Exceptions}
In usual Haskell, the \mintinline{haskell}{ExceptT} monad transformer is handled by running it:
\begin{spec}
runExceptT :: ExceptT e m b -> m (Either e b)
\end{spec}
The caller can now decide how to handle the value \mintinline{haskell}{e},
should it occur.
This approach can be adapted to cells.
A function is supplied that runs the \mintinline{haskell}{ExceptT e} layer:
\begin{code}
runExceptC
  :: (Data e, Monad m)
  => Cell (ExceptT e m) a           b
  -> Cell            m  a (Either e b)
\end{code}
To appreciate its inner workings,
let us again look at the state it encapsulates:
\begin{code}
data ExceptState state e
  = NotThrown state
  | Exception e
  deriving Data
\end{code}
As long as no exception occurred,
\mintinline{haskell}{runExceptC cell} simply stores the state of \mintinline{haskell}{cell},
wrapped in the constructor \mintinline{haskell}{NotThrown}.
The output value \mintinline{haskell}{b} is passed on.
As soon as the exception \mintinline{haskell}{e} is thrown,
the state switches to \mintinline{haskell}{Exception e},
and the exception is output forever.
\begin{comment}
\begin{code}
runExceptC (Cell state step) = Cell { .. }
  where
    cellState = NotThrown state
    cellStep (NotThrown s) a = do
      stateExcept <- runExceptT $ step s a
      case stateExcept of
        Right (!b, s')
          -> return (Right b, NotThrown s')
        Left e
          -> cellStep (Exception e) a
    cellStep (Exception e) _
      = return (Left e, Exception e)
\end{code}
\end{comment}

As soon as the exception is thrown,
we can ``live bind'' it to further cells as an extra input:
\begin{code}
(>>>=) :: (Data e1, Monad m)
  => Cell (ExceptT e1    m)      a  b
  -> Cell (ExceptT    e2 m) (e1, a) b
  -> Cell (ExceptT    e2 m)      a  b
(>>>=) cell1 cell2 = proc a -> do
  eb <- liftCell $ runExceptC cell1 -< a
  case eb of
    Right b -> returnA -< b
    Left e  -> cell2   -< (e, a)
\end{code}
\fxwarning{If we don't do reader here, why do it with foreverE?}
We run the exception effect of the first cell.
Before it has thrown an exception, its output is simply forwarded.
As soon as the exception is thrown, the second cell is activated and fed with the input and the thrown exception.

\begin{comment}
\begin{code}
(>>>==) :: (Data e1, Monad m)
  => Cell (ExceptT e1             m)  a b
  -> Cell (ReaderT e1 (ExceptT e2 m)) a b
  -> Cell             (ExceptT e2 m)  a b
(>>>==) cell1 cell2 = proc a -> do
  eb <- liftCell $ runExceptC cell1 -< a
  case eb of
    Left e -> runReaderC' cell2 -< (e, a)
    Right b -> returnA -< b
\end{code}
\end{comment}

\input{../essence-of-live-coding/src/LiveCoding/Preliminary/CellExcept/Newtype.lhs}
