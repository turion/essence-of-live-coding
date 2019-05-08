\begin{comment}
\begin{code}
{-# LANGUAGE Arrows #-}
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

-- essenceoflivecoding
import LiveCoding.Cell

\end{code}
\end{comment}

\paragraph{The return of the monad?}
\fxwarning{This paragraph name makes no sense}
For simply throwing exceptions, no new concepts are needed:
\begin{code}
throwC
  :: Monad m
  => Cell (ExceptT e m) e arbitrary
throwC = arrM throwE
\end{code}
The above function simply throws the incoming exception.
To throw an exception only if a certain condition is satisfied,
\mintinline{haskell}{if}-constructs in arrow syntax can be readily used.

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

\input{../src/LiveCoding/CellExcept/Newtype.lhs}

\paragraph{Handling exceptions}
In usual Haskell, the \mintinline{haskell}{ExceptT e} monad transformer is handled by running it in its underlying context:
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
runExceptC
  :: (Data e, Monad m)
  => Cell (ExceptT e m) a           b
  -> Cell            m  a (Either e b)
runExceptC (Cell state step) = Cell { .. }
  where
    cellState = NotThrown state
    cellStep (NotThrown s) a = do
      stateExcept <- runExceptT $ step s a
      case stateExcept of
        Right (b, s')
          -> return (Right b, NotThrown s')
        Left e
          -> cellStep (Exception e) a
    cellStep (Exception e) _
      = return (Left e, Exception e)
\end{code}
\end{comment}

As soon as the exception is thrown,
we can ``bind'' it to further cells as an extra input:
\begin{code}
(>>>=) :: (Data e1, Monad m)
  => Cell (ExceptT e1    m)      a  b
  -> Cell (ExceptT    e2 m) (e1, a) b
  -> Cell (ExceptT    e2 m)      a  b
(>>>=) cell1 cell2 = proc a -> do
  eb <- liftCell $ runExceptC cell1 -< a
  case eb of
    Right b -> returnA -< b
    Left e -> cell2 -< (e, a)
\end{code}
\fxwarning{If we don't do reader here, why do it with foreverE?}
We run the exception effect of the first cell.
Before it has thrown an exception, its output is simply forwarded.
As soon as the exception is thrown, the second cell is activated and fed with the input and the thrown exception.

\begin{comment}
\begin{code}
(>>>=) :: (Data e1, Monad m)
  => Cell (ExceptT e1             m)  a b
  -> Cell (ReaderT e1 (ExceptT e2 m)) a b
  -> Cell             (ExceptT e2 m)  a b
(>>>=) cell1 cell2 = proc a -> do
  eb <- liftCell $ runExceptC cell1 -< a
  case eb of
    Left e -> runReaderC' cell2 -< (e, a)
    Right b -> returnA -< b
--cell1 >>>= cell2 = liveBind (liftCell $ runExceptC cell1) (runReaderC' cell2)

runReaderC' :: Cell (ReaderT r m) a b -> Cell m (r, a) b
runReaderC' Cell { .. } = Cell
  { cellStep = \state (r, a) -> runReaderT (cellStep state a) r
  , ..
  }
\end{code}
\end{comment}
Armed with this new control flow operator,
called ``live bind'',
we can already implement quite a few nontrivial programs.
\fxerror{example}
The crucial advantage of handling control flow this way
is that the \emph{control state}
-- that is, the information which exceptions have been thrown and which cell is currently active --
is encoded completely in the overall state of the live program,
and can thus be migrated automatically.
If \mintinline{haskell}{cell1 >>>= cell2} has already passed control to \mintinline{haskell}{cell2},
and we edit the definition of \mintinline{haskell}{cell2} and reload,
then the migrated state will correctly migrate the state of \mintinline{haskell}{cell2} and remember to execute it.
This is in contrast to simplistic approaches to live coding in which the control flow state is forgotten upon reload,
and restarted.
\fxerror{example}

Like the sequential application operator \mintinline{haskell}{<*>} of the \mintinline{haskell}{Applicative} class
can be defined from \mintinline{haskell}{>>=},
it can also be defined from \mintinline{haskell}{>>>=}.
As a technical tour-de-force,
even a constrained \mintinline{haskell}{Monad} instance for \mintinline{haskell}{CellExcept} can be derived.
This is shown at length in the separate appendix.
