{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- | Live programs in the @'ExceptT' e m@ monad can stop execution by throwing an exception @e@.

Handling these exceptions is done by realising that live programs in fact form a monad in the exception type.
The interface is analogous to 'CellExcept'.
-}
module LiveCoding.LiveProgram.Except where

-- base
import Control.Monad (liftM, ap)
import Data.Data
import Data.Void (Void)

-- transformers
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

-- essence-of-live-coding
import LiveCoding.Cell (hoistCell, toLiveCell, liveCell, constM)
import LiveCoding.CellExcept (CellExcept, runCellExcept, once_)
import LiveCoding.Exceptions.Finite (Finite)
import LiveCoding.Forever
import LiveCoding.LiveProgram
import qualified LiveCoding.CellExcept as CellExcept

{- | A live program that can throw an exception.

* @m@: The monad in which the live program operates.
* @e@: The type of exceptions the live program can eventually throw.

'LiveProgramExcept' is a monad in the exception type.
This means that it is possible to chain several live programs,
where later programs can handle the exceptions thrown by the earlier ones.
'return' plays the role of directly throwing an exception.
'(>>=)' lets a handler decide which program to handle the exception with.

The interface is the basically the same as 'CellExcept',
and it is in fact a newtype around it.
-}
newtype LiveProgramExcept m e = LiveProgramExcept
  { unLiveProgramExcept :: CellExcept () () m e }
  deriving (Functor, Applicative, Monad)

-- | Execute a 'LiveProgramExcept', throwing its exceptions in the 'ExceptT' monad.
runLiveProgramExcept
  :: Monad m
  => LiveProgramExcept m e
  -> LiveProgram (ExceptT e m)
runLiveProgramExcept LiveProgramExcept { .. } = liveCell $ runCellExcept unLiveProgramExcept

{- | Lift a 'LiveProgram' into the 'LiveProgramExcept' monad.

Similar to 'LiveProgram.CellExcept.try'.
This will execute the live program until it throws an exception.
-}
try
  :: (Data e, Finite e, Functor m)
  => LiveProgram (ExceptT e m)
  -> LiveProgramExcept m e
try = LiveProgramExcept . CellExcept.try . toLiveCell

{- | Safely convert to 'LiveProgram's.

If the type of possible exceptions is empty,
no exceptions can be thrown,
and thus we can safely assume that it is a 'LiveProgram' in @m@.
-}
safely
  :: Monad m
  => LiveProgramExcept m Void
  -> LiveProgram m
safely = liveCell . CellExcept.safely . unLiveProgramExcept

{- | Run a 'LiveProgram' as a 'LiveProgramExcept'.

This is always safe in the sense that it has no exceptions.
-}
safe
  :: Monad m
  => LiveProgram m
  -> LiveProgramExcept m Void
safe = LiveProgramExcept . CellExcept.safe . toLiveCell

-- | Run a monadic action and immediately raise its result as an exception.
once :: (Monad m, Data e, Finite e) => m e -> LiveProgramExcept m e
once = LiveProgramExcept . once_

{- | Run a 'LiveProgramExcept' in a loop.

In the additional 'ReaderT e' context,
you can read the last thrown exception.
(For the first iteration, 'e' is set to the first argument to 'foreverELiveProgram'.)

This way, you can create an infinite loop,
with the exception as the loop variable.
-}
foreverELiveProgram
  :: (Data e, Monad m)
  => e -- ^ The loop initialisation
  -> LiveProgramExcept (ReaderT e m) e -- ^ The live program to execute indefinitely
  -> LiveProgram                  m
foreverELiveProgram e LiveProgramExcept { .. } = liveCell $ foreverE e $ hoistCell commute $ runCellExcept unLiveProgramExcept
  where
    commute :: ExceptT e (ReaderT r m) a -> ReaderT r (ExceptT e m) a
    commute action = ReaderT $ ExceptT . runReaderT (runExceptT action)

-- | Run a 'LiveProgramExcept' in a loop, discarding the exception.
foreverCLiveProgram
  :: (Data e, Monad m)
  => LiveProgramExcept m e
  -> LiveProgram       m
foreverCLiveProgram LiveProgramExcept { .. } = liveCell $ foreverC $ runCellExcept unLiveProgramExcept
