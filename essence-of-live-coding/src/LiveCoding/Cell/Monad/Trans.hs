{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Handling monad transformers.
-}
module LiveCoding.Cell.Monad.Trans where

-- base
import Control.Arrow (arr, (>>>))
import Data.Data (Data)

-- transformers
import Control.Monad.Trans.Reader (ReaderT (..), reader, runReaderT)
import Control.Monad.Trans.State.Strict (StateT (..), evalStateT, runStateT)
import Control.Monad.Trans.Writer.Strict

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Cell.Monad

-- | Push effectful state into the internal state of a cell
runStateC ::
  (Data stateT, Monad m) =>
  -- | A cell with a state effect
  Cell (StateT stateT m) a b ->
  -- | The initial state
  stateT ->
  -- | The cell, returning its current state
  Cell m a (Result stateT b)
runStateC cell stateT = hoistCellKleisliStateChange morph init cell
  where
    morph step State {..} a = do
      (Result stateInternal b, stateT) <- runStateT (step stateInternal a) stateT
      return $! Result State {..} (Result stateT b)
    init stateInternal = State {..}

-- | Like 'runStateC', but does not return the current state.
runStateC_ ::
  (Data stateT, Monad m) =>
  -- | A cell with a state effect
  Cell (StateT stateT m) a b ->
  -- | The initial state
  stateT ->
  Cell m a b
runStateC_ cell stateT = runStateC cell stateT >>> arr (\(Result _ b) -> b)

-- | The internal state of a cell to which 'runStateC' or 'runStateL' has been applied.
data State stateT stateInternal = State
  { stateT :: stateT
  , stateInternal :: stateInternal
  }
  deriving (Data, Eq, Show)

-- | Supply a 'ReaderT' environment before running the cell
runReaderC ::
  r ->
  Cell (ReaderT r m) a b ->
  Cell m a b
runReaderC r = hoistCell $ flip runReaderT r

-- | Supply a 'ReaderT' environment live
runReaderC' ::
  Monad m =>
  Cell (ReaderT r m) a b ->
  Cell m (r, a) b
runReaderC' = hoistCellKleisli_ $ \action (r, a) -> runReaderT (action a) r

-- | Inverse to 'runReaderC''
readerC' ::
  Monad m =>
  Cell m (r, a) b ->
  Cell (ReaderT r m) a b
readerC' = hoistCellKleisli_ $ \action a -> ReaderT $ \r -> action (r, a)

{- | Run the effects of the 'WriterT' monad,
   collecting all its output in the second element of the tuple.
-}
runWriterC :: (Monoid w, Monad m) => Cell (WriterT w m) a b -> Cell m a (w, b)
runWriterC = hoistCellOutput $ fmap reorder . runWriterT
  where
    reorder (Result s b, w) = Result s (w, b)
