{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{- |
Handling monad transformers.
-}
module LiveCoding.Cell.Monad.Trans where

-- base
import Control.Arrow (arr, (>>>))
import Data.Data (Data)

-- transformers
import Control.Monad.Trans.Reader (runReaderT, ReaderT)
import Control.Monad.Trans.State.Strict (StateT (..), runStateT, evalStateT)
import Control.Monad.Trans.Writer.Strict

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Cell.Monad

-- | Push effectful state into the internal state of a cell
runStateC
  :: (Data stateT, Monad m)
  => Cell (StateT stateT m) a  b
  -- ^ A cell with a state effect
  -> stateT
  -- ^ The initial state
  -> Cell                m  a (b, stateT)
  -- ^ The cell, returning its current state
runStateC cell stateT = hoistCellKleisliStateChange morph init cell
  where
    morph step State { .. } a = do
      ((b, stateInternal), stateT) <- runStateT (step stateInternal a) stateT
      return ((b, stateT), State { .. })
    init stateInternal = State { .. }

-- | Like 'runStateC', but does not return the current state.
runStateC_
  :: (Data stateT, Monad m)
  => Cell (StateT stateT m) a b
  -- ^ A cell with a state effect
  -> stateT
  -- ^ The initial state
  -> Cell                m  a b
runStateC_ cell stateT = runStateC cell stateT >>> arr fst

-- | The internal state of a cell to which 'runStateC' or 'runStateL' has been applied.
data State stateT stateInternal = State
  { stateT :: stateT
  , stateInternal :: stateInternal
  }
  deriving (Data, Eq, Show)

-- runAccumC
--   :: (Data accumT, Monad m)
--   => Cell (AccumT accumT m) a  b
--   -- ^ A cell with an accum state effect
--   -> accumT
--   -- ^ The initial state
--   -> Cell                m  a (b, accumT)
--   -- ^ The cell, returning its current state
-- runAccumC cell stateT = _

-- | The internal state of a cell to which 'runAccumC' or 'runAccumL' has been applied.
data Accum accumT accumInternal = Accum
  { accumT :: accumT
  , accumInternal :: accumInternal
  }
  deriving (Data, Eq, Show)

-- | Supply a 'ReaderT' environment before running the cell
runReaderC
  ::               r
  -> Cell (ReaderT r m) a b
  -> Cell            m  a b
runReaderC r = hoistCell $ flip runReaderT r

-- | Supply a 'ReaderT' environment live
runReaderC'
  :: Monad m
  => Cell (ReaderT r m) a b
  -> Cell m (r, a) b
runReaderC' = hoistCellKleisli_ $ \action (r, a) -> runReaderT (action a) r

-- | Run the effects of the 'WriterT' monad,
--   collecting all its output in the second element of the tuple.
runWriterC :: (Monoid w, Monad m) => Cell (WriterT w m) a b -> Cell m a (w, b)
runWriterC = hoistCellOutput $ fmap reorder . runWriterT
  where
    reorder ((b, s), w) = ((w, b), s)
