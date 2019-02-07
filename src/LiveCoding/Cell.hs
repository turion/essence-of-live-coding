\begin{code}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module LiveCoding.Cell where

-- base
import Control.Arrow
import Control.Category
import Data.Data
import Prelude hiding ((.), id)

-- transformers
import Control.Monad.Trans.Except

-- | A cell
data Cell m a b where
  Cell :: Data s =>
    { cellState :: s
    , cellStep  :: s -> a -> m (b, s)
    }

step :: Monad m => Cell m a b -> a -> m (b, Cell a b)
step Cell { .. } a = do
  (b, cellState') <- cellStep cellState a
  return Cell { cellState', .. }

type LiveProgram = Cell IO () ()

hoistCell :: (forall x . m1 x -> m2 x) -> Cell m1 a b -> Cell m2 a b
hoistCell morph Cell { .. } = Cell
  { cellStep = \s a -> morph $ cellStep s a
  , ..
  }

liftCell :: MonadTrans t => Cell m a b -> Cell (t m) a b
liftCell = hoistCell lift

instance Monad m => Category (Cell m) where
  id = Cell
    { cellState = ()
    , cellStep  = \() a -> return (a, ())
    }

  cell1 >>> cell2 = Cell
    { cellState = (cellState cell1, cellState cell2)
    , cellStep  = \a (cellState1, cellState2) -> do
        (b, cellState1') <- cellStep cell1 cellState1 a
        (c, cellState2') <- cellStep cell2 cellState2 b
        return (c, (cellState1', cellState2'))
    }

instance Monad m => Arrow (Cell m) where
  arr f = Cell
    { cellState = ()
    , cellStep = \() a -> return (f a, ())
    }

  first Cell { .. } = Cell { .. }
    where
      cellStep cellState (a, c) = do
        (b, cellState') <- cellStep cellState a
        return ((b, c), cellState')

arrM :: (a -> m b) -> Cell m a b
arrM f = Cell
  { cellState = ()
  , cellStep  = \() a -> (, ()) <$> f a
  }

constM :: m b -> Cell m a b
constM = arrM . const

instance Monad m => ArrowChoice (Cell m) where
  left Cell { .. } = Cell { .. }
    where
      cellStep cellState (Left a) = do
        (b, cellState') <- cellStep cellState a
        return (Left b, cellState')
      cellStep cellState (Right b) = return (Right b, cellState)

data CellExcept m a b e where
  CellExcept :: Data e' =>
    { fmapExcept :: e' -> e
    , cellExcept :: Cell (ExceptT e' m) a b
    }

runCellExcept :: CellExcept m a b e -> Cell (ExceptT e m) a b
runCellExcept CellExcept { .. } = hoistCell (withExceptT fmapExcept) cellExcept

try :: Cell (ExceptT e m) a b -> CellExcept m a b e
try = CellExcept id

safely :: CellExcept m a b Void -> Cell m a b
safely = hoistCell discardVoid . runCellExcept
  where
    discardVoid = fromLeft (error "safely: Received Left") . runExceptT

safe :: Cell m a b -> CellExcept m a b void
safe = try . liftCell

andThen
  :: Cell (ExceptT  e1      m) a b
  -> Cell (ExceptT      e2  m) a b
  -> Cell (ExceptT (e1, e2) m) a b
cell1 `andThen` cell2 = Cell { .. }
  where
    cellState = Right (cellState cell1, cellState cell2)
    cellStep (Right (cellState1, cellState2)) a = do
      continueExcept <- cellStep
      thing
\end{code}
