{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- base
import Data.Data

-- transformers
import Control.Monad.Trans.Except

--- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Exceptions
import LiveCoding.Preliminary.CellExcept.Applicative
import LiveCoding.Preliminary.CellExcept

-- selective
import Control.Selective

selectC :: (Monad m, Data a, Data b) => Cell (ExceptT (Either a b) m) i o -> Cell (ExceptT (a -> b) m) i o -> Cell (ExceptT b m) i o
selectC cell Cell {..} = cell >>>= Cell
  { cellStep =
      \state -> \case
        (Left a, i) -> withExceptT ($ a) $ cellStep state i
        (Right b, _i) -> throwE b
  , ..
  }

instance Monad m => Selective (CellExcept a b m) where
  select (CellExcept fmap1 cell1) cellExcept2 =
    CellExcept
    { fmapExcept = _
    , cellExcept = selectC cell1 $ runCellExcept cellExcept2
    }
