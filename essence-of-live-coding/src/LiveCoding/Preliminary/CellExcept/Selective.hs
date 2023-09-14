{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module LiveCoding.Preliminary.CellExcept.Selective where

-- base
import Data.Data
import Data.Either (fromRight)

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

--- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Exceptions
import LiveCoding.Preliminary.CellExcept.Applicative
import LiveCoding.Preliminary.CellExcept

-- selective
import Control.Selective


selectC' :: (Monad m, Data e) => Cell (ExceptT e m) i o -> (e -> Either a b) -> Cell (ExceptT (a -> b) m) i o -> Cell (ExceptT b m) i o
selectC' cell f Cell {..} = cell >>>= Cell
  { cellStep =
      \state (e, i) -> case f e of
        Left a -> withExceptT ($ a) $ cellStep state i
        Right b -> throwE b
  , ..
  }


data Select state1 state2 e
  = NotThrownS state1 state2
  | ThrownS state2 e
  deriving (Typeable, Data)


instance Monad m => Selective (CellExcept a b m) where
{-
  select (CellExcept fmap1 (cell1)) cellExcept2 =
    CellExcept
    { fmapExcept = _
    , cellExcept = selectC cell1 $ runCellExcept cellExcept2
    }
-}
{-
  select (CellExcept fmap1 cell1) (CellExcept fmap2 cell2) =
    CellExcept
    { fmapExcept = fmap2
    , cellExcept = selectC' cell1 fmap1 cell2
    -- , cellExcept = cell1 >>>= Cell
    --   { cellStep =
    --       \state (e, i) -> case fmap1 e of
    --         Left a -> withExceptT ($ a) $ cellStep state i
    --         Right b -> throwE b
    --   , ..
    --   }
    }
    -}
  -- FIXME Or simply use NoMigration to cache?
  -- FIXME And now only require Selective m!
  select (CellExcept fmap1 (Cell state1 step1)) (CellExcept fmap2 (Cell state2 step2)) =
    CellExcept
    { fmapExcept = \(e1, e2Maybe) -> case (fmap1 e1, e2Maybe) of
        (Left a, Just e2) -> fmap2 e2 a
        (Right b, Nothing) -> b
        _ -> error "CellExcept.select: Internal error (fmap1 was nondeterministic)"
    , cellExcept = Cell
      { cellStep = go
      , cellState = NotThrownS state1 state2
      }
    }
      where
        go (NotThrownS state1 state2) i = do
          result <- lift $ runExceptT $ step1 state1 i
          case result of
            Right (o, state1') -> return (o, NotThrownS state1' state2)
            Left e -> case fmap1 e of
                Left a -> go (ThrownS state2 e) i
                Right b -> throwE (e, Nothing)
        go (ThrownS state2 e) i = fmap (fmap (`ThrownS` e)) . withExceptT ((e, ) . Just) $ step2 state2 i
  select (CellExcept fmap1 cell1) (CellExcept fmap2 cell2) = select (CellExcept fmap1 $ toCell cell1) (CellExcept fmap2 $ toCell cell2)
