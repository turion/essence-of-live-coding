\begin{comment}
\begin{code}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module LiveCoding.Commutable where

-- base
import Control.Arrow
import GHC.Generics
import Data.Data
import Data.Void

-- transformers
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

-- essenceoflivecoding
import LiveCoding.Cell
import LiveCoding.Exceptions (runReaderC')
-- import LiveCoding.CellExcept
\end{code}
\end{comment}

\begin{code}
data CellExcept m a b e = forall e' .
  (Data e', ECommutable e') => CellExcept
  { fmapExcept :: e' -> e
  , cellExcept :: Cell (ExceptT e' m) a b
  }

class Commutable e where
  commute :: Monad m => (e -> Cell m a b) -> Cell (ReaderT e m) a b

  default commute :: (Generic e, GCommutable (Rep e), Monad m) => (e -> Cell m a b) -> Cell (ReaderT e m) a b
  commute handler = hoistCell (withReaderT from) $ gcommute $ handler . to

class GCommutable f where
  gcommute :: Monad m => (f e -> Cell m a b) -> Cell (ReaderT (f e) m) a b

instance GCommutable f => GCommutable (M1 a b f) where
  gcommute handler = hoistCell (withReaderT unM1) $ gcommute $ handler . M1

instance Commutable e => GCommutable (K1 a e) where
  gcommute handler = hoistCell (withReaderT unK1) $ commute $ handler . K1

instance GCommutable V1 where
  gcommute _ = error "gcommute: Can't commute with an empty type"

instance Commutable Void where
  commute _ = error "Nope"

instance GCommutable U1 where
  gcommute handler = liftCell $ handler U1

instance Commutable () where

instance Commutable Bool where
  commute handler = proc a -> do
    bool <- constM ask -< ()
    if bool
    then liftCell $ handler True  -< a
    else liftCell $ handler False -< a

instance (GCommutable eL, GCommutable eR) => GCommutable (eL :+: eR) where
  gcommute handler
    = let
          cellLeft  = runReaderC' $ gcommute $ handler . L1
          cellRight = runReaderC' $ gcommute $ handler . R1
          gdistribute (L1 eR) a = Left  (eR, a)
          gdistribute (R1 eL) a = Right (eL, a)
    in
      proc a -> do
        either12 <- constM ask -< ()
        liftCell (cellLeft ||| cellRight) -< gdistribute either12 a

instance (Commutable e1, Commutable e2) => Commutable (Either e1 e2) where

instance (GCommutable e1, GCommutable e2) => GCommutable (e1 :*: e2) where
  gcommute handler = hoistCell guncurryReader $ gcommute $ gcommute . gcurry handler
    where
      gcurry f e1 e2 = f (e1 :*: e2)
      guncurryReader a = ReaderT $ \(r1 :*: r2) -> runReaderT (runReaderT a r1) r2

data CellExceptReader m a b e1 e2 = forall e . (Data e, ECommutable e) => CellExceptReader
  { newFmap :: (e -> e2)
  , newCell :: Cell (ReaderT e1 (ExceptT e m)) a b
  }

wraparound :: CellExceptReader m a b e1 e2 -> CellExcept (ReaderT e1 m) a b e2
wraparound CellExceptReader { .. } = CellExcept
  { fmapExcept = newFmap
  , cellExcept = hoistCell commuteReaderExcept newCell
  }
  where
    commuteReaderExcept :: ReaderT e1 (ExceptT e m) a -> ExceptT e (ReaderT e1 m) a
    commuteReaderExcept a = ExceptT $ ReaderT $ \e1 -> runExceptT $ runReaderT a e1

mapCellExceptReader :: (e1b -> e1) -> CellExceptReader m a b e1 e2 -> CellExceptReader m a b e1b e2
mapCellExceptReader f CellExceptReader { .. } = CellExceptReader
  { newCell = hoistCell (withReaderT f) newCell
  , ..
  }

class ECommutable e1 where
  ecommute :: Monad m => (e1 -> CellExcept m a b e2) -> CellExceptReader m a b e1 e2

  default ecommute :: (Generic e1, GECommutable (Rep e1), Monad m) => (e1 -> CellExcept m a b e2) -> CellExceptReader m a b e1 e2
  ecommute handler = mapCellExceptReader from $ gecommute $ handler . to

class GECommutable f where
  gecommute :: Monad m => (f e1 -> CellExcept m a b e2) -> CellExceptReader m a b (f e1) e2

instance GECommutable f => GECommutable (M1 a b f) where
  gecommute handler = mapCellExceptReader unM1 $ gecommute $ handler . M1

instance ECommutable e => GECommutable (K1 a e) where
  gecommute handler = mapCellExceptReader unK1 $ ecommute $ handler . K1

instance GECommutable V1 where
  gecommute _ = error "gecommute: Can't ecommute with an empty type"

instance ECommutable Void where
  ecommute _ = error "Nope"

instance GECommutable U1 where
  gecommute handler = go $ handler U1
    where
      go CellExcept { .. } = CellExceptReader
        { newFmap = fmapExcept
        , newCell = liftCell $ cellExcept
        }

instance ECommutable () where
  -- ecommute handler = liftCell $ handler ()

instance ECommutable Bool where
  ecommute handler = go (handler True) (handler False)
    where
      go (CellExcept fmapTrue cellTrue) (CellExcept fmapFalse cellFalse) = CellExceptReader
        { newFmap = \bla -> case bla of
            Left eL -> fmapTrue eL
            Right eR -> fmapFalse eR
        , newCell = proc a -> do
            bool <- constM ask -< ()
            if bool
            then liftCell $ hoistCell (withExceptT Left) cellTrue  -< a
            else liftCell $ hoistCell (withExceptT Right) cellFalse -< a
        }

instance (GECommutable eL, GECommutable eR) => GECommutable (eL :+: eR) where
  gecommute handler = go (gecommute $ handler . L1) (gecommute $ handler . R1)
    where
      go (CellExceptReader fmapL cellL) (CellExceptReader fmapR cellR) =
        let
          cellLeft  = hoistCell (withExceptT Left ) $ runReaderC' cellL
          cellRight = hoistCell (withExceptT Right) $ runReaderC' cellR
        in CellExceptReader
          { newFmap = \bla -> case bla of
              Left eL -> fmapL eL
              Right eR -> fmapR eR
          , newCell = proc a -> do
              either12 <- constM ask -< ()
              liftCell (cellLeft ||| cellRight) -< gdistribute either12 a
          }
      gdistribute (L1 eR) a = Left  (eR, a)
      gdistribute (R1 eL) a = Right (eL, a)

instance (ECommutable e1, ECommutable e2) => ECommutable (Either e1 e2) where

instance (GECommutable e1, GECommutable e2) => GECommutable (e1 :*: e2) where
  gecommute handler = go $ gecommute $ wraparound . gecommute . gcurry handler
    where
      go CellExceptReader { .. } = CellExceptReader
        { newFmap = newFmap
        , newCell = hoistCell guncurryReader newCell
        }
      gcurry f e1 e2 = f (e1 :*: e2)
      guncurryReader a = ReaderT $ \(r1 :*: r2) -> ExceptT $ runReaderT (runExceptT $ runReaderT a r1) r2

instance (ECommutable e1, ECommutable e2) => ECommutable (e1, e2) where
\end{code}
