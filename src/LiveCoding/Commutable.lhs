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
data CellExcept m a b e where
  Return :: e -> CellExcept m a b e
  Bind
    :: CellExcept m a b e1
    -> (e1 -> CellExcept m a b e2)
    -> CellExcept m a b e2
  Try
    :: (Data e, Commutable e)
    => Cell (ExceptT e m) a b
    -> CellExcept m a b e

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
\end{code}
