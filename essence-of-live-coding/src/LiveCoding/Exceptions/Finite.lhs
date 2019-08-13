\begin{comment}
\begin{code}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module LiveCoding.Exceptions.Finite where

-- base
import Control.Arrow
import GHC.Generics
import Data.Data
import Data.Void

-- transformers
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Exceptions (runReaderC')
-- import LiveCoding.CellExcept
\end{code}
\end{comment}

\begin{code}
class Finite e where
  commute :: Monad m => (e -> Cell m a b) -> Cell (ReaderT e m) a b

  default commute :: (Generic e, GFinite (Rep e), Monad m) => (e -> Cell m a b) -> Cell (ReaderT e m) a b
  commute handler = hoistCell (withReaderT from) $ gcommute $ handler . to

class GFinite f where
  gcommute :: Monad m => (f e -> Cell m a b) -> Cell (ReaderT (f e) m) a b

instance GFinite f => GFinite (M1 a b f) where
  gcommute handler = hoistCell (withReaderT unM1) $ gcommute $ handler . M1

instance Finite e => GFinite (K1 a e) where
  gcommute handler = hoistCell (withReaderT unK1) $ commute $ handler . K1

instance GFinite V1 where
  gcommute _ = error "gcommute: Can't commute with an empty type"

instance Finite Void where
  commute _ = error "Nope"

instance GFinite U1 where
  gcommute handler = liftCell $ handler U1

instance Finite () where

instance Finite Bool where
  commute handler = proc a -> do
    bool <- constM ask -< ()
    if bool
    then liftCell $ handler True  -< a
    else liftCell $ handler False -< a

instance (GFinite eL, GFinite eR) => GFinite (eL :+: eR) where
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

instance (Finite e1, Finite e2) => Finite (Either e1 e2) where

instance (GFinite e1, GFinite e2) => GFinite (e1 :*: e2) where
  gcommute handler = hoistCell guncurryReader $ gcommute $ gcommute . gcurry handler
    where
      gcurry f e1 e2 = f (e1 :*: e2)
      guncurryReader a = ReaderT $ \(r1 :*: r2) -> runReaderT (runReaderT a r1) r2
\end{code}
