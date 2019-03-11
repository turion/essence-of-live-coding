{-# LANGUAGE Arrows #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
module LiveCoding.Bind where

-- base
import Control.Arrow
import Data.Data
import Data.Either (fromRight)
import Data.Void
import GHC.Generics
import Prelude hiding ((>>=), return, (>>))

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

-- essenceoflivecoding
import LiveCoding.Cell

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
  -- commute handler = liftCell $ handler ()

instance Commutable Bool where
  commute handler = proc a -> do
    bool <- constM ask -< ()
    if bool
    then liftCell $ handler True  -< a
    else liftCell $ handler False -< a

instance (GCommutable eL, GCommutable eR) => GCommutable (eL :+: eR) where
  gcommute handler
    = let
          cellLeft  = runReaderC $ gcommute $ handler . L1
          cellRight = runReaderC $ gcommute $ handler . R1
          gdistribute (L1 eR) a = Left  (eR, a)
          gdistribute (R1 eL) a = Right (eL, a)
    in
      proc a -> do
        either12 <- constM ask -< ()
        liftCell (cellLeft ||| cellRight) -< gdistribute either12 a

runReaderC :: Cell (ReaderT r m) a b -> Cell m (r, a) b
runReaderC Cell { .. } = Cell
  { cellStep = \state (r, a) -> runReaderT (cellStep state a) r
  , ..
  }

instance (Commutable e1, Commutable e2) => Commutable (Either e1 e2) where
{-
  commute handler
    = let
          cellLeft = runReaderC $ commute $ handler . Left
          cellRight = runReaderC $ commute $ handler . Right
    in
      proc a -> do
        either12 <- constM ask -< ()
        liftCell (cellLeft ||| cellRight) -< distribute either12 a
-}
{-
distribute :: Either b c -> a -> Either (b, a) (c, a)
distribute (Left b) a = Left (b, a)
distribute (Right c) a = Right (c, a)
-}

instance (GCommutable e1, GCommutable e2) => GCommutable (e1 :*: e2) where
  gcommute handler = hoistCell guncurryReader $ gcommute $ gcommute . gcurry handler
    where
      gcurry f e1 e2 = f (e1 :*: e2)
      guncurryReader a = ReaderT $ \(r1 :*: r2) -> runReaderT (runReaderT a r1) r2

instance (Commutable e1, Commutable e2) => Commutable (e1, e2) where
  --commute handler = hoistCell uncurryReader $ commute $ commute . curry handler

curryReader :: ReaderT (r1, r2) m a -> ReaderT r1 (ReaderT r2 m) a
curryReader a = ReaderT $ \r1 -> ReaderT $ \r2 -> runReaderT a (r1, r2)

uncurryReader :: ReaderT r1 (ReaderT r2 m) a -> ReaderT (r1, r2) m a
uncurryReader a = ReaderT $ \(r1, r2) -> runReaderT (runReaderT a r1) r2

throwC :: Monad m => Cell (ExceptT e m) e arbitrary
throwC = arrM throwE

throwIf :: Monad m => (a -> Bool) -> e -> Cell (ExceptT e m) a a
throwIf condition e = proc a -> do
  if condition a
  then throwC  -< e
  else returnA -< a

throwIf_ :: Monad m => (a -> Bool) -> Cell (ExceptT () m) a a
throwIf_ condition = throwIf condition ()

return :: Monad m => e -> CellExcept m a b e
return = pure

(>>=)
  :: (Monad m, Commutable e1, Data e2, Commutable e2)
  => CellExcept m a b e1
  -> (e1 -> CellExcept m a b e2)
  -> CellExcept m a b e2
CellExcept fmap1 cell1 >>= handler = CellExcept { .. }
  where
    fmapExcept = id
    cellExcept = cell1 >>>= commute (runCellExcept . handler . fmap1)

(>>)
  :: (Monad m, Commutable e1, Data e2, Commutable e2)
  => CellExcept m a b e1 -> CellExcept m a b e2 -> CellExcept m a b e2
cellExcept1 >> cellExcept2 = cellExcept1 >>= const cellExcept2

data CellExcept m a b e = forall e' . (Data e', Commutable e') => CellExcept
  { fmapExcept :: e' -> e
  , cellExcept :: Cell (ExceptT e' m) a b
  }

instance Functor (CellExcept m a b) where
  fmap f CellExcept { .. } = CellExcept { fmapExcept = f . fmapExcept, .. }

runCellExcept :: Monad m => CellExcept m a b e -> Cell (ExceptT e m) a b
runCellExcept CellExcept { .. } = hoistCell (withExceptT fmapExcept) cellExcept

try :: (Data e, Commutable e) => Cell (ExceptT e m) a b -> CellExcept m a b e
try = CellExcept id

safely :: Monad m => CellExcept m a b Void -> Cell m a b
safely = hoistCell discardVoid . runCellExcept

discardVoid :: Functor m => ExceptT Void m a -> m a
discardVoid = fmap (fromRight (error "safely: Received Left")) . runExceptT

safe :: Monad m => Cell m a b -> CellExcept m a b void
safe cell = CellExcept
  { fmapExcept = absurd
  , cellExcept = liftCell cell
  }

instance Monad m => Applicative (CellExcept m a b) where
  pure e = CellExcept
    { fmapExcept = const e
    , cellExcept = constM $ throwE ()
    }

  CellExcept fmap1 cell1 <*> CellExcept fmap2 cell2 = CellExcept { .. }
    where
      fmapExcept (e1, e2) = fmap1 e1 $ fmap2 e2
      cellExcept = cell1 `andThen` cell2

sumFrom :: Monad m => Integer -> Cell m Integer Integer
sumFrom n0 = feedback n0 $ proc (n, acc) -> returnA -< (acc, acc + n)

count :: Monad m => Cell m a Integer
count = arr (const 1) >>> sumFrom 0
countUpTo     n = arr (const   1 ) >>> sumFrom 0 >>> throwIf_ (>= n)
countDownFrom n = arr (const (-1)) >>> sumFrom n >>> throwIf_ (<= 0)

throwWhenReaches
  :: Monad m
  => Integer -> Cell (ExceptT () m) Integer Integer
throwWhenReaches amplitude = proc n -> if n == amplitude then constM (throwE ()) -< () else returnA -< n

saw1 :: Monad m => Integer -> Cell m () Integer
saw1 amplitude = foreverC $ runCellExcept $ do
  try $ countUpTo     amplitude
  try $ countDownFrom amplitude

saw2 :: Monad m => Integer -> Cell m () Integer
saw2 amplitude = foreverC $ runCellExcept $ do
  try $ countUpTo amplitude
  try $ countUpTo amplitude

myMapM_ f (a : as) = f a *> myMapM_ f as
myMapM_ _ [] = return ()

listThing :: Monad m => Cell m a Integer
listThing = safely $ myMapM_ (try . countUpTo) [3,5,6] *> safe count

ifThenElse True a _ = a
ifThenElse False _ a = a
