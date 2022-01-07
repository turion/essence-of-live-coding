{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module LiveCoding.Cell.Async where

-- base
import Control.Arrow (first)
import Data.Data
import Data.Kind (Type)
import Data.Proxy

-- transformers
import Control.Monad.Trans.Except

-- essence-of-live-coding
import LiveCoding.Cell

data Tuple (types :: [Type]) where
  TNil :: Tuple '[]
  (:::) :: a -> Tuple types -> Tuple (a ': types)

data BlaSession
  = Give Type BlaSession
  | Get Type BlaSession
  | Nu (BlaSession -> BlaSession)
  | Compose (BlaSession -> BlaSession) (BlaSession -> BlaSession) BlaSession
  | Id BlaSession
  | GiveAndGet [Type] [Type] BlaSession
  | InternalChoice (BlaSession -> BlaSession) (BlaSession -> BlaSession) BlaSession
  | ExternalChoice (BlaSession -> BlaSession) (BlaSession -> BlaSession) BlaSession

data Prog m (session :: BlaSession) where
  There :: Data s => s -> (s -> m (a, Prog m session)) -> Prog m (Give a session)
  Here :: Data s => s -> (s -> a -> m (Prog m session)) -> Prog m (Get a session)
  Loop :: m (Prog m (f (Nu f))) -> Prog m (Nu f)
  -- TODO Not sure I really need an m for these
  Two :: m (Prog m (f (g session))) -> Prog m (Compose f g session)
  Yep :: m (Prog m session) -> Prog m (Id session)
  -- TODO Maybe use this to replace the m's in the other constructors?
  M :: m (Prog m session) -> Prog m session
  Step :: Data s => s -> (s -> Tuple inTypes -> m (Tuple outTypes, Prog m session)) -> Prog m (GiveAndGet inTypes outTypes session)
  ILeft :: Prog m (f session) -> Prog m (InternalChoice f g session)
  IRight :: Prog m (g session) -> Prog m (InternalChoice f g session)
  Both :: Prog m (f session) -> Prog m (g session) -> Prog m (ExternalChoice f g session)

unThere :: Prog m (Give a session) -> m (a, Prog m session)
unThere (There s f) = f s

unHere :: Prog m (Get a session) -> a -> m (Prog m session)
unHere (Here s f) a = f s a

unLoop :: Prog m (Nu f) -> m (Prog m (f (Nu f)))
unLoop (Loop prog) = prog

unTwo :: Prog m (Compose f g session) -> m (Prog m (f (g session)))
unTwo (Two prog) = prog

unYep :: Prog m (Id session) -> m (Prog m session)
unYep (Yep prog) = prog

always :: Monad m => a -> Prog m (Nu (Give a))
always a = Loop $ return $ There () $ const $ return (a, always a)

-- FIXME These are all missing the M constructor

chooseInternally :: Prog m (InternalChoice f g session) -> Either (Prog m (f session)) (Prog m (g session))
chooseInternally (ILeft prog) = Left prog
chooseInternally (IRight prog) = Right prog

chooseExternallyLeft :: Prog m (ExternalChoice f g session) -> Prog m (f session)
chooseExternallyLeft (Both left _) = left

chooseExternallyRight :: Prog m (ExternalChoice f g session) -> Prog m (g session)
chooseExternallyRight (Both _ right) = right

ht :: Monad m => Prog m (Nu (Compose (Give a) (Get a))) -> Prog m (Nu Id)
ht prog = Loop $ do
  prog' <- unLoop prog
  prog'' <- unTwo prog'
  (a, prog''') <- unThere prog''
  prog'''' <- unHere prog''' a
  return $ Yep $ return $ ht prog''''

underGive :: Functor m => (Prog m session1 -> Prog m session2) -> Prog m (Give a session1) -> Prog m (Give a session2)
underGive morph (There s f) = There s $ fmap (fmap (fmap morph)) f

-- Or newtype and make it a functor
mapGive :: Functor m => (a -> b) -> Prog m (Give a session) -> Prog m (Give b session)
mapGive g (There s f) = There s $ fmap (fmap $ first g) f

-- TODO same for Get

underF :: Functor m => (Prog m session1 -> Prog m session2) -> Prog m (f session1) -> Prog m (f session2)
underF morph (There s f) = There s $ fmap (fmap (fmap morph)) f
underF morph (Here s f) = Here s $ fmap (fmap (fmap morph)) f
-- Loop is not possible currently
underF morph (Two prog) = Two $ fmap (underF (underF morph)) prog
underF morph (Yep prog) = Yep $ fmap morph prog
underF morph (M prog) = M $ fmap (underF morph) prog

mapNu :: Functor m => (forall session . Prog m (f session) -> Prog m (g session)) -> Prog m (Nu f) -> Prog m (Nu g)
mapNu morph prog = Loop $ fmap (underF (mapNu morph) . morph) $ unLoop prog

-- FIXME If I'm clever enough I can do this just with Functor I think
transposeNu :: Monad m => Prog m (Nu (Compose f g)) -> Prog m (f (Nu (Compose g f)))
transposeNu prog = M $ do
  prog' <- unLoop prog
  prog'' <- unTwo prog'
  return $ underF (Loop . return . Two . return . underF transposeNu) prog''

-- TODO isomorphisms

-- This is slightly different because Pipes don't do an m necessarily on every input & output.
-- Also they can do arbitrary Ms whenever they want, so maybe I should add a further internal choice
type Pipes a' a b' b m r = Prog (ExceptT r m) (Nu (InternalChoice (Compose (Give a') (Get a)) (Compose (Give b) (Get b'))))

type MSF m a b = Prog m (Nu (GiveAndGet '[a] '[b]))

msfToCell :: MSF m a b -> Cell m a b
msfToCell prog = _

type ConduitPipe l i o u m r = Prog (ExceptT r m) (Nu (InternalChoice (InternalChoice (Give o) (ExternalChoice (Get i) (Get u))) (Give l)))
