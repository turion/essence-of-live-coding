{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

module LiveCoding.Cell.Async where

-- base
-- base
import Control.Arrow (first, (***))
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

headT :: Tuple (a ': types) -> a
headT (a ::: _) = a

data Session
  = Nu (Session -> Session)
  | Compose (Session -> Session) (Session -> Session) Session
  | Id Session
  | GiveAndGet [Type] [Type] Session
  | InternalChoice (Session -> Session) (Session -> Session) Session
  | ExternalChoice (Session -> Session) (Session -> Session) Session
  | Lambda Session Session Session
  | ExternalChoice2 Session Session

type Give a = GiveAndGet '[] '[a]
type Get a = GiveAndGet '[a] '[]
type Act = GiveAndGet '[] '[]
type Id2 = forall x . Lambda x x
type family Id3 (session :: Session) :: Session where
  Id3 session = session

data Prog m (session :: Session) where
  Loop :: Prog m (f (Nu f)) -> Prog m (Nu f)
  Two :: Prog m (f (g session)) -> Prog m (Compose f g session)
  -- TODO Not sure I really need an m for this
  Yep :: m (Prog m session) -> Prog m (Id session)
  -- Thanks to SPJ for the idea of having Data s _and_ CPS
  Step :: Data s => s -> (s -> Tuple inTypes -> m (Tuple outTypes, Prog m session)) -> Prog m (GiveAndGet inTypes outTypes session)
  ILeft :: Prog m (f session) -> Prog m (InternalChoice f g session)
  IRight :: Prog m (g session) -> Prog m (InternalChoice f g session)
  Both :: Prog m (f session) -> Prog m (g session) -> Prog m (ExternalChoice f g session)
  Abstract :: Prog m (f session) -> Prog m (Lambda var (f var) session)
  Both2 :: Prog m session1 -> Prog m session2 -> Prog m (ExternalChoice2 session1 session2)

test :: Monad m => a -> Prog m (Nu (Lambda x (Give a x)))
test a = Loop $ Abstract $ Step () $ \() _ -> return (a ::: TNil, test a)

-- isoEC :: Prog m (Nu (ExternalChoice (Give a) (Give b))) -> Prog m (Nu (Lambda x (ExternalChoice2 (Give a x) (Give b x))))
-- isoEC (Loop (Both pr' pr2)) = Loop $ Abstract $ Both2 _ _wf

-- isoC :: Prog m (Nu (Compose f g)) -> Prog m (Nu (Lambda x (f (g x))))
-- isoC (Loop prog) = Loop $ Abstract _

unThere :: Functor m => Prog m (Give a session) -> m (a, Prog m session)
unThere (Step s f) = first (\(a ::: TNil) -> a) <$> f s TNil

unHere :: Functor m => Prog m (Get a session) -> a -> m (Prog m session)
unHere (Step s f) a = fmap snd $ f s $ a ::: TNil

unLoop :: Prog m (Nu f) -> Prog m (f (Nu f))
unLoop (Loop prog) = prog

unTwo :: Prog m (Compose f g session) -> Prog m (f (g session))
unTwo (Two prog) = prog

unYep :: Prog m (Id session) -> m (Prog m session)
unYep (Yep prog) = prog

always :: Monad m => a -> Prog m (Nu (Give a))
always a = Loop $ Step () $ const $ const $ return (a ::: TNil, always a)

-- FIXME These are all missing the M constructor

chooseInternally :: Prog m (InternalChoice f g session) -> Either (Prog m (f session)) (Prog m (g session))
chooseInternally (ILeft prog) = Left prog
chooseInternally (IRight prog) = Right prog

chooseExternallyLeft :: Prog m (ExternalChoice f g session) -> Prog m (f session)
chooseExternallyLeft (Both left _) = left

chooseExternallyRight :: Prog m (ExternalChoice f g session) -> Prog m (g session)
chooseExternallyRight (Both _ right) = right

ht :: Monad m => Prog m (Nu (Compose (Give a) (Get a))) -> Prog m (Nu Id)
ht prog = Loop $ Yep $ do
  let prog' = unLoop prog
  let prog'' = unTwo prog'
  (a, prog''') <- unThere prog''
  prog'''' <- unHere prog''' a
  return $ ht prog''''

underGive :: Functor m => (Prog m session1 -> Prog m session2) -> Prog m (Give a session1) -> Prog m (Give a session2)
underGive morph (Step s f) = Step s $ fmap (fmap (fmap (fmap morph))) f

-- Or newtype and make it a functor
mapGive :: Functor m => (a -> b) -> Prog m (Give a session) -> Prog m (Give b session)
mapGive g (Step s f) = Step s $ fmap (fmap (fmap (first $ \(a ::: TNil) -> g a ::: TNil))) f

-- TODO same for Get

underF :: Functor m => (Prog m session1 -> Prog m session2) -> Prog m (f session1) -> Prog m (f session2)
underF morph (Step s f) = Step s $ fmap (fmap (fmap (fmap morph))) f
-- Loop is not possible currently
underF morph (Two prog) = Two $ underF (underF morph) prog
underF morph (Yep prog) = Yep $ fmap morph prog
underF morph (ILeft prog) = ILeft $ underF morph prog
underF morph (IRight prog) = IRight $ underF morph prog
underF morph (Both progL progR) = Both (underF morph progL) (underF morph progR)
underF morph (Abstract prog) = Abstract $ underF morph prog

mapNu :: Functor m => (forall session . Prog m (f session) -> Prog m (g session)) -> Prog m (Nu f) -> Prog m (Nu g)
mapNu morph (Loop prog) = Loop $ underF (mapNu morph) $ morph prog

transposeNu :: Functor m => Prog m (Nu (Compose f g)) -> Prog m (f (Nu (Compose g f)))
transposeNu prog = underF (Loop . Two . underF transposeNu) $ unTwo $ unLoop prog

-- TODO isomorphisms

-- This is slightly different because Pipes don't do an m necessarily on every input & output.
-- Also they can do arbitrary Ms whenever they want, so maybe I should add a further internal choice
type Pipes a' a b' b m r = Prog (ExceptT r m) (Nu (InternalChoice (Compose (Give a') (Get a)) (Compose (Give b) (Get b'))))

type MSF m a b = Prog m (Nu (GiveAndGet '[a] '[b]))

-- TODO this doesn't work as long as Cells aren't SPJ-style
-- msfToCell :: Functor m => MSF m a b -> Cell m a b
-- msfToCell (Loop (Step cellState f)) = _

cellToMSF :: Functor m => Cell m a b -> MSF m a b
cellToMSF Cell { .. } = go cellState
  where
    go s = Loop $ Step s $ fmap ((. headT) . fmap (fmap ((::: TNil) *** go))) cellStep
cellToMSF ArrM { .. } = go
  where
    go = Loop $ Step () $ const $ fmap (fmap (\b -> (b ::: TNil, go))) $ runArrM . headT

type ConduitPipe l i o u m r = Prog (ExceptT r m) (Nu (InternalChoice (InternalChoice (Give o) (ExternalChoice (Get i) (Get u))) (Give l)))
