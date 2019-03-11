{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
module LiveCoding where

-- base
import Control.Arrow
import Control.Concurrent
import Control.Monad
import Data.Data
import Data.IORef
import Data.Maybe
import Data.Typeable
import Data.Typeable.Internal

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

-- syb
import Data.Generics.Aliases
import qualified Data.Generics.Aliases as Aliases
import Data.Generics.Text (gshow)
import Data.Generics.Twins

-- essenceoflivecoding
import LiveCoding.Cell
import LiveCoding.Bind hiding ((>>=), return)

-- class Data a => Update a where

data DataPointer
  = Here
  | Constructor String DataPointer

-- Unsure about order here. Think about this:
-- * What's the intended bind?
-- * How does `local` interact with everything else?
-- * What should happen when the `Maybe` layer yields `Nothing`?
-- Possibly just use `Maybe` inside as data.
type Inspect a b = StateT DataPointer (Reader a) b

at :: (Data a, Data b) => a -> DataPointer -> Maybe b
a `at` Here = cast a
a `at` Constructor constructorName pointer = Nothing

this :: (Data a, Data b) => Inspect a (Maybe b)
this = do
  pointer <- get
  a <- lift ask
  return $ a `at` pointer

-- set :: (Data a, Data b) => b -> Inspect a (Maybe a)

runInspect :: (Data a, Data b) => Inspect a b -> a -> b
runInspect inspect = runReader $ evalStateT inspect Here

migrate :: (Data a, Data b) => a -> b -> a
migrate a b
  |  isAlgType (dataTypeOf a)
  && isAlgType (dataTypeOf b)
  && show (dataTypeOf a) == show (dataTypeOf b)
  && showConstr (toConstr a) == showConstr (toConstr b)
  = setChildren (getChildrenSetters b) a
-- TODO Add special cases for:
-- * Exception handling! Make my own Either type
-- * a -> (a, b) and back (for feedback)
-- * a -> Maybe a and back
-- * a >>> b to a or b and back
migrate a b = fromMaybe a $ cast b

getChildrenSetters :: Data a => a -> [GenericT']
getChildrenSetters = gmapQ $ \child -> Aliases.GT $ flip migrate child

setChildren :: Data a => [GenericT'] -> a -> a
setChildren updates a = snd $ gmapAccumT f updates a
  where
    f [] e = ([], e)
    f (update : updates) e = (updates, unGT update $ e)

data Update where
  Update :: Data x => (x -> x) -> Update

data Foo = Foo Integer
  deriving (Show, Typeable, Data)

data Bar = Bar Integer Integer String
  deriving (Show, Typeable, Data)

type LiveProg = Cell IO () ()
type Debugger = forall s . Data s => s -> IO ()

launch :: LiveProg  -> IO (MVar LiveProg)
launch cell = do
  var <- newMVar cell
  forkIO $ background var
  return var

debug :: Debugger -> LiveProg -> IO ()
debug debugger (Cell state _) = debugger state

backgroundWithDebugger :: MVar LiveProg -> Debugger -> IO ()
backgroundWithDebugger var debugger = forever $ do
  cell <- takeMVar var
  ((), cell') <- step cell ()
  debug debugger cell'
  combine var cell'

background :: MVar LiveProg -> IO ()
background var = backgroundWithDebugger var $ stateShow >>> putStrLn

stateShow :: Data s => s -> String
stateShow = gshow `ext2Q` compositionShow `ext2Q` foreverEShow `ext2Q` feedbackShow
  where
    myExtQ :: (Data a, Data b) => (a -> String) -> (b -> String) -> a -> String
    myExtQ = extQ

    compositionShow :: (Data s1, Data s2) => Composition s1 s2 -> String
    compositionShow (Composition s1 s2) = stateShow s1 ++ " >>> " ++ stateShow s2

    foreverEShow :: (Data e, Data s) => ForeverE e s -> String
    foreverEShow ForeverE { .. } = "forever(" ++ gshow lastException ++ ", " ++ stateShow initState ++ "): " ++ stateShow currentState

    feedbackShow :: (Data state, Data s) => Feedback state s -> String
    feedbackShow (Feedback state s) = "feedback " ++ gshow s ++ " $ " ++ stateShow state

    exceptStateShow :: (Data e, Data s1, Data s2) => ExceptState e s1 s2 -> String
    exceptStateShow (NotYetThrown s1 s2) = "[NotYet " ++ stateShow s1 ++ "; " ++ stateShow s2 ++ "]"
    exceptStateShow (Thrown e s2) = "[Thrown " ++ gshow e ++ ". " ++ stateShow s2 ++ "]"

gcast2 :: forall c t t' a b. (Typeable t, Typeable t')
       => c (t a b) -> Maybe (c (t' a b))
gcast2 x = fmap (\Refl -> x) (eqT :: Maybe (t :~: t'))

{-
gcast3
  :: forall f t t' a b c. (Typeable t, Typeable t')
  => f (t a b c) -> Maybe (f (t' a b c))
gcast3 x = fmap (\Refl -> x) (eqT :: Maybe (t :~: t'))

dataCast3
  :: Typeable t
  => (forall b c d. (Data b, Data c, Data d) => f (t b c d))
  -> Maybe (f a)
dataCast3 x = gcast3 x
-}

-- from https://stackoverflow.com/questions/14447050/how-to-define-syb-functions-for-type-extension-for-tertiary-type-constructors-e?rq=1
gcast3' :: (Typeable t, Data a) => c (t f g h) -> Maybe (c a)
gcast3' x = r
 where
  r = if typeOf3 (getArg x) == typeOf3' (getArg (fromJust r))
       then Just $ unsafeCoerce x
       else Nothing
  getArg :: c x -> x
  getArg = undefined
  typeOf3' z = mkTyConApp (typeRepTyCon (typeOf z)) []
ext3
  :: (Data a, Typeable t)
  => f a
  -> (forall b c d. (Data b, Data c, Data d) => f (t b c d))
  -> f a
--ext3 def ext = fromMaybe def $ gcast3 ext
ext3 def ext = fromMaybe def $ gcast3' ext
--ext3 def ext = maybe def id $ dataCast3 ext

combine :: MVar LiveProg -> LiveProg -> IO ()
combine var prog = do
  success <- tryPutMVar var prog
  unless success $ do
    newProg <- takeMVar var
    combine var $ combineLiveProg prog newProg

combineLiveProg :: LiveProg -> LiveProg -> LiveProg
combineLiveProg (Cell oldState oldStep) (Cell newState newStep) = Cell (newState `migrate` oldState) newStep

update :: MVar LiveProg -> LiveProg -> IO ()
update var cell = void $ forkIO $ putMVar var cell

example1 :: Integer -> LiveProg
example1 n = saw1 n >>> arrM print >>> constM (threadDelay 500000)

example2 :: Integer -> LiveProg
example2 n = saw2 n >>> arrM print >>> constM (threadDelay 500000)


example :: LiveProg
example = listThing >>> arrM print >>> constM (threadDelay 500000)
