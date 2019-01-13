{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
module LiveCoding where

-- base
import Control.Concurrent
import Control.Monad
import Data.Data
import Data.IORef
import Data.Maybe

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

-- dunai-live
import Control.Monad.Trans.MSF.Except (saw, safely)
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore

-- syb
import Data.Generics.Aliases
import qualified Data.Generics.Aliases as Aliases
import Data.Generics.Twins

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

updateWith :: (Data a, Data b) => a -> b -> a
updateWith a b
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
updateWith a b = fromMaybe a $ cast b

getChildrenSetters :: Data a => a -> [GenericT']
getChildrenSetters = gmapQ $ \child -> Aliases.GT $ flip updateWith child

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

type LiveProg = MSF IO () ()

launch :: LiveProg  -> IO (MVar LiveProg)
launch msf = do
  var <- newMVar $ toInit msf
  forkIO $ background var
  return var

background :: MVar LiveProg -> IO ()
background var = forever $ do
  msf <- takeMVar var
  ((), msf') <- unMSF msf ()
  combine var msf'

combine :: MVar LiveProg -> LiveProg -> IO ()
combine var prog = do
  success <- tryPutMVar var prog
  unless success $ do
    newProg <- takeMVar var
    combine var $ combineLiveProg prog newProg

combineLiveProg :: LiveProg -> LiveProg -> LiveProg
combineLiveProg (Init oldState oldFun) (Init newState newFun) = Init (newState `updateWith` oldState) newFun

update :: MVar LiveProg -> LiveProg -> IO ()
update var msf = void $ forkIO $ putMVar var msf

example :: Integer -> LiveProg
example n = saw n >>> arrM print >>> constM (threadDelay 1000)
