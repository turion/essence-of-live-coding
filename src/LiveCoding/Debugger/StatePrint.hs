{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module LiveCoding.Debugger.StatePrint where

-- base
import Data.Data
import Data.Maybe (fromMaybe, fromJust)
import Data.Proxy
import Data.Typeable
import Unsafe.Coerce

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State

-- syb
import Data.Generics.Aliases
import Data.Generics.Text (gshow)

-- essenceoflivecoding
import LiveCoding.Cell
import LiveCoding.Debugger
import LiveCoding.Forever
import LiveCoding.Exceptions

statePrint :: Debugger' IO
statePrint = Debugger' $ liveCell $ arrM $ const $ do
  s <- get
  lift $ putStrLn $ stateShow s

stateShow :: Data s => s -> String
stateShow
  =       gshow
  `ext2Q` compositionShow
  `ext2Q` foreverEShow
  `ext2Q` feedbackShow
  `ext2Q` parallelShow
  `ext2Q` exceptShow
  `ext2Q` choiceShow

isUnit :: Data s => s -> Bool
isUnit = mkQ False
          (\() -> True)
  `ext2Q` (\(a, b) -> isUnit a && isUnit b)
  `ext2Q` (\(Composition (s1, s2)) -> isUnit s1 && isUnit s2)
  `ext2Q` (\(Parallel (s1, s2)) -> isUnit s1 && isUnit s2)
  `ext2Q` (\(Choice sL sR) -> isUnit sL && isUnit sR)

compositionShow :: (Data s1, Data s2) => Composition s1 s2 -> String
compositionShow (Composition (s1, s2))
  | isUnit s1 = stateShow s2
  | isUnit s2 = stateShow s1
  | otherwise = stateShow s1 ++ " >>> " ++ stateShow s2

-- TODO Would be cooler if this was multiline
parallelShow :: (Data s1, Data s2) => Parallel s1 s2 -> String
parallelShow (Parallel (s1, s2))
  | isUnit s1 = stateShow s2
  | isUnit s2 = stateShow s1
  | otherwise = "(" ++ stateShow s1 ++ " *** " ++ stateShow s2 ++ ")"

foreverEShow :: (Data e, Data s) => ForeverE e s -> String
foreverEShow ForeverE { .. }
  =  "forever("
  ++ (if isUnit lastException then "" else gshow lastException ++ ", ")
  ++ stateShow initState ++ "): " ++ stateShow currentState

feedbackShow :: (Data state, Data s) => Feedback state s -> String
feedbackShow (Feedback (state, s)) = "feedback " ++ gshow s ++ " $ " ++ stateShow state

exceptShow :: (Data s, Data e) => ExceptState s e -> String
exceptShow (NotThrown s) = "NotThrown: " ++ stateShow s ++ "\n"
exceptShow (Exception e)
  =  "Exception"
  ++ (if isUnit e then "" else " " ++ gshow e)
  ++ ":\n"

choiceShow :: (Data stateL, Data stateR) => Choice stateL stateR -> String
choiceShow Choice { .. }
  | isUnit choiceLeft  = "+" ++ stateShow choiceRight ++ "+"
  | isUnit choiceRight = "+" ++ stateShow choiceLeft  ++ "+"
  | otherwise     = "+" ++ stateShow choiceLeft ++ " +++ " ++ stateShow choiceRight ++ "+"

{-
-- TODO  Leave out for now from the examples and open bug when public
liveBindShow :: (Data e, Data s1, Data s2) => LiveBindState e s1 s2 -> String
liveBindShow (NotYetThrown s1 s2) = "[NotYet " ++ stateShow s1 ++ "; " ++ stateShow s2 ++ "]"
liveBindShow (Thrown e s2) = "[Thrown " ++ gshow e ++ ". " ++ stateShow s2 ++ "]"
-}

gcast2 :: forall c t t' a b. (Typeable t, Typeable t')
       => c (t a b) -> Maybe (c (t' a b))
gcast2 x = fmap (\Refl -> x) (eqT :: Maybe (t :~: t'))

gcast3
  :: forall f t t' a b c. (Typeable t, Typeable t')
  => f (t a b c) -> Maybe (f (t' a b c))
gcast3 x = fmap (\Refl -> x) (eqT :: Maybe (t :~: t'))

-- from https://stackoverflow.com/questions/14447050/how-to-define-syb-functions-for-type-extension-for-tertiary-type-constructors-e?rq=1
-- sclv said to just give all the things in the where clause explicit types.
-- I guess one also needs to extend typeOf3' to include all the arguments. (Same for x/typeOf3)
-- Another possibility might be kind-heterogeneous type equality
{-
dataCast3
  :: (Typeable t, Data a)
  => (forall b c d. (Data b, Data c, Data d) => f (t b c d))
  -> Maybe (f a)
dataCast3 x = let proxy = Proxy in dropMaybe proxy $ if typeRep x == typeRep proxy
      then Just $ unsafeCoerce x
      else Nothing
dropMaybe :: Proxy a -> Maybe (f a) -> Maybe (f a)
dropMaybe _ = id
-}

--thing :: (Typeable t) => (forall b c d . (Data b, Data c, Data d) => f (t b c d)) -> TypeRep
--thing = typeRep
{-
dataCast3
  :: (Typeable t, Data a)
  => (forall b c d. (Data b, Data c, Data d) => f (t b c d))
  -> Maybe (f a)
dataCast3 x =   r 
  where
    r = if typeRepFingerprint (typeOf (getArg x)) == typeRepFingerprint (typeOf (getArg (fromJust r)))
       then Just $ unsafeCoerce x
       else Nothing
    getArg :: c x -> x
    getArg = undefined
-}
{-
ext3
  :: (Data a, Typeable t)
  => f a
  -> (forall b c d. (Data b, Data c, Data d) => f (t b c d))
  -> f a
--ext3 def ext = fromMaybe def $ gcast3 ext
--ext3 def ext = fromMaybe def $ gcast3' ext
--ext3 def ext = maybe def id $ dataCast3 ext
-}
ext3
  :: (Data a, Data b, Data c, Data d, Typeable t, Typeable f)
  => f a
  -> f (t b c d)
  -> f a
ext3 def ext = maybe def id $ cast ext

ext3Q
  :: (Data a, Data b, Data c, Data d, Typeable t, Typeable q)
  => (a -> q)
  -> (t b c d -> q)
  -> a -> q
ext3Q def ext = unQ ((Q def) `ext3` (Q ext))


newtype Q q x = Q { unQ :: x -> q }
