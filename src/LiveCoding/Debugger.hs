{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module LiveCoding.Debugger where

-- base
import Data.Data
import Data.Typeable

-- essenceoflivecoding
import LiveCoding.Cell
import LiveCoding.Exceptions
import LiveCoding.Forever

-- syb
import Data.Generics.Aliases
import Data.Generics.Text (gshow)

stateShow :: Data s => s -> String
stateShow = gshow `ext2Q` compositionShow `ext2Q` foreverEShow `ext2Q` feedbackShow `ext2Q` parallelShow
  where
    myExtQ :: (Data a, Data b) => (a -> String) -> (b -> String) -> a -> String
    myExtQ = extQ

    compositionShow :: (Data s1, Data s2) => Composition s1 s2 -> String
    compositionShow (Composition (s1, s2)) = stateShow s1 ++ " >>> " ++ stateShow s2

    parallelShow :: (Data s1, Data s2) => Parallel s1 s2 -> String
    parallelShow (Parallel (s1, s2)) = stateShow s1 ++ " >>> " ++ stateShow s2

    -- TODO Where is parallelShow?
    foreverEShow :: (Data e, Data s) => ForeverE e s -> String
    foreverEShow ForeverE { .. } = "forever(" ++ gshow lastException ++ ", " ++ stateShow initState ++ "): " ++ stateShow currentState

    feedbackShow :: (Data state, Data s) => Feedback state s -> String
    feedbackShow (Feedback (state, s)) = "feedback " ++ gshow s ++ " $ " ++ stateShow state

    liveBindShow :: (Data e, Data s1, Data s2) => LiveBindState e s1 s2 -> String
    liveBindShow (NotYetThrown s1 s2) = "[NotYet " ++ stateShow s1 ++ "; " ++ stateShow s2 ++ "]"
    liveBindShow (Thrown e s2) = "[Thrown " ++ gshow e ++ ". " ++ stateShow s2 ++ "]"

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
-}

