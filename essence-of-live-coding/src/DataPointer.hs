module DataPointer where

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

