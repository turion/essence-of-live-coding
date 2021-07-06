module LiveCoding.Cell.Util.Internal where

-- | Helper for 'onChange'.
whenDifferent :: (Eq p, Monad m) => (p -> p -> a -> m b) -> (p, p, a) -> m (Maybe b)
whenDifferent action (pOld, pNew, a)
  | pOld == pNew = Just <$> action pOld pNew a
  | otherwise    = return Nothing
