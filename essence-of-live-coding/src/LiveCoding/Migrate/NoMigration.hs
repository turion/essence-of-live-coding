{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : LiveCoding.Migrate.NoMigration
Description : Mechanism to save state in a Cell without requiring a Data instance.

If a data type is wrapped in 'NoMigration' then it can be used as the state of a 'Cell'
without requiring it to have a 'Data' instance. The consequence is that if the type has changed
in between a livereload, then the previous saved value will be discarded, and no migration attempt
will happen.

'LiveCoding' does not export 'delay' and 'changes' from this module. These functions should be
used with a qualified import.
-}
module LiveCoding.Migrate.NoMigration where

-- base

import Control.Arrow (Arrow (arr, second), returnA, (>>>))
import Control.Monad (guard)
import Data.Data (
  Constr,
  Data (dataTypeOf, gunfold, toConstr),
  DataType,
  Fixity (Prefix),
  Typeable,
  mkConstr,
  mkDataType,
 )

-- essence-of-live-coding
import LiveCoding.Cell
import qualified LiveCoding.Cell.Feedback as Feedback
import LiveCoding.Cell.Monad (hoistCellKleisli)

-- * 'NoMigration' data type and 'Data' instance.

{- | Isomorphic to @'Maybe' a@ but has a different 'Data' instance. The 'Data' instance for @'NoMigration' a@ doesn't require a 'Data' instance for @a@.

 If a data type is wrapped in 'NoMigration' then it can be used as the state of a 'Cell'
 without requiring it to have a 'Data' instance. The consequence is that if the type has changed
 in between a livereload, then the previous saved value will be discarded, and no migration attempt
 will happen.
-}
data NoMigration a = Initialized a | Uninitialized
  deriving (Show, Eq, Functor, Foldable, Traversable)

fromNoMigration :: a -> NoMigration a -> a
fromNoMigration _ (Initialized a) = a
fromNoMigration a Uninitialized = a

dataTypeNoMigration :: DataType
dataTypeNoMigration = mkDataType "NoMigration" [initializedConstr, uninitializedConstr]

initializedConstr :: Constr
initializedConstr = mkConstr dataTypeNoMigration "Initialized" [] Prefix

uninitializedConstr :: Constr
uninitializedConstr = mkConstr dataTypeNoMigration "Uninitialized" [] Prefix

-- |The Data instance for @'NoMigration' a@ doesn't require a 'Data' instance for @a@.
instance (Typeable a) => Data (NoMigration a) where
  dataTypeOf _ = dataTypeNoMigration
  toConstr (Initialized _) = initializedConstr
  toConstr Uninitialized = uninitializedConstr
  gunfold _cons nil _ = nil Uninitialized

-- * Utility functions which internally use 'NoMigration'.

{- | Like 'Feedback.delay', but doesn't require 'Data' instance, and only migrates the
 last value if it still has the same type.
-}
delay :: (Monad m, Typeable a) => a -> Cell m a a
delay a = arr Initialized >>> Feedback.delay Uninitialized >>> arr (fromNoMigration a)

{- | Like 'Utils.changes', but doesn't require Data instance, and only migrates the last
 value if it still is of the same type.
-}
changes :: (Typeable a, Eq a, Monad m) => Cell m a (Maybe a)
changes = proc a -> do
  aLast <- delay Nothing -< Just a
  returnA
    -< do
      aLast' <- aLast
      guard $ a /= aLast'
      return a

{- | Caching version of 'arrM'.

   Only runs the computation in @m@ when the input value
   changes. Meanwhile it keeps outputing the last outputted value. Also runs the computation
   on the first tick. Does not require 'Data' instance. On `:livereload` will run action again on
   first tick.
-}
arrChangesM :: (Monad m, Typeable a, Typeable b, Eq a) => (a -> m b) -> Cell m a b
arrChangesM f = Cell {cellState = Uninitialized, ..}
  where
    cellStep Uninitialized a = h a
    cellStep (Initialized (a', b)) a =
      if a == a'
        then return (b, Initialized (a, b))
        else h a
    h a = (\b' -> (b', Initialized (a, b'))) <$> f a

cellNoMigration :: (Typeable s, Functor m) => s -> (s -> a -> m (b, s)) -> Cell m a b
cellNoMigration state step = Cell {cellState = Uninitialized, ..}
  where
    cellStep Uninitialized a = second Initialized <$> step state a
    cellStep (Initialized s) a = second Initialized <$> step s a
