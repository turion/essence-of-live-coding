{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module LiveCoding.Migrate.Cell where

-- base
import Data.Data

-- syb
import Data.Generics.Aliases

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Exceptions
import LiveCoding.Migrate.Migration

-- * Migrations involving sequential compositions of cells

maybeMigrateToComposition1
  :: (Typeable state1', Typeable state1)
  => Composition state1 state2
  -> state1'
  -> Maybe (Composition state1 state2)
maybeMigrateToComposition1 Composition { state2 } state1' = do
  state1 <- cast state1'
  return $ Composition { .. }

-- | Migrate @cell1@ to @cell1 >>> cell2@.
migrationToComposition1 :: Migration
migrationToComposition1 = migrationTo2 maybeMigrateToComposition1

maybeMigrateFromComposition1
  :: (Typeable state1', Typeable state1)
  => Composition state1 state2
  -> Maybe       state1'
maybeMigrateFromComposition1 Composition { state1 } = cast state1

-- | Migrate to @cell1@ from @cell1 >>> cell2@.
migrationFromComposition1 :: Migration
migrationFromComposition1 = constMigrationFrom2 maybeMigrateFromComposition1

maybeMigrateToComposition2
  :: (Typeable state2', Typeable state2)
  => Composition state1 state2
  -> state2'
  -> Maybe (Composition state1 state2)
maybeMigrateToComposition2 Composition { state1 } state2' = do
  state2 <- cast state2'
  return $ Composition { .. }

-- | Migrate @cell2@ to @cell1 >>> cell2@.
migrationToComposition2 :: Migration
migrationToComposition2 = migrationTo2 maybeMigrateToComposition2

maybeMigrateFromComposition2
  :: (Typeable state2', Typeable state2)
  => Composition state1 state2
  -> Maybe              state2'
maybeMigrateFromComposition2 Composition { state2 } = cast state2

-- | Migrate to @cell2@ from @cell1 >>> cell2@.
migrationFromComposition2 :: Migration
migrationFromComposition2 = constMigrationFrom2 maybeMigrateFromComposition2

-- | Combines all migrations related to composition, favouring the first argument.
migrationComposition :: Migration
migrationComposition
  =  migrationToComposition1
  <> migrationFromComposition1
  <> migrationToComposition2
  <> migrationFromComposition2

-- * Migrations involving parallel compositions of cells

maybeMigrateToParallel1
  :: (Typeable stateP1', Typeable stateP1)
  => Parallel stateP1 stateP2
  -> stateP1'
  -> Maybe (Parallel stateP1 stateP2)
maybeMigrateToParallel1 (Parallel { stateP2 }) stateP1' = do
  stateP1 <- cast stateP1'
  return $ Parallel { .. }

-- | Migrate @cell1@ to @cell1 *** cell2@.
migrationToParallel1 :: Migration
migrationToParallel1 = migrationTo2 maybeMigrateToParallel1

maybeMigrateFromParallel1
  :: (Typeable stateP1', Typeable stateP1)
  => Parallel stateP1 stateP2
  -> Maybe    stateP1'
maybeMigrateFromParallel1 (Parallel { stateP1 }) = cast stateP1

-- | Migrate to @cell1@ from @cell1 *** cell2@.
migrationFromParallel1 :: Migration
migrationFromParallel1 = constMigrationFrom2 maybeMigrateFromParallel1

maybeMigrateToParallel2
  :: (Typeable stateP2', Typeable stateP2)
  => Parallel stateP1 stateP2
  -> stateP2'
  -> Maybe (Parallel stateP1 stateP2)
maybeMigrateToParallel2 (Parallel { stateP1 }) stateP2' = do
  stateP2 <- cast stateP2'
  return $ Parallel { .. }

-- | Migrate @cell2@ to @cell1 *** cell2@.
migrationToParallel2 :: Migration
migrationToParallel2 = migrationTo2 maybeMigrateToParallel2

maybeMigrateFromParallel2
  :: (Typeable stateP2', Typeable stateP2)
  => Parallel stateP1 stateP2
  -> Maybe           stateP2'
maybeMigrateFromParallel2 (Parallel { stateP2 }) = cast stateP2

-- | Migrate to @cell2@ from @cell1 *** cell2@.
migrationFromParallel2 :: Migration
migrationFromParallel2 = constMigrationFrom2 maybeMigrateFromParallel2

-- | Combines all migrations related to parallel composition, favouring the first argument.
migrationParallel :: Migration
migrationParallel
  =  migrationToParallel1
  <> migrationFromParallel1
  <> migrationToParallel2
  <> migrationFromParallel2

-- * Migration involving 'ArrowChoice'

maybeMigrateToChoice1
  :: (Typeable stateLeft', Typeable stateLeft)
  => Choice stateLeft stateRight
  -> stateLeft'
  -> Maybe (Choice stateLeft stateRight)
maybeMigrateToChoice1 Choice { .. } choiceLeft' = do
  choiceLeft <- cast choiceLeft'
  return Choice { .. }

-- | Migrate @cell1@ to @cell1 ||| cell2@.
migrationToChoice1 :: Migration
migrationToChoice1 = migrationTo2 maybeMigrateToChoice1

maybeMigrateFromChoice1
  :: (Typeable stateLeft', Typeable stateLeft)
  => Choice stateLeft stateRight
  -> Maybe  stateLeft'
maybeMigrateFromChoice1 Choice { .. } = cast choiceLeft

-- | Migrate to @cell1@ from @cell1 ||| cell2@.
migrationFromChoice1 :: Migration
migrationFromChoice1 = constMigrationFrom2 maybeMigrateFromChoice1

maybeMigrateToChoice2
  :: (Typeable stateRight', Typeable stateRight)
  => Choice stateLeft stateRight
  -> stateRight'
  -> Maybe (Choice stateLeft stateRight)
maybeMigrateToChoice2 Choice { .. } choiceRight' = do
  choiceRight <- cast choiceRight'
  return Choice { .. }

-- | Migrate @cell2@ to @cell1 ||| cell2@.
migrationToChoice2 :: Migration
migrationToChoice2 = migrationTo2 maybeMigrateToChoice2

maybeMigrateFromChoice2
  :: (Typeable stateRight', Typeable stateRight)
  => Choice stateLeft stateRight
  -> Maybe            stateRight'
maybeMigrateFromChoice2 Choice { .. } = cast choiceRight

-- | Migrate to @cell2@ from @cell1 ||| cell2@.
migrationFromChoice2 :: Migration
migrationFromChoice2 = constMigrationFrom2 maybeMigrateFromChoice2

-- | Combines all migrations related to choice.
migrationChoice :: Migration
migrationChoice
  =  migrationToChoice1
  <> migrationFromChoice1
  <> migrationToChoice2
  <> migrationFromChoice2

-- * Control flow

maybeMigrateToExceptState
  :: (Typeable state, Typeable state')
  => ExceptState state e
  ->             state'
  -> Maybe (ExceptState state e)
maybeMigrateToExceptState (NotThrown _) state = NotThrown <$> cast state
maybeMigrateToExceptState (Exception e) _ = Just $ Exception e

-- | Migration from @cell2@ to @try cell1 >> safe cell2@
migrationToExceptState :: Migration
migrationToExceptState = migrationTo2 maybeMigrateToExceptState

maybeMigrateFromExceptState
  :: (Typeable state, Typeable state')
  => ExceptState state e
  -> Maybe       state'
maybeMigrateFromExceptState (NotThrown state) = cast state
maybeMigrateFromExceptState (Exception e) = Nothing

-- | Migration from @try cell1 >> safe cell2@ to @cell2@
migrationFromExceptState :: Migration
migrationFromExceptState = constMigrationFrom2 maybeMigrateFromExceptState

-- | Combines all control flow related migrations
migrationExceptState :: Migration
migrationExceptState = migrationToExceptState <> migrationFromExceptState

-- * Overall migration

-- | Combines all 'Cell'-related migrations.
migrationCell :: Migration
migrationCell
  =  migrationComposition
  <> migrationParallel
  <> migrationChoice
  <> migrationExceptState
