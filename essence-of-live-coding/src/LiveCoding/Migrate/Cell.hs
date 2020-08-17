{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module LiveCoding.Migrate.Cell where

-- base
import Data.Data

-- syb
import Data.Generics.Aliases

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Cell.Feedback
import LiveCoding.Exceptions
import LiveCoding.Migrate.Migration
import Control.Applicative (Alternative((<|>)))

-- * Migrations to and from pairs

-- ** Generic migration functions

-- | Builds the migration function for a pair, or product type,
--   such as tuples, but customisable to your own products.
--   You need to pass it the equivalents of 'fst', 'snd', and '(,)'.
--   Tries to migrate the value into the first element, then into the second.
maybeMigrateToPair
  :: (Typeable a, Typeable b, Typeable c)
  => (t a b -> a)
  -- ^ The accessor of the first element
  -> (t a b -> b)
  -- ^ The accessor of the second element
  -> (a -> b -> t a b)
  -- ^ The constructor
  -> t a b
  -- ^ The pair
  -> c
  -- ^ The new value for the first or second element
  -> Maybe (t a b)
maybeMigrateToPair fst snd cons pair c = do
  flip cons (snd pair) <$> cast c <|> cons (fst pair) <$> cast c

-- | Like 'maybeMigrateToPair', but in the other direction.
--   Again, it is biased with respect to the first element of the pair.
maybeMigrateFromPair
  :: (Typeable a, Typeable b, Typeable c)
  => (t a b -> a)
  -- ^ The accessor of the first element
  -> (t a b -> b)
  -- ^ The accessor of the second element
  -> t a b
  -> Maybe c
maybeMigrateFromPair fst snd pair = cast (fst pair) <|> cast (snd pair)

-- ** Migrations involving sequential compositions of cells

-- | Migrate @cell@ to @cell >>> cell'@, and if this fails, to @cell' >>> cell@.
migrationToComposition :: Migration
migrationToComposition = migrationTo2 $ maybeMigrateToPair state1 state2 Composition


-- | Migrate @cell1 >>> cell2@ to @cell1@, and if this fails, to @cell2@.
migrationFromComposition :: Migration
migrationFromComposition = constMigrationFrom2 $ maybeMigrateFromPair state1 state2

-- | Combines all migrations related to composition, favouring migration to compositions.
migrationComposition :: Migration
migrationComposition
  =  migrationToComposition
  <> migrationFromComposition

-- ** Migrations involving parallel compositions of cells

-- | Migrate @cell@ to @cell *** cell'@, and if this fails, to @cell' *** cell@.
migrationToParallel :: Migration
migrationToParallel = migrationTo2 $ maybeMigrateToPair stateP1 stateP2 Parallel

-- | Migrate from @cell1 *** cell2@ to @cell1@, and if this fails, to @cell2@.
migrationFromParallel :: Migration
migrationFromParallel = constMigrationFrom2 $ maybeMigrateFromPair stateP1 stateP2

-- | Combines all migrations related to parallel composition, favouring migration to parallel composition.
migrationParallel :: Migration
migrationParallel
  =  migrationToParallel
  <> migrationFromParallel

-- ** Migration involving 'ArrowChoice'

-- | Migrate @cell@ to @cell ||| cell'@, and if this fails, to @cell' ||| cell@.
migrationToChoice :: Migration
migrationToChoice = migrationTo2 $ maybeMigrateToPair choiceLeft choiceRight Choice

-- | Migrate from @cell1 ||| cell2@ to @cell1@, and if this fails, to @cell2@.
migrationFromChoice :: Migration
migrationFromChoice = constMigrationFrom2 $ maybeMigrateFromPair choiceLeft choiceRight

-- | Combines all migrations related to choice, favouring migration to choice.
migrationChoice :: Migration
migrationChoice
  =  migrationToChoice
  <> migrationFromChoice

-- ** Feedback

-- | Migrate from @cell@ to @feedback s cell@, and if this fails, to @feedback (cellState cell) cell'@.
migrationToFeedback :: Migration
migrationToFeedback = migrationTo2 $ maybeMigrateToPair sPrevious sAdditional Feedback

-- | Migrate from @feedback s cell@ to @cell@, and if this fails, to @Cell { cellState = s, .. }@.
migrationFromFeedback :: Migration
migrationFromFeedback = constMigrationFrom2 $ maybeMigrateFromPair sPrevious sAdditional

-- | Combines all migrations related to feedback, favouring migration to feedback.
migrationFeedback :: Migration
migrationFeedback = migrationToFeedback <> migrationFromFeedback

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
  <> migrationFeedback
