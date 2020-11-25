{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
module LiveCoding.Cell.Util where

-- base
import Control.Arrow
import Control.Monad.IO.Class
import Data.Data (Data)
import Data.Functor (void)
import Data.Maybe

-- containers
import Data.Sequence hiding (take)
import qualified Data.Sequence as Sequence

-- time
import Data.Time.Clock

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Cell.Feedback

-- * State accumulation

-- | Sum all past inputs, starting by the given number
sumFrom :: Monad m => Integer -> Cell m Integer Integer
sumFrom n0 = feedback n0 $ proc (n, acc) -> returnA -< (acc, acc + n)

-- | Count the number of ticks, starting at 0
count :: Monad m => Cell m a Integer
count = arr (const 1) >>> sumC

-- | Accumulate all incoming data,
--   using the given fold function and start value.
--   For example, if @'foldC' f b@ receives inputs @a0@, @a1@,...
--   it will output @b@, @f a0 b@, @f a1 $ f a0 b@, and so on.
foldC :: (Data b, Monad m) => (a -> b -> b) -> b -> Cell m a b
foldC step cellState = Cell { .. }
  where
    cellStep b a = let b' = step a b in return (b, b')

-- | Like 'foldC', but does not delay the output.
foldC' :: (Data b, Monad m) => (a -> b -> b) -> b -> Cell m a b
foldC' step cellState = Cell { .. }
  where
    cellStep b a = let b' = step a b in return (b', b')

-- | Initialise with a value 'a'.
--   If the input is 'Nothing', @keep a@ will output the stored indefinitely.
--   A new value can be stored by inputting @'Just' a@.
keep :: (Data a, Monad m) => a -> Cell m (Maybe a) a
keep a = feedback a $ proc (ma, aOld) -> do
  let aNew = fromMaybe aOld ma
  returnA -< (aNew, aNew)

-- | Like 'keep', but returns 'Nothing' until it is initialised by a @'Just' a@ value.
keepJust
  :: (Monad m, Data a)
  => Cell m (Maybe a) (Maybe a)
keepJust = feedback Nothing $ arr keep
  where
    keep (Nothing, Nothing) = (Nothing, Nothing)
    keep (_, Just a) = (Just a, Just a)
    keep (Just a, Nothing) = (Just a, Just a)

-- | @boundedFIFO n@ keeps the first @n@ present values.
boundedFIFO :: (Data a, Monad m) => Int -> Cell m (Maybe a) (Seq a)
boundedFIFO n = foldC' step empty
  where
    step Nothing  as = as
    step (Just a) as = Sequence.take n $ a <| as

-- | Returns 'True' iff the current input value is 'True' and the last input value was 'False'.
edge :: Monad m => Cell m Bool Bool
edge = proc b -> do
  bLast <- delay False -< b
  returnA -< b && not bLast

-- * Debugging utilities

-- | Print the current UTC time, prepended with the first 8 characters of the given message.
printTime :: MonadIO m => String -> m ()
printTime msg = liftIO $ putStrLn . (take 8 msg ++) . show =<< getCurrentTime

-- | Like 'printTime', but as a cell.
printTimeC :: MonadIO m => String -> Cell m () ()
printTimeC msg = constM $ printTime msg

-- * Buffers

-- | A command to send to 'buffer'.
data BufferCommand a
  = Push a
  -- ^ Add an 'a' to the buffer.
  | Pop
  -- ^ Remove the oldest element from the buffer.

-- | Pushes @'Just' a@ and does nothing on 'Nothing'.
maybePush :: Maybe a -> [BufferCommand a]
maybePush = (Push <$>) . maybeToList

-- | Pops on @'Just' a@ and does nothing on 'Nothing'.
maybePop :: Maybe a -> [BufferCommand b]
maybePop = (const Pop <$>) . maybeToList

{- | Single-consumer, multi-producer buffer.

The output value is the oldest value in the buffer,
if it exists.

* Add elements by inputting @'Push' a@.
* Remove elements by inputting 'Pop'.
-}
buffer :: (Monad m, Data a) => Cell m [BufferCommand a] (Maybe a)
buffer = Cell { .. }
  where
    cellState = empty
    cellStep as commands = return (currentHead as, nextBuffer as commands)
    currentHead as = case viewl as of
      EmptyL   -> Nothing
      a :< as' -> Just a
    nextBuffer as [] = as
    nextBuffer as (Push a : commands) = nextBuffer (as |> a) commands
    nextBuffer as (Pop : commands) = nextBuffer (Sequence.drop 1 as) commands

{- | Equip a 'Cell' with a 'buffer'.

* Whenever @'Just' a@ value enters @buffered cell@,
  it is added to the buffer.
* Whenever @cell@ emits @'Just' b@,
  the oldest value is dropped from the buffer.
* @cell@ is always fed with 'Just' the oldest value from the buffer,
  except when the buffer is empty, then it is fed 'Nothing'.

This construction guarantees that @cell@ produces exactly one output for every input value.
-}
buffered
  :: (Monad m, Data a)
  => Cell m (Maybe a) (Maybe b)
  -> Cell m (Maybe a) (Maybe b)
buffered cell = feedback Nothing $ proc (aMaybe, ticked) -> do
  aMaybe' <- buffer -< maybePop ticked ++ maybePush aMaybe
  bMaybe' <- cell   -< aMaybe'
  returnA           -< (bMaybe', void bMaybe')
