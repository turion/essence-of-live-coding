{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module LiveCoding.Cell.Util where

-- base
import Control.Arrow
import Control.Monad (guard, join)
import Control.Monad.IO.Class
import Data.Data (Data)
import Data.Foldable (toList)
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
import LiveCoding.Cell.Resample (resampleMaybe)
import LiveCoding.Cell.Util.Internal

-- * State accumulation

-- | Sum all past inputs, starting by the given number
sumFrom :: Monad m => Integer -> Cell m Integer Integer
sumFrom n0 = feedback n0 $ proc (n, acc) -> returnA -< (acc, acc + n)

-- | Count the number of ticks, starting at 0
count :: Monad m => Cell m a Integer
count = arr (const 1) >>> sumC

{- | Accumulate all incoming data,
   using the given fold function and start value.
   For example, if @'foldC' f b@ receives inputs @a0@, @a1@,...
   it will output @b@, @f a0 b@, @f a1 $ f a0 b@, and so on.
-}
foldC :: (Data b, Monad m) => (a -> b -> b) -> b -> Cell m a b
foldC step cellState = Cell {..}
  where
    cellStep b a = let b' = step a b in return (b, b')

-- | Like 'foldC', but does not delay the output.
foldC' :: (Data b, Monad m) => (a -> b -> b) -> b -> Cell m a b
foldC' step cellState = Cell {..}
  where
    cellStep b a = let b' = step a b in return (b', b')

{- | Initialise with a value 'a'.
   If the input is 'Nothing', @'hold' a@ will output the stored indefinitely.
   A new value can be stored by inputting @'Just' a@.
-}
hold :: (Data a, Monad m) => a -> Cell m (Maybe a) a
hold a = feedback a $ proc (ma, aOld) -> do
  let aNew = fromMaybe aOld ma
  returnA -< (aNew, aNew)

{- | Outputs @'Just' a@ whenever the the value a changes and 'Nothing' otherwise.
  The first output is always 'Nothing'. The following holds:

  @
    delay a >>> changes >>> hold a == delay a
  @
-}
changes ::
  (Data a, Eq a, Monad m) =>
  Cell m a (Maybe a)
changes = proc a -> do
  aLast <- delay Nothing -< Just a
  returnA
    -< do
      aLast' <- aLast
      guard $ a /= aLast'
      return a

-- | Like 'hold', but returns 'Nothing' until it is initialised by a @'Just' a@ value.
holdJust ::
  (Monad m, Data a) =>
  Cell m (Maybe a) (Maybe a)
holdJust = feedback Nothing $ arr keep
  where
    keep (Nothing, Nothing) = (Nothing, Nothing)
    keep (_, Just a) = (Just a, Just a)
    keep (Just a, Nothing) = (Just a, Just a)

-- | Hold the first value and output it indefinitely.
holdFirst :: (Data a, Monad m) => Cell m a a
holdFirst = Cell {..}
  where
    cellState = Nothing
    cellStep Nothing x = return (x, Just x)
    cellStep (Just s) _ = return (s, Just s)

-- | @boundedFIFO n@ keeps the first @n@ present values.
boundedFIFO :: (Data a, Monad m) => Int -> Cell m (Maybe a) (Seq a)
boundedFIFO n = foldC' step empty
  where
    step Nothing as = as
    step (Just a) as = Sequence.take n $ a <| as

{- | Buffers and returns the elements in First-In-First-Out order,
   returning 'Nothing' whenever the buffer is empty.
-}
fifo :: (Monad m, Data a) => Cell m (Seq a) (Maybe a)
fifo = feedback empty $ proc (as, accum) -> do
  let accum' = accum >< as
  returnA
    -< case accum' of
      Empty -> (Nothing, empty)
      a :<| as -> (Just a, as)

{- | Like 'fifo', but accepts lists as input.
   Each step is O(n) in the length of the list.
-}
fifoList :: (Monad m, Data a) => Cell m [a] (Maybe a)
fifoList = arr fromList >>> fifo

-- | Like 'fifoList', but generalised to any 'Foldable'.
fifoFoldable :: (Monad m, Data a, Foldable f) => Cell m (f a) (Maybe a)
fifoFoldable = arr toList >>> fifoList

-- | Returns 'True' iff the current input value is 'True' and the last input value was 'False'.
edge :: Monad m => Cell m Bool Bool
edge = proc b -> do
  bLast <- delay False -< b
  returnA -< b && not bLast

changeInit :: (Monad m, Data a, Eq a) => a -> Cell m a (Maybe a)
changeInit a0 = proc a -> do
  aLast <- delay a0 -< a
  returnA -< guard (a /= aLast) >> Just a

change :: (Monad m, Data a, Eq a) => Cell m a (Maybe a)
change = arr Just >>> changeInit Nothing >>> arr join


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
  = -- | Add an 'a' to the buffer.
    Push a
  | -- | Remove the oldest element from the buffer.
    Pop

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
buffer = Cell {..}
  where
    cellState = empty
    cellStep as commands = return (currentHead as, nextBuffer as commands)
    currentHead as = case viewl as of
      EmptyL -> Nothing
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
buffered ::
  (Monad m, Data a) =>
  Cell m (Maybe a) (Maybe b) ->
  Cell m (Maybe a) (Maybe b)
buffered cell = feedback Nothing $ proc (aMaybe, ticked) -> do
  aMaybe' <- buffer -< maybePop ticked ++ maybePush aMaybe
  bMaybe' <- cell -< aMaybe'
  returnA -< (bMaybe', void bMaybe')

-- * Detecting change

{- | Perform an action whenever the parameter @p@ changes, and the code is reloaded.

Note that this does not trigger any actions when adding, or removing an 'onChange' cell.
For this functionality, see "LiveCoding.Handle".
Also, when moving such a cell, the action may not be triggered reliably.
-}
onChange ::
  (Monad m, Data p, Eq p) =>
  -- | This parameter has to change during live coding to trigger an action
  p ->
  -- | This action gets passed the old parameter and the new parameter
  (p -> p -> a -> m b) ->
  Cell m a (Maybe b)
onChange p action = proc a -> do
  pCurrent <- arr $ const p -< ()
  pPrevious <- delay p -< pCurrent
  arrM $ whenDifferent action -< (pCurrent, pPrevious, a)

-- | Like 'onChange'', but with a dynamic input.
onChange' ::
  (Monad m, Data p, Eq p) =>
  -- | This action gets passed the old parameter and the new parameter
  (p -> p -> a -> m b) ->
  Cell m (p, a) (Maybe b)
onChange' action = proc (pCurrent, a) -> do
  pPrevious <- delay Nothing -< Just pCurrent
  bMaybeMaybe <- resampleMaybe $ arrM $ whenDifferent action -< (,pCurrent,a) <$> pPrevious
  returnA -< join bMaybeMaybe
