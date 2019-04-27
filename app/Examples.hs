{-# LANGUAGE Arrows #-}
module Examples where

-- base
import Control.Arrow
import Control.Concurrent

-- essenceoflivecoding
import LiveCoding
import LiveCoding.Bind
import LiveCoding.Cell
import LiveCoding.Exceptions
import LiveCoding.Forever
import LiveCoding.LiveProgram

sumFrom :: Monad m => Integer -> Cell m Integer Integer
sumFrom n0 = feedback n0 $ proc (n, acc) -> returnA -< (acc, acc + n)

count :: Monad m => Cell m a Integer
count = arr (const 1) >>> sumFrom 0
countUpTo     n = arr (const   1 ) >>> sumFrom 0 >>> throwIf_ (>= n)
countDownFrom n = arr (const (-1)) >>> sumFrom n >>> throwIf_ (<= 0)

throwWhenReaches
  :: Monad m
  => Integer -> Cell (ExceptT () m) Integer Integer
throwWhenReaches amplitude = proc n -> if n == amplitude then throwC -< () else returnA -< n

saw1 :: Monad m => Integer -> Cell m () Integer
saw1 amplitude = foreverC $ runCellExcept $ do
  try $ countUpTo     amplitude
  try $ countDownFrom amplitude

saw2 :: Monad m => Integer -> Cell m () Integer
saw2 amplitude = foreverC $ runCellExcept $ do
  try $ countUpTo amplitude
  try $ countUpTo amplitude

example1 :: Integer -> LiveProgram IO
example1 n = liveCell $ sine 1 >>> arrM print >>> constM (threadDelay 10000)

example2 :: Integer -> LiveProgram IO
example2 n = liveCell $ saw2 n >>> arrM print >>> constM (threadDelay 500000)


myMapM_ f (a : as) = f a *> myMapM_ f as
myMapM_ _ [] = return ()

listThing :: Monad m => Cell m a Integer
listThing = safely $ myMapM_ (try . countUpTo) [3,5,6] *> safe count

example :: LiveProgram IO
example = liveCell $ listThing >>> arrM print >>> constM (threadDelay 500000)
