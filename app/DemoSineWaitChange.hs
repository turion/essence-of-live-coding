-- base
import Control.Arrow

-- essenceoflivecoding
import LiveCoding

t1 :: Num a => a
t1 = 10
t2 :: Num a => a
t2 = 5

printSineWait' t = liveCell
  $   safely (sineWait t)
  >>> printEverySecond

main = do
  (debugger, observer) <- countDebugger
  var <- launchWithDebugger (printSineWait' t1) $ debugger
  await observer $ (2 + t1) * stepRate
  update var $ printSineWait' t2
  await observer $ (2 + t1 + t2) * stepRate
