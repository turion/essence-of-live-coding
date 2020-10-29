-- base
import Control.Arrow

-- essence-of-live-coding
import LiveCoding

t1 :: Num a => a
t1 = 8
t2 :: Num a => a
t2 = 4

printSineWait' t = liveCell
  $   safely (sineWait t)
  >>> printEverySecond

main = do
  (debugger, observer) <- countDebugger
  launchedProgram <- launchWithDebugger (printSineWait' t1) $ debugger
  await observer $ (2 + t1) * stepRate
  update launchedProgram $ printSineWait' t2 `withDebugger` debugger
  await observer $ (2 + t1 + t2) * stepRate
