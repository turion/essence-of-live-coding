-- essence-of-live-coding
import LiveCoding

t1 :: Num a => a
t1 = 4

t2 :: Num a => a
t2 = 8

main = do
  (debugger, observer) <- countDebugger
  launchedProgram <- launchWithDebugger (printSine t1) $ debugger
  await observer $ t1 * stepRate
  update launchedProgram $ printSine t2 `withDebugger` debugger
  await observer $ (t1 + t2) * stepRate
