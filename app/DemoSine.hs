-- TODO Fix imports to a single one
-- essenceoflivecoding
import LiveCoding.Debugger
import LiveCoding.Debugger.StatePrint
import LiveCoding.Cell
import LiveCoding.RuntimeIO

t1 :: Num a => a
t1 = 6
t2 :: Num a => a
t2 = 10

main = do
  (debugger, observer) <- countDebugger
  var <- launchWithDebugger (printSine t1) $ debugger
  await observer $ t1 * stepRate
  update var $ printSine t2
  await observer $ (t1 + t2) * stepRate
