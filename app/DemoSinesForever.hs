-- TODO Fix imports to a single one
-- essenceoflivecoding
import LiveCoding.Debugger
import LiveCoding.Debugger.StatePrint
import LiveCoding.Cell
import LiveCoding.RuntimeIO
import LiveCoding.Forever

main = do
  (debugger, observer) <- countDebugger
  var <- launchWithDebugger printSinesForever $ debugger -- <> statePrint
  await observer $ 12 * stepRate
  putStrLn "[...]"
