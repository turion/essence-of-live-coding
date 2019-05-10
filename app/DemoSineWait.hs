-- TODO Fix imports to a single one
-- essenceoflivecoding
import LiveCoding.Debugger
import LiveCoding.Debugger.StatePrint
import LiveCoding.Cell
import LiveCoding.Bind (printSineWait)
import LiveCoding.RuntimeIO

main = do
  (debugger, observer) <- countDebugger
  var <- launchWithDebugger printSineWait $ debugger -- <> gshowDebugger --statePrint
  -- await observer 1
  await observer $ 16 * stepRate
