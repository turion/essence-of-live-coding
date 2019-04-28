-- TODO Fix imports to a single one
-- essenceoflivecoding
import LiveCoding
import LiveCoding.Cell
import LiveCoding.Bind (printSineChange)
import LiveCoding.RuntimeIO

main = do
  (debugger, observer) <- countDebugger
  var <- launchWithDebugger printSineChange debugger
  await observer $ 16 * stepRate
