-- essenceoflivecoding
import LiveCoding

main = do
  (debugger, observer) <- countDebugger
  var <- launchWithDebugger printSineWait $ debugger -- <> gshowDebugger --statePrint
  -- await observer 1
  await observer $ 16 * stepRate
