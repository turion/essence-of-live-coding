-- essence-of-live-coding
import LiveCoding

main = do
  (debugger, observer) <- countDebugger
  _ <- launchWithDebugger printSineWait $ debugger -- <> gshowDebugger --statePrint
  -- await observer 1
  await observer $ 12 * stepRate
