-- essenceoflivecoding
import LiveCoding

main = do
  (debugger, observer) <- countDebugger
  var <- launchWithDebugger printSinesForever $ debugger
  await observer $ 12 * stepRate
  --putStrLn "[...]"
