#! /bin/sh
set -e

test_repl() {
  start_repl
  test_for_errors
}

start_repl() {
  cat ../replcommands.txt | cabal repl &> result.txt
}

test_for_errors() {
if grep -qE "error|unknown" result.txt; then
  cat result.txt
  rm result.txt
  exit 1
fi
rm result.txt
}

test_for_handle_messages() {
# See essence-of-live-coding-ghci-example/app/Main.hs
if grep -qE "Creating" result.txt && grep -qE "Destroying" result.txt
then
  rm result.txt
else
  cat result.txt
  rm result.txt
  exit 1
fi
}

pushd gears
test_repl
popd

pushd essence-of-live-coding-gloss-example
test_repl
popd

pushd essence-of-live-coding-pulse-example
test_repl
popd

pushd essence-of-live-coding-ghci-example
start_repl
test_for_handle_messages
popd
