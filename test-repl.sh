#! /bin/sh
set -e

test_repl() {
  cat ../replcommands.txt | cabal repl &> result.txt
  test_for_errors
}

test_for_errors() {
if grep -qE "error|unknown" result.txt; then
  cat result.txt
  rm result.txt
  exit 1
fi
rm result.txt
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
