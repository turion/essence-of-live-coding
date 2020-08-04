#! /bin/sh
set -e

test_for_errors() {
if grep -qE "error|unknown" result.txt; then
  cat result.txt
  rm result.txt
  exit 1
fi
rm result.txt
}

pushd gears
cabal repl &> result.txt <<EOF
:livegears
:gearsreload
EOF
test_for_errors
popd

pushd essence-of-live-coding-gloss-example
cabal repl &> result.txt <<EOF
:livegloss
:livereloadgloss
EOF
test_for_errors
popd

pushd essence-of-live-coding-pulse-example
cabal repl &> result.txt <<EOF
:livepulse
:livereloadpulse
EOF
test_for_errors
popd
