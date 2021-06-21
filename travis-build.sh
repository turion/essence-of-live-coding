#!/bin/bash

set -e

# Cabal

cabal new-build all --enable-tests
cabal new-test all --enable-tests
git fetch origin master:origin/master
git rebase origin/master --exec "cabal new-build all --enable-tests"

# Stack

export STACK_YAML="stack.${TRAVIS_HASKELL_VERSION}.yaml"

# install stack
curl -sSL https://get.haskellstack.org/ | sh

# build project with stack
stack --version
stack build --system-ghc --test
git fetch origin master:origin/master
git rebase origin/master --exec "stack build --system-ghc --test"
