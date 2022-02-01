#!/bin/bash

create_cabal_project () {
    local compiler=$1

    ln -sf cabal.project "cabal.project.$compiler"

    cabal freeze --enable-tests --project-file="cabal.project.$compiler" -w "ghc-$compiler"
}

create_cabal_project "8.8.4"
create_cabal_project "8.10.7"
create_cabal_project "9.0.2"
