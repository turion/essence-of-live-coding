#!/bin/bash

# Or find all files in a project with git ls-files:
fourmolu --mode inplace $(git ls-files '*.hs')

