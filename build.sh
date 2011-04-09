#!/bin/bash

RTSFLAGS="-threaded -rtsopts"
OPTIMIZATIONS="-O10 -msse2 -fdo-lambda-eta-expansion -fexcess-precision -fignore-asserts -fmax-simplifier-iterations=100000"

ghc --make $RTSFLAGS $OPTIMIZATIONS main.hs
