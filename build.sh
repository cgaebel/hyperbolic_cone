#!/bin/bash

ghc -rtsopts -threaded --make -O10 -msse2 main.hs
