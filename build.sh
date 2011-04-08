#!/bin/bash

ghc -threaded -fllvm --make -O10 -msse2 main.hs
