#!/bin/bash

clear

# clear previous bin file
find ./bin -name 'server' -delete
find ./bin -name 'client' -delete

# local (static) compilation with stack 
stack install --local-bin-path ./bin

# clear .cabal file
find . -name '*.cabal' -delete
