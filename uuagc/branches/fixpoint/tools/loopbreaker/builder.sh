#!/bin/bash

mkdir -p test
cp -f Bad.ag test
cp -f intermediate.attrs test
cd test
../../../dist/build/uuagc/uuagc -a --forceirrefutable=intermediate.attrs Bad.ag
ghc --make Bad.hs -o bad
