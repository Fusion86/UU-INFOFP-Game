#!/bin/sh

if [ ! -f "package.yaml" ]; then
    echo "This script should be run from the project root."
    exit 1
fi

stack build --ghc-options -O2 --copy-bins --local-bin-path "./dist"
cp -r assets/ dist/
cp -r deps/linux64/* dist/
