#!/bin/bash

set -o errexit

FILE="bench/json_files/users_200000.json"

cabal clean
cabal configure --enable-library-profiling --enable-executable-profiling
cabal build
dist/build/js-profile/js-profile $FILE +RTS -p

echo "Time profiling output:"

cat js-profile.prof
