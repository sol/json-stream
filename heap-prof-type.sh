#!/bin/bash

set -o errexit

FILE="bench/json_files/users_200000.json"

cabal clean
cabal configure --enable-library-profiling --enable-executable-profiling
cabal build

time dist/build/js-profile/js-profile $FILE +RTS -hy -i0.01

hp2ps -c js-profile.hp
mv js-profile.ps heap.ps
echo "Space profiling output by type in heap.ps. Trying to open it now."
open heap.ps
