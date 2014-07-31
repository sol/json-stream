#!/bin/bash

set -o errexit

FILE="bench/json_files/users_20000.json"

cabal clean
cabal configure --enable-library-profiling --enable-executable-profiling
cabal build

dist/build/js-profile/js-profile $FILE +RTS -hd -i0.01

hp2ps -c js-profile.hp
mv js-profile.ps constructors.ps
echo "Space profiling output by constructor in constrs.ps -- trying to open it now."
open constructors.ps
