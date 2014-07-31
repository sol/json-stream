#!/bin/bash

set -o errexit

cabal clean
cabal configure --enable-benchmarks
cabal build
dist/build/vs-aeson/vs-aeson -o report.html
echo "Benchmark result in report.html. Opening it now."
open report.html