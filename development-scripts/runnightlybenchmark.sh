#!/bin/bash
cd "$(dirname "$0")/.."
git pull
rm -r dist
./development-scripts/make.sh
./development-scripts/benchmarkall.sh /dev/null benchmark_results.txt 5

