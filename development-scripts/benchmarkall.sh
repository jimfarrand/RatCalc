#!/bin/bash
if [ -z "$1" ] ; then
    OUT=/dev/null
else
    OUT="$1"
fi

DATE="$(date '+%Y-%m-%d %H:%M')"

RESULTS="benchmark_results.txt"
COMMIT="$(git log | grep "^commit" | head -n 1 | sed -e 's/^commit //')"

./dist/build/ratcalc_benchmark/ratcalc_benchmark 2>>$OUT Addition       "$DATE" $COMMIT | tee --append "$RESULTS"
./dist/build/ratcalc_benchmark/ratcalc_benchmark 2>>$OUT Multiplication "$DATE" $COMMIT | tee --append "$RESULTS"
./dist/build/ratcalc_benchmark/ratcalc_benchmark 2>>$OUT Division       "$DATE" $COMMIT | tee --append "$RESULTS"
./dist/build/ratcalc_benchmark/ratcalc_benchmark 2>>$OUT Pi             "$DATE" $COMMIT | tee --append "$RESULTS"
