#!/bin/bash
if [ -z "$1" ] ; then
    OUT=/dev/null
else
    OUT="$1"
fi

if [ -z "$2" ] ; then
    RESULTS="benchmark_results.txt"
else
    RESULTS="$2"
fi

if [ -z "$3" ] ; then
    COUNT="1"
else
    COUNT="$3"
fi


COMMIT="$(git log | grep "^commit" | head -n 1 | sed -e 's/^commit //')"

for (( I=0; I<COUNT; I++ )) ; do
    DATE="$(date '+%Y-%m-%d %H:%M')"
    ./dist/build/ratcalc_benchmark/ratcalc_benchmark 2>>$OUT Addition       "$DATE" $COMMIT | tee --append "$RESULTS"
    ./dist/build/ratcalc_benchmark/ratcalc_benchmark 2>>$OUT Multiplication "$DATE" $COMMIT | tee --append "$RESULTS"
    ./dist/build/ratcalc_benchmark/ratcalc_benchmark 2>>$OUT Division       "$DATE" $COMMIT | tee --append "$RESULTS"
    ./dist/build/ratcalc_benchmark/ratcalc_benchmark 2>>$OUT Pi             "$DATE" $COMMIT | tee --append "$RESULTS"
done
