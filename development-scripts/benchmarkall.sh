#!/bin/bash
#
# Copyright (C) 2011 Jim Farrand
# This program is free software: you can redistribute it and/or modify
# it without restriction
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#

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
    ./dist/build/ratcalc_benchmark/ratcalc_benchmark 2>>$OUT E              "$DATE" $COMMIT | tee --append "$RESULTS"
    ./dist/build/ratcalc_benchmark/ratcalc_benchmark 2>>$OUT E^Pi           "$DATE" $COMMIT | tee --append "$RESULTS"
    ./dist/build/ratcalc_benchmark/ratcalc_benchmark 2>>$OUT E^E            "$DATE" $COMMIT | tee --append "$RESULTS"
done
