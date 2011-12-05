#!/bin/bash

echo "
set datafile separator \",\"
set xdata time
set timefmt \"%Y-%m-%d %H:%M\"
set format x \"%y-%m-%d\"
set term postscript enhanced color
set output \"$2\"
plot \"-\" using 1:6
"
grep "$1" benchmark_results.txt
