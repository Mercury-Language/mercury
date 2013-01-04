#!/bin/bash

set -e
set -x

TIME_CMD="time -a -v"
PERF_CMD="perf stat -e cycles -e instructions -e cache-references -e cache-misses -e branches -e branch-misses -e faults -e migrations "
REPS=10
ARGS="512"

# this script takes a list of programs on it's command line and runs them $REPS
# times with $ARGS and builds logs of their runtimes.  It can be processed with
# log_2_r to be turned into a CSV file that can be processed with R to
# calculate average and standard deviation runtimes.  log_2_r can be found in
# benchmarks/tools/log_2_r

# Kill a process if it uses more than 3GB of memory.
MEMORY=$((3*1024*1024))
ulimit -S -m $MEMORY -v $MEMORY 
CORESIZE=unlimited
ulimit -S -c $CORESIZE

for PROGRAM in "$@"; do
    # use either 1 or 4 maker threads in a parallel program.
    if echo $PROGRAM | sed -e '/p[0-9]\+/q0;q1' > /dev/null; then
        ALL_MARKERS="1 4"
    else
        ALL_MARKERS=1
    fi

    for MARKERS in $ALL_MARKERS; do
        echo Testing $PROGRAM, with $MARKERS gc threads...
        export GC_MARKERS=$MARKERS;
        LOG=$PROGRAM.gc$MARKERS.log
        rm -f $LOG
        for ((I=0; I<$REPS; I++)); do
            echo Rep: $I
            echo Rep: $I >> $LOG
            $TIME_CMD -o $LOG ./$PROGRAM 512
        done
    done
done

