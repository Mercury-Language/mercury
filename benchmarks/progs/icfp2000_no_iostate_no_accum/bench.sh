#!/bin/bash

set -e

TIME_CMD="time -a -v" 
REPS=10

# Kill a process if it uses more than 3GB of memory.
MEMORY=$((3*1024*1024*1024))
ulimit -S -m $MEMORY -v $MEMORY 

for PROGRAM in "$@"; do
    if echo $PROGRAM | sed -e '/pargc/q0;q1' > /dev/null; then
        ALL_MARKERS="1 2 3 4 5 6 7 8"
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
            $TIME_CMD -o $LOG ./$PROGRAM < snowgoon.gml
        done
    done
done

