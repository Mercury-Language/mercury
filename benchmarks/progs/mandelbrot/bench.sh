#!/bin/bash

set -e

TIME_CMD="time -a -v" 
REPS=25

# Kill a process if it uses more than 3GB of memory.
MEMORY=$((3*1024*1024*1024))
ulimit -S -m $MEMORY -v $MEMORY 

for PROGRAM in "$@"; do
    echo Testing $PROGRAM...
    LOG=$PROGRAM.log
    rm -f $LOG
    for ((I=0; I<$REPS; I++)); do
        echo Rep: $I
        echo Rep: $I >> $LOG
        $TIME_CMD -o $LOG ./$PROGRAM
    done
done

