#!/bin/bash

set -e

TIME_CMD="time -a -v" 
REPS=20

for PROGRAM in "$@"; do
    echo Testing $PROGRAM...
    LOG=$PROGRAM.log
    rm -f $LOG
    for ((I=0; I<$REPS; I++)); do
        echo Rep: $I
        echo Rep: $I >> $LOG
        $TIME_CMD -o $LOG ./$PROGRAM < snowgoon.gml
    done
done

