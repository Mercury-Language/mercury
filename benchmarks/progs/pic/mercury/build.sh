#!/bin/bash

PROG=pic
PROF_ARGS=10000
# Note that we shouldn't go higher than O2.  Name mangling from higher-order
# and type sepcialisation confiuses the interface between the analysis tool and
# the compiler.
MCFLAGS="-O2 --intermodule-optimisation"

set -x
set -e

function build
{
    local NAME=$1
    shift
    local NTHREADS=$1
    shift

    rm -rf $PROG Mercury $PROG.$NAME Mercury.$NAME
    mmc --make $PROG --runtime-flags "-P $NTHREADS" --c-debug $@
    mv $PROG $PROG.$NAME
    mv Mercury Mercury.$NAME
}

build asmfast_gc.profdeep 1 --grade asm_fast.gc.profdeep $MCFLAGS \
    --profile-for-implicit-parallelism
MERCURY_OPTIONS="--deep-procrep-file" ./$PROG.asmfast_gc.profdeep $PROF_ARGS
rm -f $PROG.feedback
mdprof_feedback --report \
    --candidate-parallel-conjunctions \
    --desired-parallelism 8 \
    --implicit-parallelism-clique-cost-threshold 2000 \
    --implicit-parallelism-call-site-cost-threshold 2000 \
    --implicit-parallelism-dependant-conjunctions \
    --implicit-parallelism-intermodule-var-use \
    Deep.data $PROG.feedback

build asmfast_gc 1 --grade asm_fast.gc $MCFLAGS
build asmfast_gc.stseg 1 --grade asm_fast.gc.stseg $MCFLAGS

for NTHREADS in 1 2 3 4; do

    build asmfast_gc.p$NTHREADS $NTHREADS \
        --grade asm_fast.gc.par --feedback-file $PROG.feedback \
        --implicit-parallelism $MCFLAGS
    build asmfast_gc.stseg.p$NTHREADS $NTHREADS \
        --grade asm_fast.gc.par.stseg --feedback-file $PROG.feedback \
        --implicit-parallelism $MCFLAGS

done

