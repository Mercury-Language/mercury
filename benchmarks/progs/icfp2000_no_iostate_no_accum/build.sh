#!/bin/sh

set -e

rm -rf main Mercury
mmc -O3 --grade asm_fast.gc --make main
mv main main.asmfast_gc

rm -rf main Mercury
mmc -O3 --grade asm_fast.gc.par --make main
mv main main.asmfast_pargc

rm -rf main Mercury
mmc -O3 --grade asm_fast.gc.profdeep --profile-for-implicit-parallelism \
    --runtime-flags "--deep-procrep-file" --make main
rm -f Deep.data Deep.procrep
time ./main < snowgoon-simple.gml

rm -f main.feedback
mdprof_feedback --candidate-parallel-conjunctions \
    --desired-parallelism 8 \
    --implicit-parallelism-clique-cost-threshold 1000 \
    --implicit-parallelism-call-site-cost-threshold 1000 \
    Deep.data main.feedback

mv main main.asmfast_gc_profdeep

for NTHREADS in 1 2 3 4 5 6 7 8; do
    rm -rf main Mercury
    mmc -O3 --grade asm_fast.gc.par --implicit-parallelism \
        --feedback-file main.feedback --runtime-flags "-P $NTHREADS --thread-pinning" --make main
    mv main main.asmfast_pargc.autopar.pinning.p$NTHREADS

    rm -rf main Mercury
    mmc -O3 --grade asm_fast.gc.par --implicit-parallelism \
        --feedback-file main.feedback --runtime-flags "-P $NTHREADS" --make main
    mv main main.asmfast_pargc.autopar.p$NTHREADS
done

