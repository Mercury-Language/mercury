#!/bin/sh

set -e

rm -rf mandelbrot Mercury
mmc -O3 --grade asm_fast.gc --make mandelbrot
mv mandelbrot mandelbrot.asmfast_gc

rm -rf mandelbrot Mercury
mmc -O3 --grade asm_fast.gc.par --make mandelbrot
mv mandelbrot mandelbrot.asmfast_pargc

rm -rf mandelbrot Mercury
mmc -O3 --grade asm_fast.gc.profdeep --profile-for-implicit-parallelism \
    --runtime-flags "--deep-procrep-file" --make mandelbrot
rm -f Deep.data Deep.procrep
time ./mandelbrot 300

rm -f mandelbrot.feedback
mdprof_feedback --candidate-parallel-conjunctions \
    --desired-parallelism 8 \
    --implicit-parallelism-clique-cost-threshold 1000 \
    --implicit-parallelism-call-site-cost-threshold 1000 \
    Deep.data mandelbrot.feedback

mv mandelbrot mandelbrot.asmfast_gc_profdeep

for NTHREADS in 1 2 4 8; do
    rm -rf mandelbrot Mercury
    mmc -O3 --grade asm_fast.gc.par --implicit-parallelism \
        --feedback-file mandelbrot.feedback --runtime-flags "-P $NTHREADS --thread-pinning" --make mandelbrot
    mv mandelbrot mandelbrot.asmfast_pargc.autopar.pinning.p$NTHREADS

    rm -rf mandelbrot Mercury
    mmc -O3 --grade asm_fast.gc.par --implicit-parallelism \
        --feedback-file mandelbrot.feedback --runtime-flags "-P $NTHREADS" --make mandelbrot
    mv mandelbrot mandelbrot.asmfast_pargc.autopar.p$NTHREADS
done

