#!/bin/sh
P=`pwd`/libatomic_ops-install
cd libatomic_ops
# Mercury-specific: allow additional arguments.
# Mercury-specific: explicitly set --libdir to avoid some 64-bit Linux
# systems installing in $P/lib64.
./configure --prefix=$P --libdir=$P/lib "$@"
$MAKE CC="$CC" AR="$AR" RANLIB="$RANLIB" install
