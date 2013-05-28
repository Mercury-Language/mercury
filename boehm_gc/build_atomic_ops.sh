#!/bin/sh
P=`pwd`/libatomic_ops-install
cd libatomic_ops
# Mercury-specific: allow additional arguments.
./configure --prefix=$P "$@"
$MAKE CC="$CC" AR="$AR" RANLIB="$RANLIB" install
