#!/bin/sh

INTDIR=${MERCURY_INT_DIR:-@LIBDIR@/ints}
DEPDIR=${MERCURY_DEP_DIR:-@LIBDIR@/deps}
MC=${MERCURY_COMPILER:-@LIBDIR@/nuprolog/@FULLARCH@/mercury_compile}

exec $MC -I "$INTDIR" -I "$DEPDIR" "$@"
