#!/bin/sh

INTDIR=${MERCURY_INT_DIR:-@LIBDIR@/ints}
DEPDIR=${MERCURY_DEP_DIR:-@LIBDIR@/deps}
C_INCL=${MERCURY_C_INCL_DIR:-@LIBDIR@/inc}
MC=${MERCURY_COMPILER:-@LIBDIR@/nuprolog/@FULLARCH@/mercury_compile}

exec $MC -I "$INTDIR" -I "$DEPDIR" --c-include-directory "$C_INCL" "$@"
