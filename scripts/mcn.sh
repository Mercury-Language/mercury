#!/bin/sh
#
# MCN - Mercury Compiler (compiled with NU-Prolog).
#
# Use `mcn -h' for help.
#
# Environment variables: MERCURY_INT_DIR, MERCURY_DEP_DIR, MERCURY_C_INCL_DIR,
# MERCURY_COMPILER.

INTDIR=${MERCURY_INT_DIR:-@LIBDIR@/ints}
DEPDIR=${MERCURY_DEP_DIR:-@LIBDIR@/deps}
C_INCL=${MERCURY_C_INCL_DIR:-@LIBDIR@/inc}
MC=${MERCURY_COMPILER:-@LIBDIR@/nuprolog/@FULLARCH@/mercury_compile.nu}

exec $MC -I "$INTDIR" -I "$DEPDIR" --c-include-directory "$C_INCL" "$@"
