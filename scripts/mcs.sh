#!/bin/sh
#
# MCS - Mercury Compiler, compiled with Sicstus
#
# Use `mcs -h' for help.
#
# Environment variables: MERCURY_INT_DIR, MERCURY_DEP_DIR, MERCURY_C_INCL_DIR,
# MERCURY_COMPILER_SICSTUS.

INTDIR=${MERCURY_INT_DIR:-@LIBDIR@/ints}
DEPDIR=${MERCURY_DEP_DIR:-@LIBDIR@/deps}
C_INCL=${MERCURY_C_INCL_DIR:-@LIBDIR@/inc}
MCS=${MERCURY_COMPILER_SICSTUS:-@LIBDIR@/sicstus/@FULLARCH@/mercury_compile.sicstus}

exec $MCS -I "$INTDIR" -I "$DEPDIR" --c-include-directory "$C_INCL" "$@"
