#!/bin/sh
#
# MC - Mercury Compiler.
#
# Use `mc -h' for help.
#
# Environment variables: MERCURY_INT_DIR, MERCURY_DEP_DIR, MERCURY_C_INCL_DIR,
# MERCURY_COMPILER.

INTDIR=${MERCURY_INT_DIR:-@LIBDIR@/ints}
DEPDIR=${MERCURY_DEP_DIR:-@LIBDIR@/deps}
C_INCL=${MERCURY_C_INCL_DIR:-@LIBDIR@/inc}
MC=${MERCURY_COMPILER:-"@LIBDIR@/bin/@FULLARCH@/mercury_compile --"}

# this is a temporary hack so I can get some idea of who is actually
# using Mercury
{ date; echo "$@"; echo; } >> /tmp/.mc_log.$USER
chmod go+r /tmp/.mc_log.$USER

exec $MC -I "$INTDIR" -I "$DEPDIR" --c-include-directory "$C_INCL" "$@"
