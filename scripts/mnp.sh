#!/bin/sh

# mnp - Mercury NU-Prolog Interpreter
#
# A version of `np' with `np_builtin' and the Mercury library already loaded.
#
# Usage: mnp [<np options>]
#
# Environment variables: MERCURY_INTERPRETER

INTERPRETER=${MERCURY_INTERPRETER:-@LIBDIR@/nuprolog/@FULLARCH@/library}

exec $INTERPRETER "$@"
