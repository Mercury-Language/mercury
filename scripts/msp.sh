#!/bin/sh

# msp - Mercury SICStus Prolog Interpreter
#
# A version of `sp' with `sp_builtin' and the Mercury library already loaded.
#
# Usage: msp [<sp options>]
#
# Environment variables: MERCURY_SICSTUS_INTERPRETER

INTERPRETER=\
${MERCURY_SICSTUS_INTERPRETER:-@LIBDIR@/sicstus/@FULLARCH@/library.sicstus}

exec $INTERPRETER "$@"
