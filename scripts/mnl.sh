#!/bin/sh

# mnl - Mercury NU-Prolog Linker.
#
# Links NU-Prolog object files together with the Mercury library
# to produce an executable binary.

exec nc "$@" @LIBDIR@/nuprolog/@FULLARCH@/*.no
