#!/bin/sh
#---------------------------------------------------------------------------#
# Copyright (C) 1995 University of Melbourne.
# This file may only be copied under the terms of the GNU General
# Public License - see the file COPYING in the Mercury distribution.
#---------------------------------------------------------------------------#

# msp - Mercury SICStus Prolog Interpreter
#
# A version of `sp' with `sp_builtin' and the Mercury library already loaded.
#
# Usage: msp [<sp options>]
#
# Environment variables: MERCURY_SICSTUS_INTERPRETER

INTERPRETER=\
${MERCURY_SICSTUS_INTERPRETER=@LIBDIR@/sicstus/@FULLARCH@/library.sicstus}

exec $INTERPRETER "$@"
