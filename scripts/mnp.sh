#!/bin/sh
#---------------------------------------------------------------------------#
# Copyright (C) 1995 University of Melbourne.
# This file may only be copied under the terms of the GNU General
# Public License - see the file COPYING in the Mercury distribution.
#---------------------------------------------------------------------------#

# mnp - Mercury NU-Prolog Interpreter
#
# A version of `np' with `np_builtin' and the Mercury library already loaded.
#
# Usage: mnp [<np options>]
#
# Environment variables: MERCURY_INTERPRETER

INTERPRETER=${MERCURY_INTERPRETER=@LIBDIR@/nuprolog/@FULLARCH@/library.nu}

exec $INTERPRETER "$@"
