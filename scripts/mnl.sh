#!/bin/sh
#---------------------------------------------------------------------------#
# Copyright (C) 1995 University of Melbourne.
# This file may only be copied under the terms of the GNU General
# Public License - see the file COPYING in the Mercury distribution.
#---------------------------------------------------------------------------#

# mnl - Mercury NU-Prolog Linker.
#
# Links NU-Prolog object files together with the Mercury library
# to produce an executable binary.
#
# Usage: mnl [-v|--verbose] [<nc link options>] files...
#
# Environment variables: MERCURY_LIB_DIR, MERCURY_LIB_OBJS

NULIBDIR=${MERCURY_LIB_DIR=@LIBDIR@/nuprolog/@FULLARCH@}
LIBRARY_OBJS=${MERCURY_LIB_OBJS=`cd $NULIBDIR; echo *.no`}

options=
verbose=false
debug=false

while true; do
	case "$1" in
		-v|--verbose)
			verbose=true
			shift
			;;
		-d|--debug)
			debug=true
			shift
			;;
		-e|-u|-o|-F)
			options="$options $1 $2"
			shift 2
			;;
		-*)	options="$options $1"
			shift
			;;
		*)	break 2
			;;
	esac
done
if $debug; then
	LIBRARY_OBJS="$LIBRARY_OBJS debug.no portray.no"
fi

objlist=
for obj in $LIBRARY_OBJS; do
	if echo "" "$@" "" | grep " $obj " > /dev/null; then
		true
	else
		objlist="$objlist $NULIBDIR/$obj"
	fi
done

if $verbose; then
	echo nc $options $objlist "$@"
fi
exec nc $options $objlist "$@"
