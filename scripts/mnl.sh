#!/bin/sh

# mnl - Mercury NU-Prolog Linker.
#
# Links NU-Prolog object files together with the Mercury library
# to produce an executable binary.

NULIBDIR=${MERCURY_LIB_DIR:-@LIBDIR@/nuprolog/@FULLARCH@}

options=

while true; do
	case "$1" in
		-*)	options="$options $1"
			shift
			;;
		*)	break 2
			;;
	esac
done

exec nc $options "$NULIBDIR"/*.no "$@"
