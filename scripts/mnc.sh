#!/bin/sh

# mnc - Mercury NU-Prolog Compiler.
#
# Compiles Mercury programs to NU-Prolog object code (*.no).

nc_builtin_nl=${MERCURY_NC_BUILTIN:-@LIBDIR@/nuprolog/nc_builtin.nl}

options=

unset target

while true; do
	case "$1" in
		-F)	options="$options $1 $2"
			shift 2
			;;
		-o)	target="$2"
			shift 2
			;;
		-*)	options="$options $1"
			shift
			;;
		*)	break 2
			;;
	esac
done

for file in "$@"; do
	echo "mnc: compiling \`$file'"
	tmp=/tmp/mnc$$
	trap 'rm -f $tmp.nl $tmp.ns $tmp.no; exit 1' 1 2 3 13 15
	cat $nc_builtin_nl $file > $tmp.nl
	nc -c $options $tmp.nl
	rm $tmp.nl $tmp.ns
	mv $tmp.no ${target:-`dirname $file`/`basename $file .nl`.no}
done
