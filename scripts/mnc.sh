#!/bin/sh

# MNC - Mercury NU-Prolog Compiler.
#
# Compiles Mercury programs to NU-Prolog object code (*.no).
#
# Usage: same as for `nc'.
#
# Environment variables: MERCURY_NC_BUILTIN

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
	rootname="`dirname $file`/`basename $file .nl`"
	tmp=/tmp/mnc$$
	trap 'rm -f $tmp.nl $tmp.ns $tmp.no; exit 1' 1 2 3 13 15
	cat $nc_builtin_nl > $tmp.nl
	if [ -f "$rootname.pp" ]; then
		echo "mnc: compiling \`$rootname.pp'"
		sed -e '/^#if *NU_PROLOG/s/.*//' -e '/^#endif/s/.*//' \
			"$rootname.pp" >> $tmp.nl
	else
		echo "mnc: compiling \`$file'"
		cat $file >> $tmp.nl
	fi
	nc -c $options $tmp.nl
	rm $tmp.nl $tmp.ns
	mv $tmp.no ${target:-"$rootname.no"}
done
