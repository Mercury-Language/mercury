#!/bin/sh

# mnc - Mercury NU-Prolog Compiler.
#
# Compiles Mercury programs to NU-Prolog object code (*.no).

nc_builtin=@LIBDIR@/nuprolog/nc_builtin

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

for file in "$@"; do
	echo "mnc: compiling $file"
	tmp=/tmp/mnc$$
	cat $nc_builtin.nl $file > $tmp.nl
	nc -c $options $tmp.nl
	rm $tmp.ns
	mv $tmp.no `dirname $file`/`basename $file .nl`.no
done
