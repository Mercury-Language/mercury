#!/bin/sh
#---------------------------------------------------------------------------#
# Copyright (C) 1995 University of Melbourne.
# This file may only be copied under the terms of the GNU General
# Public License - see the file COPYING in the Mercury distribution.
#---------------------------------------------------------------------------#

# MNC - Mercury NU-Prolog Compiler.
#
# Compiles Mercury programs to NU-Prolog object code (*.no).
#
# Usage: same as for `nc'.
#
# Environment variables: MERCURY_NC_BUILTIN

nc_builtin_nl=${MERCURY_NC_BUILTIN=@LIBDIR@/nuprolog/nc_builtin.nl}

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
	dir="`dirname $file`"
	case $file in
		*.m)	base="`basename $file .m`" ;;
		*.nl)	base="`basename $file .nl`" ;;
		*)	base="`basename $file`" ;;
	esac
	rootname="$dir/$base"
	tmp=/tmp/mnc$$
	trap 'rm -f $tmp.nl $tmp.ns $tmp.no; exit 1' 1 2 3 13 15
	cat $nc_builtin_nl > $tmp.nl
	# as a special-case hack, if there is a .pp file we use it instead,
	# after preprocessing away any #if NU_PROLOG commands in it
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
	mv $tmp.no ${target="$rootname.no"}
done
