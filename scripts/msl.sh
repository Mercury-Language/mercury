#!/bin/sh
#---------------------------------------------------------------------------#
# Copyright (C) 1995 University of Melbourne.
# This file may only be copied under the terms of the GNU General
# Public License - see the file COPYING in the Mercury distribution.
#---------------------------------------------------------------------------#

# msl - Mercury SICStus Prolog Linker.
#
# Loads SICStus object files (*.ql) together with the Mercury library
# into the SICStus interpreter, and then saves the state to produce an
# executable binary.
#
# Usage: msl [<options>] files...
# Options:
#	-v, --verbose
#	-d, --debug
#	-o, --output
# Environment variables: MERCURY_SP_LIB_DIR, MERCURY_SP_LIB_OBJS

SP=${MERCURY_SICSTUS_PROLOG=@LIBDIR@/sicstus/@FULLARCH@/library.sicstus.debug}
SPLIBDIR=${MERCURY_SP_LIB_DIR=@LIBDIR@/sicstus/@FULLARCH@}
LIBRARY_OBJS=${MERCURY_SP_LIB_OBJS=`cd $SPLIBDIR; echo *.ql`}

verbose=false
debug=false
save_temp=false
target=a.out

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
		-o|--output)
			target=$2
			shift 2
			;;
		-s|--save-temp)
			save_temp=true
			shift
			;;
		-*)	
			echo "$0: invalid option \`$1'" 1>&2
			exit 1
			;;
		*)	break 2
			;;
	esac
done

objlist=
for obj in $LIBRARY_OBJS; do
	if [ "$obj" = "sp_lib.ql" ]; then
		sp_lib=$SPLIBDIR/$obj
	elif echo "" "$objlist" "$@" "" | grep " $obj " > /dev/null; then
		true
	else
		objlist="$objlist $SPLIBDIR/$obj"
	fi
done

if $verbose; then
	echo Linking $objlist "$@" $sp_lib
fi
if $save_temp; then
	tmp=msl.tmp
else
	tmp=/tmp/msl$$
	trap 'rm -f $tmp; exit 1' 1 2 3 13 15 
	trap 'rm -f $tmp; exit 0' 0
fi
if $debug; then
	cat > $tmp << EOF
	assert((mercury_do_save :-
		on_exception(Error, (
		  prolog_flag(compiling, _, fastcode),
		  unix(argv(Files)), load(Files),
		  abolish(mercury_do_save),
		  save('$target', _),
		  version
		), (print_message(error, Error), halt)))).

	mercury_do_save.

EOF
	$SP $objlist "$@" $sp_lib < $tmp 2>&1 
else
	cat > $tmp << EOF
	on_exception(Error, (
	  prolog_flag(compiling, _, fastcode),
	  unix(argv(Files)), load(Files),
	  garbage_collect,
	  save('$target', 1),
	  unix(argv(Args)), run([mc|Args])
	), print_message(error, Error)), halt ; halt.
	e
	e
	e
	e
EOF
	$SP $objlist "$@" $sp_lib < $tmp 2>&1 
fi # grep -v 'DOMAIN ERROR.*when_condition' 
# We pipe the output through grep -v to suppress some spurious warnings
# caused by the NU-Prolog when declarations not matching Sicstus syntax.
