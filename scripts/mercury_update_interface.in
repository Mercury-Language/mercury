#!/bin/sh
#---------------------------------------------------------------------------#
# Copyright (C) 1995 University of Melbourne.
# This file may only be copied under the terms of the GNU General
# Public License - see the file COPYING in the Mercury distribution.
#---------------------------------------------------------------------------#
#
# MERCURY_UPDATE_INTERFACE
#
# usage:
#	mercury_update_interface [-v] filename
#
# Moves `filename.tmp' to `filename', but only if necessary:
# if they are identical, then `filename.tmp' is simply removed,
# and the time-stamp is left unaltered.
#
# If the `-v' (verbose) option is specified, then appropriate progress messages
# will be printed.
#
# Enviroment variables: none.

PATH=/bin:/usr/bin

verbose=true

if [ $# -ge 1 ] && [ "$1" = "-v" ]; then
	verbose=true
	shift
fi

if [ $# -ne 1 ]; then
	echo "Usage: `basename $0` filename" 1>&2
	exit 1
fi

filename="$1"
	
if	[ ! -f "$filename" ]
then
	$verbose && echo "creating \`$filename'." 1>&2
	mv -f "$filename.tmp" "$filename"
elif 	cmp -s "$filename.tmp" "$filename"
then
	$verbose && echo "\`$filename' has not changed." 1>&2
	rm -f "$filename.tmp"
else
	$verbose && echo "\`$filename' has changed." 1>&2
	mv -f "$filename.tmp" "$filename"
fi
