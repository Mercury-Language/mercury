#!/bin/sh
#---------------------------------------------------------------------------#
# Copyright (C) 1995 University of Melbourne.
# This file may only be copied under the terms of the GNU General
# Public License - see the file COPYING in the Mercury distribution.
#---------------------------------------------------------------------------#
#
# mint - Mercury interface browser.
#
# Usage: mint module-name ...

INTDIR=${MERCURY_INT_DIR=@LIBDIR@/ints}

exit_status=0

if [  $# -lt 1 ]
then
	echo "usage: `basename $0` module-name ..." 1>&2
	exit 1
fi

for arg in "$@"; do
	module="`basename $arg .nl`"
	file="$INTDIR/$module.int"
	if [ -r "$file" ]; then
		${PAGER=more} "$file"
	else
		echo "`basename $0`: no interface file for \`$module'." 1>&2
		exit_status=1
	fi
done
exit $exit_status
