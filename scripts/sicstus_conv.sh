#!/bin/sh
#---------------------------------------------------------------------------#
# Copyright (C) 1995 University of Melbourne.
# This file may only be copied under the terms of the GNU General
# Public License - see the file COPYING in the Mercury distribution.
#---------------------------------------------------------------------------#
#
# This sed script is used to convert from Mercury to Sicstus Prolog
# It does three things: delete the `:- module' line,
# expand backslash escapes, and replace the use
# of `^' for xor in calls to is/2 with `#'.

for file in "$@"; do
	case $file in
		*.m) base=`basename $file .m` ;;
		*.nl) base=`basename $file .nl` ;;
		*) base=`basename $file` ;;
	esac
	sed -e '
		/ is /s/\^/#/g
		/^:- *module/d
		/^[ 	]*%/s/.*//
		/\\\\/s//\\/g
		/\\a/s///g
		/\\b/s///g
		/\\r/s///g
		/\\f/s///g
		/\\t/s//	/g
		/\\n/s//\
		/g
		/\\v/s///g
		' $file > $base.pl
done
