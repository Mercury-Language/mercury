#!/bin/sh
#---------------------------------------------------------------------------#
# Copyright (C) 1995 University of Melbourne.
# This file may only be copied under the terms of the GNU General
# Public License - see the file COPYING in the Mercury distribution.
#---------------------------------------------------------------------------#
#
# mmake - Mercury Make.
#
#	Type mmake -h for help.
#
#-----------------------------------------------------------------------------#

MAKE=${MMAKE_MAKE=make}
MMAKE_VARS=${MMAKE_VARS=@LIBDIR@/mmake/Mmake.vars}
MMAKE_RULES=${MMAKE_RULES=@LIBDIR@/mmake/Mmake.rules}
MERCURY_INT_DIR=${MERCURY_INT_DIR=@LIBDIR@/ints}

MMAKE=$0
verbose=false
save_makefile=false

while [ $# -gt 0 ]; do
	case $1 in
		-h|--help)
#-----------------------------------------------------------------------------#
			cat << 'EOF'
Usage: mmake [<mmake options>] [-- <make options>] <target>...
Options:
	-s, --save-makefile:
		Save the generated makefile to `Mmake.makefile'.
	-v, --verbose:
		Print verbose progress messages.
	-h, --help:
		Print this usage message.
Targets:
	<module>.depend:
		Make the file `<module>.dep'.  This step is required
		in preparation for the targets below.
	<module>:
		Compile and link a Mercury program with main module
		`<module>.nl' to produce an executable.
	<module>.nu:
		Compile and link a Mercury program with NU-Prolog
		rather than with the Mercury compiler.
	clean:
		Remove intermediate files.
	realclean:
		Remove all automatically-generated files: intermediate files,
		dependency files, and executables.
EOF
#-----------------------------------------------------------------------------#
			exit
			;;
		-s|--save-makefile)
			save_makefile=true
			MMAKE="$MMAKE $1"
			shift
			;;
		-v|--verbose)
			verbose=true
			MMAKE="$MMAKE $1"
			shift
			;;
		--)	
			MMAKE="$MMAKE $1"
			shift
			break
			;;
		*)
			break
			;;
	esac
done

if [ -f Mmake ]; then
	mmake="Mmake"
else
	mmake=""
fi
if [ "`echo *.dep`" = "*.dep" ]; then
	deps=""
else
	deps="*.dep"
fi
if [ "`echo *.d`" = "*.d" ]; then
	ds=""
else
	ds="*.d"
fi

if $save_makefile; then
	tmp=Mmake.makefile
else
	tmp=/tmp/mmake.$$
	trap 'status=$?; rm -f $tmp; exit $status' 0 1 2 3 13 15
fi

if $verbose; then
	echo MMAKE=$MMAKE
	echo export MMAKE
	echo MERCURY_INT_DIR=$MERCURY_INT_DIR
	echo export MERCURY_INT_DIR
	echo cat ${MMAKE_VARS} $deps $ds $mmake ${MMAKE_RULES} ">" $tmp
	echo ${MAKE} -f $tmp "$@"
fi
export MMAKE
export MERCURY_INT_DIR
cat ${MMAKE_VARS} $deps $ds $mmake ${MMAKE_RULES} > $tmp
${MAKE} -f $tmp -r "$@"
