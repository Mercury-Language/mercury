#!/bin/sh

#-----------------------------------------------------------------------------#
#
# mmake - Mercury Make.
#
#-----------------------------------------------------------------------------#

MAKE=${MAKE:-make}
MERCURY_MMAKE_VARS=${MERCURY_MAKEFILE:-@LIBDIR@/mmake/Mmake.vars}
MERCURY_MMAKE_RULES=${MERCURY_MAKEFILE2:-@LIBDIR@/mmake/Mmake.rules}

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
VPATH=${MERCURY_INT_DIR:-@LIBDIR@/ints}:$VPATH
export VPATH
tmp=/tmp/mmake.$$
trap 'rm -f $tmp' 0 1 2 3 13 15
cat ${MERCURY_MMAKE_VARS} $mmake ${MERCURY_MMAKE_RULES} $deps $ds > $tmp
${MAKE} -f $tmp "$@"
