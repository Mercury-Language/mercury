#!/bin/sh
#
# ML - Mercury Linker.
#
# Invokes GCC with the appropriate options to link in the Mercury library.
#
# Usage: ml [-v|--verbose] [-s <grade>] [<gcc options>] files...
#
# Environment variables: MERCURY_C_LIB_DIR

LIBDIR=${MERCURY_C_LIB_DIR:-@LIBDIR@/lib}

case "$1" in
	-v|--verbose)
		verbose=true
		shift;;
	*)
		verbose=false
		;;
esac

GRADE=none
case "$1" in
	-s)
		shift
		GRADE="$1"
		shift ;;
	-s*)
		GRADE="` expr $1 : '-s\(.*\)' `"
		shift ;;
esac

# The following will pick up both the .a and the .so
# if they both exist (i.e. on systems which support shared libraries).
# Otherwise it will just pick up the .a file.

LIBDIR_OPTS="$LIBDIR/$GRADE/@FULLARCH@/libmer.*"

case "`hostname`" in
	cadillac.dd.citri.edu.au)
		GCC=/usr/local/bin/gcc ;;
	kryten.cs.mu.OZ.AU)
		GCC=/usr/local/bin/gcc ;;
	kryten.cs.mu.OZ.AU)
		GCC=/usr/local/gcc/bin/gcc ;;
	*)
		GCC=${GCC:-gcc}
esac

if $verbose; then
	echo $GCC "$@" $LIBDIR_OPTS
fi
exec $GCC "$@" $LIBDIR_OPTS
