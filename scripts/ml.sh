#!/bin/sh

LIBDIR=${MERCURY_CLIB_DIR:-@LIBDIR@/lib}

GRADE=none
case "$1" in
	-s*)
		GRADE=` expr $1 : '-s\(.*\)' `
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

exec $GCC "$@" $LIBDIR_OPTS
