#!/bin/sh

LIBDIR=${MERCURY_CLIB_DIR:-@LIBDIR@/lib}

GRADE=none
case "$1" in
	-s*)
		GRADE=` expr $1 : '-s\(.*\)' `
		shift;;
esac

LIBDIR_OPTS="-L$LIBDIR/$GRADE/@FULLARCH@ -lmer"

case "`hostname`" in
	cadillac.dd.citri.edu.au)
		GCC=/usr/local/bin/gcc ;;
	kryten.cs.mu.oz.au)
		GCC=/usr/local/bin/gcc ;;
	munta.cs.mu.oz.au)
		GCC=/usr/local/gcc/bin/gcc ;;
	*)
		GCC=gcc
esac

exec $GCC "$@" $LIBDIR_OPTS
