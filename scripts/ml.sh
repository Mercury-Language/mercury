#!/bin/sh
#
# ML - Mercury Linker.
#
# Invokes GCC with the appropriate options to link in the Mercury library.
#
# Usage: ml [<ml options>] [<gcc options] files...
# Options:
# 	-v, --verbose
#		Echo gcc command line before executing it
#	--shared
#		Link with the shared version of the Mercury library
#	--static (the default)
#		Link with the static version of the Mercury library
#	-s <grade>
#		Specify which grade of the Mercury library to link with
#
# Environment variables: MERCURY_C_LIB_DIR

LIBDIR=${MERCURY_C_LIB_DIR:-@LIBDIR@/lib}
verbose=false
shared=false
GRADE=asm_fast.gc

while true; do
    case "$1" in
	-v|--verbose)
		verbose=true
		shift ;;
	--shared)
		shared=true
		shift ;;
	--static)
		shared=false
		shift ;;
	-s)
		shift
		GRADE="$1"
		shift ;;
	-s*)
		GRADE="` expr $1 : '-s\(.*\)' `"
		shift ;;
	*)
		break
		;;
    esac
done

case "$GRADE" in
	*.gc)
		LIBGC=$LIBDIR/gc.a
		;;
	*)
		LIBGC=
		;;
esac

if $shared; then
	LIBMER="libmer*.so"
else
	LIBMER="libmer*.a"
fi

LIBDIR_OPTS="$LIBDIR/$GRADE/@FULLARCH@/$LIBMER"

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
	echo $GCC "$@" $LIBDIR_OPTS $LIBGC
fi
exec $GCC "$@" $LIBDIR_OPTS $LIBGC
