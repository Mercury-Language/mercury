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
#	-s <grade>
#		Specify which grade of the Mercury library to link with.
#		Defaults to `asm_fast.gc'.
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
		LIBGC="-lgc"
		;;
	*)
		LIBGC=
		;;
esac

LIBDIR_OPTS="
-R @LIBDIR@/lib/@FULLARCH@ -L $LIBDIR/@FULLARCH@
-R @LIBDIR@/lib/$GRADE/@FULLARCH@ -L $LIBDIR/$GRADE/@FULLARCH@
"

case "`hostname`" in
	cadillac.dd.citri.edu.au)
		GCC=/usr/local/bin/gcc ;;
	kryten.cs.mu.OZ.AU)
		GCC=/usr/local/bin/gcc ;;
	*)
		GCC=${GCC:-gcc}
esac

if $verbose; then
	exec $GCC "$@" $LIBDIR_OPTS -lmer -lmercury $LIBGC
fi
exec $GCC "$@" $LIBDIR_OPTS -lmer -lmercury $LIBGC
