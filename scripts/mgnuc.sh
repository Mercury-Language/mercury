#!/bin/sh

# MGNUC - Mercury GNU C
#
# Usage: mgnuc [-v|--verbose] [-s<grade>] [<gcc options>] files...
#
# -v, --verbose
#	Echo gcc command before executing it.
# -s<grade>
#	Select optimization/debug/gc options according to <grade>, which
#	must be one of debug, none, jump, asm_jump, reg, fast, or asm_fast,
#	or one of those with .gc appended.
#
# This runs gcc with all warnings enabled, except for the following
# exceptions:
#
# -Wredundant-decls	causes too many complaints in system header files
# -Wconversion		really only intended to help people using `unprotoize'
# -Waggregate-return	not useful, IMHO
#
# -Wcast-align 		causes redundant warnings in memory.c
# -pedantic		causes unsuppressable warnings about LVALUE_CAST()
# -Wnested-externs	causes unsuppressable warnings about callentry()
# -Wid-clash-31 	causes warnings about entry_mercury__xxx ...
# -Wenum-clash 		is for C++ only

C_INCL_DIR=${MERCURY_C_INCL_DIR:-@LIBDIR@/inc}

CHECK_OPTS="-ansi
      -Wall -Wwrite-strings -Wpointer-arith -Wcast-qual -Wtraditional -Wshadow
      -Wstrict-prototypes -Wmissing-prototypes"
OPT_OPTS="-O2 -fomit-frame-pointer -DSPEED"
DEBUG_OPTS="-g"

grade=fast
verbose=false

while true; do
    case "$1" in
	-v|--verbose)
		verbose=true
		shift
		;;
	-s)
		shift
		grade="$1";
		shift
		;;
	-s*)
		grade="` expr $1 : '-s\(.*\)' `"
		shift
		;;
	--)
		shift
		break
		;;
	*)
		break
		;;
    esac
done

case "$grade" in
	*.gc)	GC_OPTS="-DCONSERVATIVE_GC -DTAGBITS=0"
		grade="` expr $grade : '\(.*\).gc' `"
		;;
	*)
		GC_OPTS=""
		;;
esac

case "$grade" in
	asm_fast)
		GRADE_OPTS="$OPT_OPTS -DUSE_GCC_GLOBAL_REGISTERS
				-DUSE_ASM_LABELS -DUSE_GCC_NONLOCAL_GOTOS"
		;;
	fast)
		GRADE_OPTS="$OPT_OPTS
			-DUSE_GCC_GLOBAL_REGISTERS -DUSE_GCC_NONLOCAL_GOTOS"
		;;
	reg)
		GRADE_OPTS="$OPT_OPTS
			-DUSE_GCC_GLOBAL_REGISTERS"
		;;
	asm_jump)
		GRADE_OPTS="$OPT_OPTS
			-DUSE_ASM_LABELS -DUSE_GCC_NONLOCAL_GOTOS"
		;;
	jump)
		GRADE_OPTS="$OPT_OPTS
			-DUSE_GCC_NONLOCAL_GOTOS"
		;;
	none)
		GRADE_OPTS="$OPT_OPTS"
		;;
	init)
		GRADE_OPTS="$OPT_OPTS"
		;;
	debug)
		GRADE_OPTS="$DEBUG_OPTS"
		;;
	*)
		echo "$0: invalid grade \`$grade'" 1>&2;
		exit 1
esac

HOST_OPTS=""
case "`hostname`" in
	cadillac.*)
		GCC=/usr/local/bin/gcc ;;
	kryten.*)
		GCC=/usr/local/contrib/bin/gcc
		HOST_OPTS="-msupersparc" ;;
	munta.*)
		GCC=/usr/local/gcc/bin/gcc ;;
	*)
		GCC=gcc
esac

if [ "`uname -r -s`" = "SunOS 4.1.2" ]; then
	# the header files on cadillac are stuffed, so don't
	# enable any warnings
	CHECK_OPTS=
fi
if $verbose; then
	echo $GCC-I $C_INCL_DIR $HOST_OPTS $CHECK_OPTS $GRADE_OPTS $GC_OPTS \
		"$@"
fi
exec $GCC -I $C_INCL_DIR $HOST_OPTS $CHECK_OPTS $GRADE_OPTS $GC_OPTS "$@"
