#!/bin/sh

# MGNUC - Mercury GNU C
#
# Usage: mgnuc [-v|--verbose] [-s<grade>] [<gcc options>] files...
#
# -v, --verbose
#	Echo gcc command before executing it.
# -s<grade>
#	Select optimization/debug options according to <grade>, which
#	must be one of debug, none, jump, reg, or fast
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

CHECKOPTS="-ansi
      -Wall -Wwrite-strings -Wpointer-arith -Wcast-qual -Wtraditional -Wshadow
      -Wstrict-prototypes -Wmissing-prototypes"

case "$1" in
	-v|--verbose)
		verbose=true
		shift;;
	*)
		verbose=false
		;;
esac

case "$1" in
	-sfast)
		GRADEOPTS="-O2 -fomit-frame-pointer -DSPEED
			-DUSE_GCC_GLOBAL_REGISTERS -DUSE_GCC_NONLOCAL_GOTOS"
		shift;;
	-sreg)
		GRADEOPTS="-O2 -fomit-frame-pointer -DSPEED
			-DUSE_GCC_GLOBAL_REGISTERS"
		shift;;
	-sjump)
		GRADEOPTS="-O2 -fomit-frame-pointer -DSPEED
			-DUSE_GCC_NONLOCAL_GOTOS"
		shift;;
	-snone)
		GRADEOPTS="-O2 -fomit-frame-pointer -DSPEED"
		shift;;
	-sinit)
		GRADEOPTS="-O2 -fomit-frame-pointer -DSPEED"
		shift;;
	-sdebug)
		GRADEOPTS="-g -Wno-uninitialized"
		shift;;
	*)
		GRADEOPTS="";;
esac

HOSTOPTS=""
case "`hostname`" in
	cadillac.*)
		GCC=/usr/local/bin/gcc ;;
	kryten.*)
		GCC=/usr/local/contrib/bin/gcc
		HOSTOPTS="-msupersparc" ;;
	munta.*)
		GCC=/usr/local/gcc/bin/gcc ;;
	*)
		GCC=gcc
esac

if [ "`uname -r -s`" = "SunOS 4.1.2" ]; then
	# the header files on cadillac are stuffed, so don't
	# enable any warnings
	CHECKOPTS=
fi
if $verbose; then
	echo $GCC -I $C_INCL_DIR $HOSTOPTS $CHECKOPTS $GRADEOPTS "$@"
fi
exec $GCC -I $C_INCL_DIR $HOSTOPTS $CHECKOPTS $GRADEOPTS "$@"
