#!/bin/sh
#---------------------------------------------------------------------------#
# Copyright (C) 1995 University of Melbourne.
# This file may only be copied under the terms of the GNU General
# Public License - see the file COPYING in the Mercury distribution.
#---------------------------------------------------------------------------#

# MSC - Mercury SICStus Compiler.
#
# Compiles Mercury programs to SICStus Prolog object code (*.ql).
#
# Use `msc -h' for help.

sp_builtin_pl=${MERCURY_SP_BUILTIN=@LIBDIR@/sicstus/sp_builtin.pl}
sicstus_compile=\
${MERCURY_SICSTUS_COMPILER=@LIBDIR@/sicstus/@FULLARCH@/sicstus_compile}

help=false
compile_mode=fastcode
unset target

while true; do
	case $1 in
		-h|--help)
			help=true
			break
			;;
		-o|--output)
			target=$2
			shift 2
			;;
		-m|--mode)
			compile_mode=$2
			shift 2
			;;
		--)
			shift
			break
			;;
		-*)
			echo "$0: unrecognized option \`$1'" 1>&2
			exit 1
			;;
		*)
			break
			;;
	esac
done
			
if [ $# -lt 1 ] || $help; then
	cat << 'EOF'
MSC - Mercury SICStus Compiler.
Compiles Mercury programs to SICStus Prolog object code (*.ql).
Usage: msc [<options>] file(s)
Options:
	-h, --help
		Print this help message
	-o <target>, --output <target>
		Name the output file <target>.
	-m <compile-mode>, --mode <compile-mode>
		Use the specified mode of compilation.
		<compile-mode> should be either compactcode, fastcode,
		or profiledcode.
Environment variables: MERCURY_SP_BUILTIN, MERCURY_SICSTUS_COMPILER
EOF
	exit 0
fi

for file in "$@"; do
	echo "msc: compiling \`$file'"
	dir="`dirname $file`"
	case $file in
		*.m)	base="`basename $file .m`" ;;
		*.nl)	base="`basename $file .nl`" ;;
		*)	base="`basename $file`" ;;
	esac
	rootname="$dir/$base"
	tmp=/tmp/msc$$
	trap 'rm -f $tmp.pl $tmp.ql; exit 1' 1 2 3 13 15

# This sed script is used to convert from Mercury/NU-Prolog to Sicstus Prolog
# It does three things: delete the `:- module' line,
# expand backslash escapes, and replace the use
# of `^' for xor in calls to is/2 with `#'.
# It also removes '%' comments, to avoid problems with quotes in them.
# Obviously this is not the most robust method of translation imaginable!

	sed '
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
	' $file > $tmp.pl
	$sicstus_compile $compile_mode $tmp.pl
	rm $tmp.pl 
	mv $tmp.ql ${target="$rootname.ql"}
done
