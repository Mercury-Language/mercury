#-----------------------------------------------------------------------------#
# vim: ts=4 sw=4 expandtab
#-----------------------------------------------------------------------------#
# Copyright (C) 1995-2012 The University of Melbourne.
# Copyright (C) 2013-2018 The Mercury team.
# This file may only be copied under the terms of the GNU General
# Public Licence - see the file COPYING in the Mercury distribution.
#-----------------------------------------------------------------------------#
#
# configure.in: autoconf source code for `configure' script.
#
# This file is automatically processed by `autoconf' to produce the
# `configure' script.
#
#-----------------------------------------------------------------------------#
#
# Define a macro MERCURY_MSG, similar to AC_MSG_RESULT, for displaying output.
#
dnl MERCURY_MSG(MESSAGE)
define(MERCURY_MSG,
[echo $1 1>&AC_FD_MSG])
#-----------------------------------------------------------------------------#
AC_INIT(scripts/mmc.in)
TMPDIR=${TMPDIR=/tmp}

# AC_CHECK_TYPES does not exist in versions of autoconf before 2.50.
# AC_CHECK_TYPE is broken in versions of autoconf before 2.50.
# Version 7 of the Boehm gc requires at least version 2.58.
AC_PREREQ(2.58)
#-----------------------------------------------------------------------------#
#
# Ensure that all messages are saved to a file `configure.log' by
# invoking ourself recursively without output piped into `tee configure.log'.
# The environment variable `SUPPRESS_LOG_FILE_RECURSION' is used to prevent
# infinite recursion.
#

# XXX Currently this is commented out because autoconf 2.60 breaks
# how this works.
#if test "$SUPPRESS_LOG_FILE_RECURSION" != "yes"; then
#   SUPPRESS_LOG_FILE_RECURSION=yes
#   export SUPPRESS_LOG_FILE_RECURSION
#   trap 0 1 2 3 13 15
#   rm -f configure.exit_status
#   { case $# in
#       0) ${CONFIG_SHELL-/bin/sh} "$0"      ;;
#       *) ${CONFIG_SHELL-/bin/sh} "$0" "$@" ;;
#   esac; echo $? > configure.exit_status; } | exec tee configure.log
#   status=`cat configure.exit_status`
#   rm -f configure.exit_status
#   exit $status
#fi

#-----------------------------------------------------------------------------#
# Work out the default arguments to configure when reconfiguring,
# for example when installing a binary distribution.
# Strip out --no-create and --no-recursion added by config.status.
# Remove any --prefix and --enable-reconfigure arguments passed by
# mercury_config.
# Strip out any site or system specific configuration files since their
# presence when reconfiguring will result in the command line used to invoke
# configure being invalid.

# Also quote any args containing shell metacharacters.
#
# NOTE: Some macros, e.g. AC_CHECK_PROG, overwrite the argument list,
# so this must come before any uses of those macros.

prefix_arg=false
RECONFIGURE_ARGS=
for arg
do
    if test "$prefix_arg" = true
    then
        prefix_arg=false
    else
        case "$arg" in
            --no-create | --no-recursion)
                ;;
            --prefix)
                prefix_arg=true
                ;;
            --prefix=*)
                ;;
            --enable-reconfigure=*)
                ;;
            *config.site)
                ;;
            *" "*|*"    "*|*[[\[\]\~\#\$\^\&\*\(\)\{\}\\\|\;\<\>\?]]*)
                # The argument needs to be quoted.
                RECONFIGURE_ARGS="$RECONFIGURE_ARGS '$arg'"
                ;;
            *)
                RECONFIGURE_ARGS="$RECONFIGURE_ARGS $arg"
                ;;
       esac
    fi
done
AC_SUBST(RECONFIGURE_ARGS)

#-----------------------------------------------------------------------------#

AC_CONFIG_HEADER(runtime/mercury_conf.h)
AC_CONFIG_HEADER(robdd/robdd_conf.h)
DEFAULT_PREFIX="/usr/local/mercury-`. ./VERSION; echo $VERSION`"
AC_PREFIX_DEFAULT(/usr/local/mercury-`. ./VERSION; echo $VERSION`)

#-----------------------------------------------------------------------------#
#
# Let the user specify which C compiler to use.
#

AC_ARG_WITH(cc,
    AC_HELP_STRING([--with-cc=<program>], [Specify which C compiler to use.]),
    mercury_cv_with_cc="$withval", mercury_cv_with_cc="")

case "$mercury_cv_with_cc" in
    yes)
        AC_MSG_ERROR(missing argument to --with-cc=... option)
        exit 1
        ;;
    no)
        AC_MSG_ERROR(invalid option --without-cc)
        exit 1
        ;;
    "")
        ;;
    *)
        CC="$mercury_cv_with_cc"
        ;;
esac

#-----------------------------------------------------------------------------#
#
# Determine what C compiler to use
#

# The code in config.guess that distinguishes between the i*86 and x86_64
# versions of Darwin needs to use the C compiler to make that distinction.
# In particular, if we wish to build a 32-bit Mercury installation on
# x86_64 machine, then we need to tell configure that we are using the
# 32-bit C compiler. If we didn't call the AC_PROG_CC macro at this point
# then configure's --with-cc option would not do that.

AC_PROG_CC
AC_SUBST([CC])

# Ensure that AC_CANONICAL_HOST uses the value of CC we just determined.
#
export CC

# NOTE: AC_PROG_CC will set the variable GCC if it thinks the C compiler is
# GCC. However, it also sets it if the C compiler is clang -- because they
# both define __GNUC__ -- do *NOT* use the GCC variable to check the C compiler
# type, instead use the value of C_COMPILER_TYPE (defined below).
#
MERCURY_CC_TYPE

MERCURY_HAVE_CLANG=no
MERCURY_HAVE_GCC=no
MERCURY_HAVE_MSVC=no

case "$mercury_cv_cc_type" in
    clang)
        MERCURY_HAVE_CLANG=yes
        MERCURY_CLANG_VERSION
        C_COMPILER_TYPE="clang_${mercury_cv_clang_version}"
    ;;

    gcc)
        MERCURY_HAVE_GCC=yes
        MERCURY_GCC_VERSION
        C_COMPILER_TYPE="gcc_${mercury_cv_gcc_version}"
    ;;

    msvc)
        MERCURY_HAVE_MSVC=yes
        MERCURY_MSVC_VERSION
        C_COMPILER_TYPE="msvc_${mercury_cv_msvc_version}"
    ;;

    *)
        C_COMPILER_TYPE="unknown"
    ;;
esac

#-----------------------------------------------------------------------------#

AC_CANONICAL_HOST
FULLARCH="$host"
AC_SUBST(FULLARCH)
AC_DEFINE_UNQUOTED(MR_FULLARCH, "$FULLARCH")
. ./VERSION
AC_DEFINE_UNQUOTED(MR_VERSION, "$VERSION")
if test "$prefix" = "NONE"; then
    PREFIX="$ac_default_prefix"
    if test -d "$PREFIX"
    then
        INSTALLABLE_PREFIX=yes
    else
        INSTALLABLE_PREFIX=no
    fi
else
    PREFIX="$prefix"
    # the directory may be created later
    INSTALLABLE_PREFIX=yes
fi
AC_SUBST(INSTALLABLE_PREFIX)
case "$PREFIX" in
    *\ *)
        AC_MSG_ERROR(
[cannot install into a directory with a name containing
    spaces: \`$PREFIX.'])
        exit 1
        ;;
esac

#-----------------------------------------------------------------------------#

case "$cross_compiling" in
    yes)
        CROSS_COMPILING=yes
        BUILD_C_PROGS_FOR_BUILD_SYSTEM=cc
        HOST_TOOL_PREFIX="$host-"
        ;;
    *)
        CROSS_COMPILING=no
        BUILD_C_PROGS_FOR_BUILD_SYSTEM="$CC"
        HOST_TOOL_PREFIX=
        ;;
esac

AC_SUBST(CROSS_COMPILING)
AC_SUBST(BUILD_C_PROGS_FOR_BUILD_SYSTEM)

#-----------------------------------------------------------------------------#

# Check that we are not using llvm-gcc on Mac OS X since it is too broken
# to successfully compile Mercury.

if test "$MERCURY_HAVE_GCC" = yes
then
    case "$host" in
        *apple*darwin*)
            if $CC -v 2>&1 | grep -q "gcc.*Apple.*LLVM"
            then
                AC_MSG_ERROR(
[
**** The executable `$CC' on this system appears to be llvm-gcc.
**** It contains bugs that means it cannot be used to compile Mercury.
**** See README.MacOS for further details.])
            exit 1
            fi
        ;;
    esac
fi

#-----------------------------------------------------------------------------#

# Using `cygpath -m' gives a path that works under both Cygwin and native
# Windows, without causing quoting problems in shell scripts.
# (`cygpath -m' converts the path to Windows format, with '/' as the directory
# separator).
AC_CHECK_PROG(CYGPATH, cygpath, cygpath -m, echo)
AC_CHECK_PROG(CYGPATHU, cygpath, cygpath -u, echo)

AC_SUBST(CYGPATHU)

PREFIX="`$CYGPATH $PREFIX`"
LIBDIR="`$CYGPATH $PREFIX/lib/mercury`"
NONSHARED_LIB_DIR=${MERCURY_NONSHARED_LIB_DIR=$PREFIX/lib/nonshared}
AC_SUBST(VERSION)
AC_SUBST(PREFIX)
AC_SUBST(NONSHARED_LIB_DIR)
AC_SUBST(LIBDIR)
DEFAULT_MERCURY_DEBUGGER_INIT_DIR=$LIBDIR/mdb
AC_SUBST(DEFAULT_MERCURY_DEBUGGER_INIT_DIR)
DEFAULT_MERCURY_DEBUGGER_DOC=$DEFAULT_MERCURY_DEBUGGER_INIT_DIR/mdb_doc
AC_SUBST(DEFAULT_MERCURY_DEBUGGER_DOC)

#-----------------------------------------------------------------------------#
# MMC_INVOKE_CMD is the command used to invoke mercury_compile in scripts/mmc.
# On MSYS (MinGW) we cannot use exec, since exec will not find the file because
# it will expect a path relative to the MSYS home directory and will not work
# with a full Windows path. A full Windows path, however must be used for the
# prefix so that mercury_compile will find the standard library.
#
case "$host" in
    *mingw*)
        MMC_INVOKE_CMD=
        ;;
    *)  MMC_INVOKE_CMD=exec
        ;;
esac
AC_SUBST(MMC_INVOKE_CMD)
#-----------------------------------------------------------------------------#

# Don't document this -- it's only used by the mercury_config script
# to generate a new configuration for an already installed system.
# CONFIG_PREFIX is the PREFIX for a partial installation generated
# by mercury_config containing only the scripts and the configuration
# files, which uses the libraries and executables from a previous
# installation.
AC_ARG_ENABLE(reconfigure,
[],
[reconfiguring=yes
CONFIG_PREFIX="$enableval"
CONFIG_LIBDIR="$CONFIG_PREFIX/lib/mercury"],
[reconfiguring=no
CONFIG_PREFIX="$PREFIX"
CONFIG_LIBDIR="$LIBDIR"]
)

case "$CONFIG_PREFIX" in
    yes)
        AC_MSG_ERROR(missing argument to --enable-reconfigure=... option)
        exit 1
        ;;
    no)
        AC_MSG_ERROR(invalid option --no-enable-reconfigure)
        exit 1
        ;;
esac
CONFIG_PREFIX="`$CYGPATH $CONFIG_PREFIX`"
CONFIG_LIBDIR="`$CYGPATH $CONFIG_LIBDIR`"
AC_SUBST(CONFIG_PREFIX)
AC_SUBST(CONFIG_LIBDIR)
#-----------------------------------------------------------------------------#

case "$build" in
    i*86-*-solaris2.*)  link_static_opt= ;;
    *solaris2.10)       link_static_opt= ;;
    *apple*darwin*)     link_static_opt= ;;
    *linux*)            link_static_opt= ;;
    *cygwin*)           link_static_opt= ;;
    *)                  link_static_opt="--linkage static" ;;
esac

MERCURY_MSG("looking for an already installed Mercury compiler to bootstrap with...")
AC_PATH_PROG(BOOTSTRAP_MC,mmc)
# Ignore the Microsoft Management Console, which if executed will pop up with
# an annoying error message.
case $BOOTSTRAP_MC in
    *Windows*|*system32*|*WINNT*)
        BOOTSTRAP_MC=
        MERCURY_MSG("wrong mmc ignored")
        ;;
esac

if test "$BOOTSTRAP_MC" != ""
then
    AC_MSG_CHECKING(whether the above mmc works and is sufficiently recent)
    cat > conftest.m << EOF
        % This module contains features which did not work in
        % previous versions of the Mercury compiler.
        % We may also test to see if the installed compiler is
        % recent enough by checking what flags mmc takes.
        % Use [[ ]] for lists.
        :- module conftest.
        :- interface.
        :- import_module io.
        :- pred main(io__state::di, io__state::uo) is det.

        :- implementation.

        :- import_module bool, char, string, int, list, uint32, univ.

        :- mutable(global_var,
            int,
            561,
            ground,
            [[untrailed, attach_to_io_state, thread_local]]
        ).

        :- type abs_enum ---> yes ; no.
        :- type abs_enum where type_is_abstract_enum(1).

        main(!IO) :-
            return_rtti_version(Version),
            p(T),
            q(T, _),
            get_global_var(Global, !IO),
            (
                % If you change the minimum RTTI version, you also
                % need to update the Mercury clause for return_rtti_version
                % below.
                Version >= 8,
                ac(2) ^ elem(3, 5) = 17,
                (1 .. 2) = [[1, 2]],
                Global = 561
            ->
                trace [[io(!S)]] (
                    io.print("Hello, ", !S)
                ),
                promise_pure (
                    semipure io.unsafe_get_io_state(UIO0),
                    io.print("world\n", UIO0, UIO),
                    impure io.unsafe_set_io_state(UIO)
                )
            ;
                Words = string.words_separator(char.is_alpha, "xx yy"),
                io.write(Words, !IO)
            ).

        :- type t    ---> a ; b ; c(int).

        :- type bear ---> brown ; black ; teddy.
        :- pragma foreign_export_enum("Java", bear/0).

        :- pred check_int64(int64::out) is det.

        check_int64(561i64).
       
        :- pred my_uint_xor(uint32::in, uint32::out) is det.

        my_uint_xor(A, B) :-
            561u32 = uint32.xor(A, B).

        :- pred p(t::out) is det.

        p(b).

        :- pred q(t::in, int::out) is det.

        q(T, N) :-
            (
                T = a : t,
                N = 0 : int
            ;
                ( T = b ; T = c(_) ),
                N = 1
            ).

        :- type a    ---> ac(int).

        :- func a ^ elem(int, int) = int.
        :- mode in ^ elem(in,  in ) = out is det.

        ac(T) ^ elem(I, J) = T + I * J.

        :- pred return_rtti_version(int::out) is det.

        :- pragma foreign_decl("C", local, "
        typedef enum {
            enum1, enum2
        } Enum;
        ").

        :- type c_enum
            --->    c_enum_1
            ;       c_enum_2.

        :- pragma foreign_enum("C", c_enum/0, [[
            c_enum_1 - "enum1",
            c_enum_2 - "enum2"
        ]]).

        :- type x ---> x.
        :- pragma foreign_type("C", x, "MR_Integer",
            [[can_pass_as_mercury_type, stable]]).
        :- pragma foreign_type("Erlang", x, "").
        :- pragma foreign_type("C#", x, "object").

        :- pragma foreign_proc("C", return_rtti_version(Version::out),
            [[may_call_mercury, promise_pure, terminates,
              does_not_affect_liveness, will_not_modify_trail]],
        "
            Version = MR_RTTI_VERSION;
        ").

        % XXX the non-C backends do not currently define an equivalent
        % to MR_RTTI_VERSION.
        return_rtti_version(8).

        % The code below is the hard_coded/disjs_in_switch test case.
        % The compiler now depends on the installed compiler handling
        % code like this test case correctly.

        :- type tp
            --->    f(int)
            ;       g(int, int)
            ;       h(float)
            ;       i(string).

        :- pred p(tp::in, string::out) is det.

        p(T, U) :-
            (
                ( T = f(_)
                ; T = g(_, _)
                ),
                U = "f or g"
            ;
                ( T = h(_)
                ; T = i(_)
                ),
                U = "h or i"
            ).

        :- pred ext(int::out) is det.
        :- pragma external_pred(ext/1).

        :- type psv_t
            --->    psv_t1
            ;       psv_t2
            ;       psv_t3
            ;       psv_t4
            ;       psv_t5
            ;       psv_t6
            ;       psv_t7
            ;       psv_t8.

        :- type psv_u
            --->    psv_u1
            ;       psv_u2.

        :- pred preferred_switch_var(psv_t::in, psv_u::in, int::out)
            is semidet.

        preferred_switch_var(T, U, N) :-
            require_switch_arms_semidet [[T]]
            (
                T = psv_t1,
                U = psv_u1,
                N = 11
            ;
                T = psv_t2,
                U = psv_u2,
                N = 22
            ;
                T = psv_t3,
                U = psv_u1,
                N = 31
            ;
                T = psv_t4,
                U = psv_u2,
                N = 42
            ).
EOF

    # When this script has been invoked with --prefix=..., these variables
    # will have values by the time we get here, but the values will be the
    # ones appropriate to the system *we will install*, not the system that is
    # *already installed*, i.e. the bootstrap compiler. If we don't temporarily
    # unset these variables, the bootstrap test will fail because the Mercury
    # compiler won't find the (to-be-installed-later) .int files for the
    # standard library modules it needs.
    SAVE_MERCURY_CONFIG_DIR="${MERCURY_CONFIG_DIR}"
    SAVE_MERCURY_DEFAULT_GRADE="${MERCURY_DEFAULT_GRADE}"
    SAVE_MERCURY_STDLIB_DIR="${MERCURY_STDLIB_DIR}"
    unset MERCURY_CONFIG_DIR
    unset MERCURY_DEFAULT_GRADE
    unset MERCURY_STDLIB_DIR

    if
        echo $BOOTSTRAP_MC conftest >&AC_FD_CC 2>&1 &&
        $BOOTSTRAP_MC \
            --verbose \
            --halt-at-warn \
            $link_static_opt conftest \
            --erlang-native-code \
            --warn-insts-with-functors-without-type \
            --no-no-reserved-addrs \
            --no-ssdb \
            </dev/null >&AC_FD_CC 2>&1 &&
        test "`./conftest 2>&1 | tr -d '\015'`" = "Hello, world" &&
        # Test for the --record-term-sizes-as-words option.
        # Note that we need to set --grade to an LLDS grade
        # when doing this test because term size profiling
        # requires an LLDS grade.
        # This also tests for the --shlib-linker-install-name-flag,
        # --warn-known-bad-format-calls and
        # --warn-insts-without-matching-type options.
        $BOOTSTRAP_MC \
            --grade none.gc \
            --warn-insts-without-matching-type \
            --warn-known-bad-format-calls \
            --no-detect-libgrades \
            --record-term-sizes-as-words \
            --output-grade-string \
            --shlib-linker-install-name-flag "-dummy_flag" \
            --ranlib-flags "" \
            </dev/null >&AC_FD_CC 2>&1
    then
        AC_MSG_RESULT(yes)
    else
        BOOTSTRAP_MC=
        AC_MSG_RESULT(no)
        # This will only work if the user is working with a source
        # distribution which includes the pre-generated .C files.
        # So do a quick check now to ensure that we fail early with a
        # reasonable error message if they are not present.
        if  test -f compiler/mercury_compile.c ||
            test -f compiler/Mercury/cs/mercury_compile.c
        then
            true
        elif
            test -f $PREFIX/bin/mercury_compile ||
            test -f $PREFIX/bin/mercury_compile.exe
        then
            # This variable is used to let the tests in the following section
            # know that we are going to attempt to bootstrap with the compiler
            # $PREFIX/bin.
            bootstrap_from_prefix="yes"
            AC_MSG_WARN(
[using $PREFIX/bin/mercury_compile to bootstrap])
        else
            AC_MSG_ERROR(
[You need a working Mercury compiler to bootstrap with])
        fi
    fi
    rm -f conftest*

    export MERCURY_CONFIG_DIR="${SAVE_MERCURY_CONFIG_DIR}"
    export MERCURY_DEFAULT_GRADE="${SAVE_MERCURY_DEFAULT_GRADE}"
    export MERCURY_STDLIB_DIR="${SAVE_MERCURY_STDLIB_DIR}"
fi

#-----------------------------------------------------------------------------#

if test "$BOOTSTRAP_MC" = ""
then
    # If we do not have a working bootstrap Mercury compiler, then work out
    # whether any .c files we do have were compiled with high level code or
    # not. At this point we only check a single file -- this is enough to
    # allow us to determine a value for BOOTSTRAP_GRADE below -- we check
    # any other pre-generated .c files as part of the call to
    # mercury_check_c_files() later on.
    #
    if test -f compiler/mercury_compile.c
    then
        highlevel_code=`grep "HIGHLEVEL_CODE=.*" compiler/mercury_compile.c`
    else
        if test -f compiler/Mercury/cs/mercury_compile.c
        then
            highlevel_code=`grep "HIGHLEVEL_CODE=.*" compiler/Mercury/cs/mercury_compile.c`
        else
            # If we are going to attempt to bootstrap from a compiler in
            # $PREFIX/bin (see the above section), then it is ok if we do not
            # have any pre-generated .c files; if not, then we cannot continue.
            #
            if test "$bootstrap_from_prefix" != "yes"
            then
                AC_MSG_ERROR(
[You need a working Mercury compiler to bootstrap with])
            fi
        fi
    fi

    case "$highlevel_code" in

        *yes)
            BOOTSTRAP_HLC="yes"
            ;;

        *no)
            BOOTSTRAP_HLC="no"
            ;;

        "")
            # This corresponds to the case where we compile using
            # $PREFIX/bin/mercury_compile. Since we can (hopefully)
            # regenerate any .c files, it is irrelevant whether we used
            # --highlevel-code or not when compiling any existing ones.
            ;;

        *)
            AC_MSG_ERROR(
[Cannot determine what code model pre-generated .c files use.])
            ;;
    esac
fi

#-----------------------------------------------------------------------------#
#
# Determine whether to define LARGE_CONFIG when compiling boehm_gc.
#

AC_ARG_ENABLE(gc-large-config,
    AC_HELP_STRING([--disable-gc-large-config],
                   [disable large heaps when using conservative GC]),
    enable_gc_large_config="$enableval",enable_gc_large_config=yes)

AC_MSG_CHECKING(whether to enable large heaps when using conservative GC)
case "$enable_gc_large_config" in
    yes)
        AC_MSG_RESULT(yes)
        ENABLE_BOEHM_LARGE_CONFIG=-DLARGE_CONFIG
        ;;
    no)
        AC_MSG_RESULT(no)
        ENABLE_BOEHM_LARGE_CONFIG=
        ;;
    *)
        AC_MSG_RESULT(yes)
        AC_MSG_WARN([unexpected argument to --enable-gc-large-config])
        ENABLE_BOEHM_LARGE_CONFIG=-DLARGE_CONFIG
        ;;
esac
AC_SUBST(ENABLE_BOEHM_LARGE_CONFIG)

#-----------------------------------------------------------------------------#
#
# Determine whether to define USE_MMAP, USE_MUNMAP when compiling boehm_gc.
#

AC_ARG_ENABLE(gc-mmap,
    AC_HELP_STRING([--enable-gc-mmap],
                   [use mmap instead of sbrk when using Boehm GC]),
    enable_gc_mmap="$enableval",enable_gc_mmap=no)

AC_ARG_ENABLE(gc-munmap,
    AC_HELP_STRING([--enable-gc-munmap],
                   [enable unmapping of unused pages when using Boehm GC]),
    enable_gc_munmap="$enableval",enable_gc_munmap=no)

AC_MSG_CHECKING(whether to enable unmapping of unused pages when using Boehm GC)
case "$enable_gc_munmap" in
    yes)
        # --enable-gc-munmap implies --enable-gc-mmap
        enable_gc_mmap=yes
        AC_MSG_RESULT(yes)
        ;;
    no)
        AC_MSG_RESULT(no)
        ;;
    *)
        AC_MSG_ERROR([unexpected argument to --enable-gc-munmap])
        ;;
esac

AC_MSG_CHECKING(whether to use mmap for Boehm GC)
case "$enable_gc_mmap" in
    yes|no)
        AC_MSG_RESULT($enable_gc_mmap)
        ;;
    *)
        AC_MSG_ERROR([unexpected argument to --enable-gc-mmap])
        ;;
esac

if test "$enable_gc_mmap" = yes; then
    ENABLE_BOEHM_USE_MMAP="-DUSE_MMAP"
else
    ENABLE_BOEHM_USE_MMAP=
fi
if test "$enable_gc_munmap" = yes; then
    ENABLE_BOEHM_USE_MUNMAP="-DUSE_MUNMAP"
else
    ENABLE_BOEHM_USE_MUNMAP=
fi
AC_SUBST(ENABLE_BOEHM_USE_MMAP)
AC_SUBST(ENABLE_BOEHM_USE_MUNMAP)

#-----------------------------------------------------------------------------#

MERCURY_MSG("looking for GNU Make...")
AC_PROGRAMS_CHECK(GNU_MAKE,gmake make)
if test "$GNU_MAKE" != ""; then
    case "`$GNU_MAKE -v bogus </dev/null 2>&1`" in
        "GNU Make"*)
            ;;
        *)
            GNU_MAKE=
    esac
fi
if test "$GNU_MAKE" = ""; then
    AC_MSG_ERROR(cannot find GNU Make)
    exit 1
else
    MERCURY_MSG("found GNU Make: $GNU_MAKE")
fi
AC_SUBST(GNU_MAKE)

#-----------------------------------------------------------------------------#

MERCURY_MSG("looking for a way to create named pipes...")

# Neither mkfifo or mknod exist in a working state on MinGW, so on that
# platform we skip the following tests in order to avoid emitting extra
# warnings.
#
case "$host" in
    *mingw*)
        MERCURY_MSG("named pipes not available for MinGW")
        AC_DEFINE_UNQUOTED(MR_MKFIFO, "")
        MKFIFO=none
        MKNOD=""
    ;;

    *)

        save_PATH="$PATH"
        PATH="$PATH:/etc:/usr/etc:/sbin"

        AC_PATH_PROG(MKFIFO,mkfifo)
        if test "$MKFIFO" != ""; then
            # check that it really works
            tmp=$TMPDIR/fifo$$
            trap "rm -f $tmp" 1 2 3 13 15
            if $MKFIFO $tmp && test -p $tmp; then
                true
            else
                MKFIFO=""
            fi
            rm -f $tmp
        fi
        if test "$MKFIFO" = ""; then
            AC_PATH_PROG(MKNOD,mknod)
            if test "$MKNOD" != ""; then
                # check that it really works
                tmp=$TMPDIR/fifo$$
                trap "rm -f $tmp" 1 2 3 13 15
                if $MKNOD $tmp p && test -p $tmp; then
                    MKFIFO=mkfifo_using_mknod
                else
                    MKNOD=
                fi
            fi
        fi
        if test "$MKFIFO" = ""; then
            AC_MSG_WARN(cannot find a working \`mkfifo' or \`mknod')
            MKFIFO=none
            AC_DEFINE_UNQUOTED(MR_MKFIFO, "")
        else
            AC_DEFINE_UNQUOTED(MR_MKFIFO, "$MKFIFO")
        fi

        PATH="$save_PATH"
    ;;
esac

AC_SUBST(MKFIFO)
test "$MKNOD" = "" && MKNOD=mknod
AC_SUBST(MKNOD)

#-----------------------------------------------------------------------------#

MERCURY_MSG("looking for a way to create temporary files...")

AC_PATH_PROG(MKTEMP,mktemp)
if test "$MKTEMP" != ""; then
    # check that it really works
    TMPFILE="`mktemp $TMPDIR/configure.XXXXXX < /dev/null`"
    case "$TMPFILE" in
        $TMPDIR/configure.*)
            rm -f $TMPFILE
            ;;
        *)
            MKTEMP=""
            ;;
    esac
fi
if test "$MKTEMP" = ""; then
    AC_MSG_WARN(cannot find a working \`mktemp', using \`mkdir || exit')
    MKTEMP=""
fi
AC_SUBST(MKTEMP)

#-----------------------------------------------------------------------------#
AC_PATH_PROG(INSTALL_INFO,install-info)
AC_SUBST(INSTALL_INFO)
#-----------------------------------------------------------------------------#
AC_PATH_PROG(TEXI2DVI,texi2dvi)
AC_SUBST(TEXI2DVI)
#-----------------------------------------------------------------------------#
AC_PATH_PROG(PDFTEX, pdftex)
AC_SUBST(PDFTEX)
#-----------------------------------------------------------------------------#
AC_PATH_PROG(LYNX,lynx)
AC_SUBST(LYNX)
#-----------------------------------------------------------------------------#
AC_PATH_PROG(DVIPS,dvips)
AC_SUBST(DVIPS)
#-----------------------------------------------------------------------------#
AC_PATH_PROG(MAKEINFO,makeinfo)
AC_SUBST(MAKEINFO)
#-----------------------------------------------------------------------------#
AC_PATH_PROG(INFO,info)
AC_SUBST(INFO)
#-----------------------------------------------------------------------------#
AC_PATH_PROG(PERL,perl)
AC_SUBST(PERL)
#-----------------------------------------------------------------------------#
AC_PATH_PROG(HOSTNAMECMD,hostname uname)
case "$HOSTNAMECMD" in
    *hostname)
        # OS X uses BSD's hostname command which outputs
        # the fully qualified domain name by default. It
        # doesn't have a `-f' flag.
        # XXX We should probably check that the user isn't
        # using GNU's hostname on OS X instead.
        case "$host" in
            *apple*darwin*)
                HOSTNAMECMD="$HOSTNAMECMD"
                ;;
            *)
                HOSTNAMECMD="$HOSTNAMECMD -f"
                ;;
        esac
        ;;
esac
AC_DEFINE_UNQUOTED(MR_HOSTNAMECMD, "$HOSTNAMECMD")
#-----------------------------------------------------------------------------#

MERCURY_CHECK_CC_NEEDS_TRAD_CPP

if test "$MERCURY_HAVE_GCC" = "yes"
then
    GCC_PROG=$CC
else
    AC_PATH_PROG(GCC_PROG,gcc)
fi

AC_EXEEXT
AC_SUBST(EXEEXT)

# We need to ensure that CFLAGS does not contain `-g', because
# on some (broken) distributions of Linux (specifically Debian 0.93R6),
# the `-lc' and `-lg' libraries have different contents; specifically,
# only the latter contains memalign(). We need to make sure that the test
# for memalign() doesn't use `-g', since `ml' doesn't use `-g'.
CFLAGS="-O"

# We need to define _XOPEN_SOURCE when building boehm on Darwin 10
# so that we don't get errors about the functions in ucontext.h being
# deprecated.
case "$host" in
    *apple*darwin*10*)
        ENABLE_BOEHM_XOPEN_SOURCE="-D_XOPEN_SOURCE"
        ;;
    *)
        ENABLE_BOEHM_XOPEN_SOURCE=
        ;;
esac
AC_SUBST(ENABLE_BOEHM_XOPEN_SOURCE)

# We also need to add the appropriate `-I' options so that the test programs
# can #include various Mercury runtime headers.
case "$MERCURY_CONF_RECONFIGURE" in
    "")
        CPPFLAGS="-Iruntime -Iruntime/machdeps -Itrace $CPPFLAGS"
        ;;
    *)
        # We are generating a new configuration for an existing installation.
        CPPFLAGS="-I$PREFIX/lib/mercury/inc $CPPFLAGS"
        ;;
esac

#-----------------------------------------------------------------------------#

# Test whether specific command line options are supported by the C compiler.
# We do this by attempting to compile a program with the option whose presence
# we are testing for. This will not work for MSVC since it prints a warning
# about any unknown options and then proceeds as normal. There is no way to
# tell it to treat that warning as an error (in particular, this warning is not
# affected by the -WX option).

if test "$MERCURY_HAVE_MSVC" = "yes"
then

    CFLAGS_FOR_ERRMSG_FILTER=""
    CFLAGS_FOR_NO_MOVE_LOOP_INVARIANTS=
    CFLAGS_FOR_NO_REORDER_FUNCTIONS=""
    CFLAGS_FOR_NO_TREE_DOMINATOR_OPTS=

else

cat > conftest.c << EOF
    struct MR_TypeInfo_Almost_Struct {
        int MR_almost;
    };

    struct MR_TypeCtorInfo_Struct {
        int MR_tci1;
        int MR_tci2;
    };

    typedef struct MR_TypeCtorInfo_Struct       *MR_TypeCtorInfo;
    typedef const struct MR_TypeInfo_Almost_Struct  *MR_TypeInfo;

    struct MR_FA_TypeInfo_Struct1 {
        MR_TypeCtorInfo MR_ti_type_ctor_info;
        MR_TypeInfo MR_ti_fixed_arity_arg_typeinfos[1];
    };

    extern  const struct MR_FA_TypeInfo_Struct1 ML_type_ctor_info;

    MR_TypeInfo
    f(void)
    {
        MR_TypeInfo x;

        x = (MR_TypeInfo) &ML_type_ctor_info;
        return x;
    }
EOF

    AC_MSG_CHECKING(whether we can make error messages filterable)
    if $CC -ftrack-macro-expansion=0 -fno-diagnostics-show-caret -c conftest.c \
        </dev/null >&AC_FD_CC 2>&1
    then
        AC_MSG_RESULT(yes)
        CFLAGS_FOR_ERRMSG_FILTER="-ftrack-macro-expansion=0 -fno-diagnostics-show-caret"
    else
        AC_MSG_RESULT(no)
        CFLAGS_FOR_ERRMSG_FILTER=""
    fi

    AC_MSG_CHECKING(whether we can use -fno-move-loop-invariants)
    if $CC -fno-move-loop-invariants -c conftest.c \
        </dev/null >&AC_FD_CC 2>&1
    then
        AC_MSG_RESULT(yes)
        CFLAGS_FOR_NO_MOVE_LOOP_INVARIANTS="-fno-move-loop-invariants"
    else
        AC_MSG_RESULT(no)
        CFLAGS_FOR_NO_MOVE_LOOP_INVARIANTS=
    fi

    AC_MSG_CHECKING(whether we can use -fno-reorder-functions)
    if $CC -fno-reorder-functions -c conftest.c \
        </dev/null >&AC_FD_CC 2>&1
    then
        AC_MSG_RESULT(yes)
        CFLAGS_FOR_NO_REORDER_FUNCTIONS="-fno-reorder-functions"
    else
        AC_MSG_RESULT(no)
        CFLAGS_FOR_NO_REORDER_FUNCTIONS=""
    fi

    AC_MSG_CHECKING(whether we can use -fno-tree-dominator-opts)
    if $CC -fno-tree-dominator-opts -c conftest.c \
        </dev/null >&AC_FD_CC 2>&1
    then
        AC_MSG_RESULT(yes)
        CFLAGS_FOR_NO_TREE_DOMINATOR_OPTS="-fno-tree-dominator-opts"
    else
        AC_MSG_RESULT(no)
        CFLAGS_FOR_NO_TREE_DOMINATOR_OPTS=
    fi

    rm -f conftest*
fi
AC_SUBST(CFLAGS_FOR_ERRMSG_FILTER)

#-----------------------------------------------------------------------------#
# Make sure we search /usr/local/include and /usr/local/lib for
# header files and libraries. GNU C normally searches /usr/local/include
# by default, but (inconsistently) on some systems, such as Solaris,
# it does _not_ search /usr/local/lib.
# The user might also be using a different C compiler that
# doesn't search either of those by default.
MERCURY_CHECK_LOCAL_C_INCL_DIRS
CPPFLAGS="$ALL_LOCAL_C_INCL_DIRS $CPPFLAGS"
MERCURY_CHECK_LOCAL_C_LIB_DIRS
for dir in $ALL_LOCAL_C_LIB_DIRS kludge_for_broken_shells
do
    if test "$dir" != "kludge_for_broken_shells"; then
        LIBS="-L$dir $LIBS"
    fi
done

AC_PROG_CPP

#-----------------------------------------------------------------------------#

AC_PROG_EGREP
AC_MSG_CHECKING([for use of a Microsoft C compiler])
AC_EGREP_CPP([yes],
    [
        #ifdef _MSC_VER
            yes
        #endif
    ],
    [ac_microsoft=yes],
    [ac_microsoft=no]
)

if test "$ac_microsoft" = "yes"
then
    AC_MSG_RESULT([yes])
else
    AC_MSG_RESULT([no])
fi

# Determine if we are on a system which has a restricted command line length.
# This is true if we are building under mingw or using the MS C compiler.
RESTRICTED_COMMAND_LINE_OPT=
case "$host" in
    *mingw*)
        RESTRICTED_COMMAND_LINE_OPT=--restricted-command-line
        ;;
    *)
        if test "$ac_microsoft" = "yes"
        then
            RESTRICTED_COMMAND_LINE_OPT=--restricted-command-line
        else
            RESTRICTED_COMMAND_LINE_OPT=
        fi
        ;;
esac
AC_SUBST(RESTRICTED_COMMAND_LINE_OPT)

LDFLAGS_FOR_TRACE=
LD_LIBFLAGS_FOR_TRACE=
if test "$ac_microsoft" = "yes" ; then
    # Use MS-style file names and command-line options
    OBJ_SUFFIX="obj"
    LIB_SUFFIX="lib"
    LIB_PREFIX="lib"
    LIB_LIBPATH="-LIBPATH:"
    LINK_LIB=""
    LINK_LIB_SUFFIX=".lib"
    LINK_OPT_SEP="-link"

    OBJFILE_OPT="-Fo"
    EXEFILE_OPT="-Fe"
    AR="lib"
    ARFLAGS="-nologo"
    AR_LIBFILE_OPT="-OUT:"

    LDFLAGS_FOR_DEBUG="-DEBUG"
    LD_LIBFLAGS_FOR_DEBUG="-DEBUG"
    LD_STRIP_FLAG=
    STRIP_EXE=
    STRIP_EXE_SHARED_FLAGS=
    STRIP_EXE_STATIC_FLAGS=

    USING_MICROSOFT_CL_COMPILER="yes"
    FIX_PATH_FOR_CC="$CYGPATH"

    # MS doesn't use a ranlib.
    RANLIB="echo"
    RANLIBFLAGS=""
    AC_SUBST(RANLIB)
else
    # Use Unix-style file names and command-line options.
    OBJ_SUFFIX="o"
    LIB_SUFFIX="a"
    LIB_PREFIX=""
    LIB_LIBPATH="-L"
    LINK_LIB="-l"
    LINK_LIB_SUFFIX=""
    LINK_OPT_SEP=""

    OBJFILE_OPT="-o "
    EXEFILE_OPT="-o "
    AC_CHECK_TOOL([AR], [ar], [ar])
    ARFLAGS="cr"
    AR_LIBFILE_OPT=""

    LD_STRIP_FLAG="-s"
    STRIP_EXE=""
    STRIP_EXE_SHARED_FLAGS=""
    STRIP_EXE_STATIC_FLAGS=""

    # The `-s' linker option is deprecated on Darwin 9+ and has no effect other
    # than causing the linker to emit a warning. Call the strip tool directly.
    #
    case "$host" in
        *apple*darwin*)
            LD_STRIP_FLAG=""
            STRIP_EXE="strip"
            STRIP_EXE_SHARED_FLAGS="-x"
            STRIP_EXE_STATIC_FLAGS=""
            ;;
    esac

    LDFLAGS_FOR_DEBUG="-g"
    LD_LIBFLAGS_FOR_DEBUG="-g"

    # On Linux ELF, the linker option `-export-dynamic' is needed to make
    # symbols exported for use in code linked in with dlopen(), which is used
    # for interactive queries in the Mercury debugger. For gcc, this
    # linker option can be enabled by passing either `-rdynamic' or
    # `-Wl,-export-dynamic' to gcc; we use the former.
    #
    case $FULLARCH in
        *-linux*)
            LDFLAGS_FOR_TRACE="-rdynamic"
            ;;
    esac

    USING_MICROSOFT_CL_COMPILER="no"

    FIX_PATH_FOR_CC=echo

    AC_PROG_RANLIB
    RANLIBFLAGS=""

    # On MacOS 10.1 (and other MacOS systems?),
    # we need to pass the `-c' option to ranlib.
    # Otherwise, "common" symbols, i.e. "int foo;" without an initializer,
    # do not get included in the archive table of contents, and so
    # don't get found during linking. Whose !@#$ing idea was it to make
    # that brain-damaged and non-standard-conforming mode the default?
    case "$host" in
        *apple*darwin*)
            RANLIBFLAGS="-c"
            ;;
    esac
fi

AC_CHECK_TOOL([NM], [nm], [nm])

AC_SUBST(RANLIBFLAGS)
AC_SUBST(OBJFILE_OPT)
AC_SUBST(EXEFILE_OPT)
AC_SUBST(AR)
AC_SUBST(ARFLAGS)
AC_SUBST(AR_LIBFILE_OPT)
AC_SUBST(LDFLAGS_FOR_DEBUG)
AC_SUBST(LD_LIBFLAGS_FOR_DEBUG)
AC_SUBST(LD_STRIP_FLAG)
AC_SUBST(STRIP_EXE)
AC_SUBST(STRIP_EXE_SHARED_FLAGS)
AC_SUBST(STRIP_EXE_STATIC_FLAGS)
AC_SUBST(LDFLAGS_FOR_TRACE)
AC_SUBST(LD_LIBFLAGS_FOR_TRACE)
AC_SUBST(USING_MICROSOFT_CL_COMPILER)
AC_SUBST(OBJ_SUFFIX)
AC_SUBST(LIB_SUFFIX)
AC_SUBST(LIB_PREFIX)
AC_SUBST(LIB_LIBPATH)
AC_SUBST(LINK_LIB)
AC_SUBST(LINK_LIB_SUFFIX)
AC_SUBST(LINK_OPT_SEP)
AC_SUBST(FIX_PATH_FOR_CC)
AC_SUBST(CYGPATH)

#-----------------------------------------------------------------------------#
# Check for `-lm': some systems, e.g. MacOS X (Darwin), don't have it.
# The result of this check may be overridden below.
AC_CHECK_LIB(m, sin, [MATH_LIB=-lm], [MATH_LIB=])

#-----------------------------------------------------------------------------#
#
# Check whether to use symbolic links
#

enable_symlinks_opt=no
AC_ARG_ENABLE(symlinks,
   AC_HELP_STRING([--enable-symlinks],
                  [enable symbolic links. This is the default on all
                   systems except Windows. On Windows you can force
                   symbolic links to be used by giving this option.
                   Note that symbolic links will only work on Windows if
                   you are using the Cygwin version of GCC.]),
   [ enable_symlinks_opt=yes ])

# If we're compiling the compiler on Windows, don't use symlinks when
# installing unless the --enable-symlinks option is given.
#
# When cross-compiling from Cygwin to Mingw (and perhaps also on
# native Mingw?), `-lm' exists, but it is not needed and in fact we must
# not use it, because if we do then it causes cygwin1.dll to be linked in.
MMC_USE_SYMLINKS_OPT=
LN_S="ln -s"
case "$host" in
    *cygwin*)
        MATH_LIB=
        if test "$enable_symlinks_opt" != "yes"; then
            MMC_USE_SYMLINKS_OPT=--no-use-symlinks
            LN_S=false
        fi
        ;;
    *mingw*)
        MATH_LIB=
        if test "$enable_symlinks_opt" != "yes"; then
            MMC_USE_SYMLINKS_OPT=--no-use-symlinks
            LN_S=false
        fi
        ;;
esac

AC_SUBST(LN_S)
AC_SUBST(MMC_USE_SYMLINKS_OPT)
AC_SUBST(MATH_LIB)

#-----------------------------------------------------------------------------#
# Microsoft.NET configuration
#

AC_ARG_WITH(csharp-compiler,
    AC_HELP_STRING([--with-csharp-compiler=<compiler>],
                   [Specify which C Sharp compiler to use (default:
                   autodetect)]),
    mercury_cv_with_csharp_compiler="$withval",
    mercury_cv_with_csharp_compiler="")
MERCURY_CHECK_DOTNET

#-----------------------------------------------------------------------------#
# Java configuration
#

MERCURY_CHECK_JAVA

AC_ARG_ENABLE(javac-flags-for-heap-size,
    AC_HELP_STRING([--enable-javac-flags-for-heap-size],
                   [enable maximum heap size option for Java compiler]),
    enable_javac_flags_for_heap_size="$enableval",
    enable_javac_flags_for_heap_size=yes)

case "$enable_javac_flags_for_heap_size" in
    yes)
        MERCURY_CHECK_JAVAC_HEAP_SIZE
        JAVAC_FLAGS_FOR_HEAP_SIZE_MMAKE=$mercury_cv_javac_flags_for_heap_size_mmake
        JAVAC_FLAGS_FOR_HEAP_SIZE_CONFIG=$mercury_cv_javac_flags_for_heap_size_config
        ;;
    no)
        JAVAC_FLAGS_FOR_HEAP_SIZE_MMAKE=
        JAVAC_FLAGS_FOR_HEAP_SIZE_CONFIG=
        ;;
esac
AC_SUBST(JAVAC_FLAGS_FOR_HEAP_SIZE_MMAKE)
AC_SUBST(JAVAC_FLAGS_FOR_HEAP_SIZE_CONFIG)

#-----------------------------------------------------------------------------#
# Erlang configuration
#

MERCURY_CHECK_ERLANG

#-----------------------------------------------------------------------------#

mercury_check_for_functions () {
    for mercury_cv_func in "$@"
    do
        mercury_cv_func_define="MR_HAVE_`echo $mercury_cv_func | \
            tr abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ`"
        AC_CHECK_FUNC($mercury_cv_func, [
            AC_DEFINE_UNQUOTED($mercury_cv_func_define)
        ])
    done
}

mercury_check_for_stdio_functions () {
    for mercury_cv_stdio_func in "$@"
    do
        mercury_cv_stdio_func_define="MR_HAVE_`echo $mercury_cv_stdio_func | \
            tr abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ`"
        AC_CHECK_DECL($mercury_cv_stdio_func,
            [AC_DEFINE_UNQUOTED($mercury_cv_stdio_func_define)],
            [],
            [#include <stdio.h>])
    done
}

# Don't try to use mprotect() on gnu-win32, since it is broken
# (at least for version b18, anyway) and trying it can crash Win95.
case "$host" in
    *-cygwin*)
        ac_cv_func_mprotect=no
        ;;
    *mingw*)
        ac_cv_func_mprotect=no
        ;;
esac

# Do not try to use sleep() on MinGW or MinGW64 as it isn't available
# *despite* what the standard autoconf tests report.
#
# The MinGW64 implementation of mkstemp is broken; it isn't supported on
# MinGW32.
#
case "$host" in
    *mingw*)
        ac_cv_func_sleep=no
        ac_cv_func_mkstemp=no
        ;;
esac

mercury_check_for_functions \
        sysconf getpagesize gethostname \
        mmap mprotect memalign posix_memalign sbrk memmove \
        sigaction siginterrupt setitimer \
        strerror strerror_r strerror_s \
        open close dup dup2 fdopen fileno fstat stat lstat isatty \
        getpid setpgid fork execlp wait kill \
        grantpt unlockpt ptsname tcgetattr tcsetattr ioctl \
        access sleep opendir readdir closedir mkdir symlink readlink \
        gettimeofday setenv putenv _putenv posix_spawn sched_setaffinity \
        sched_getaffinity sched_getcpu sched_yield mkstemp mkdtemp \
        setrlimit getcontext dl_iterate_phdr

mercury_check_for_stdio_functions \
        snprintf _snprintf vsnprintf _vsnprintf

save_LIBS="$LIBS"
LIBS="$MATH_LIB"
mercury_check_for_functions \
        fma
LIBS="$save_LIBS"

#-----------------------------------------------------------------------------#

MERCURY_CHECK_FOR_HEADERS( \
        unistd.h sys/wait.h sys/siginfo.h sys/signal.h ucontext.h \
        asm/sigcontext.h sys/param.h sys/time.h sys/times.h \
        sys/types.h sys/stat.h fcntl.h termios.h sys/ioctl.h \
        sys/resource.h sys/stropts.h windows.h dirent.h getopt.h malloc.h \
        semaphore.h pthread.h time.h spawn.h fenv.h sys/mman.h sys/sem.h \
        sched.h utmpx.h dispatch/dispatch.h stdint.h)

if test "$MR_HAVE_GETOPT_H" = 1; then
    GETOPT_H_AVAILABLE=yes
else
    GETOPT_H_AVAILABLE=no
fi
AC_SUBST(GETOPT_H_AVAILABLE)

if test "$MR_HAVE_UCONTEXT_H" != 1; then
    MERCURY_CHECK_FOR_HEADERS(sys/ucontext.h)
fi

#-----------------------------------------------------------------------------#
#
# Check whether we can set the FP rounding mode
#

MERCURY_CHECK_FOR_FENV_FUNC([fesetround], [$MATH_LIB])

#-----------------------------------------------------------------------------#
#
# Check whether we have __builtin_bswap builtin functions
#

AC_MSG_CHECKING(whether we have __builtin_bswap16)
AC_CACHE_VAL(mercury_cv_have_builtin_bswap16,
    AC_TRY_LINK(
        [],
        [__builtin_bswap16(1);],
        [mercury_cv_have_builtin_bswap16=yes],
        [mercury_cv_have_builtin_bswap16=no])
)
AC_MSG_RESULT($mercury_cv_have_builtin_bswap16)
if test "$mercury_cv_have_builtin_bswap16" = yes; then
    AC_DEFINE(MR_HAVE_BUILTIN_BSWAP16)
fi

AC_MSG_CHECKING(whether we have __builtin_bswap32)
AC_CACHE_VAL(mercury_cv_have_builtin_bswap32,
    AC_TRY_LINK(
        [],
        [__builtin_bswap32(1);],
        [mercury_cv_have_builtin_bswap32=yes],
        [mercury_cv_have_builtin_bswap32=no])
)
AC_MSG_RESULT($mercury_cv_have_builtin_bswap32)
if test "$mercury_cv_have_builtin_bswap32" = yes; then
    AC_DEFINE(MR_HAVE_BUILTIN_BSWAP32)
fi

AC_MSG_CHECKING(whether we have __builtin_bswap64)
AC_CACHE_VAL(mercury_cv_have_builtin_bswap64,
    AC_TRY_LINK(
        [],
        [__builtin_bswap64(1);],
        [mercury_cv_have_builtin_bswap64=yes],
        [mercury_cv_have_builtin_bswap64=no])
)
AC_MSG_RESULT($mercury_cv_have_builtin_bswap64)
if test "$mercury_cv_have_builtin_bswap64" = yes; then
    AC_DEFINE(MR_HAVE_BUILTIN_BSWAP64)
fi

#-----------------------------------------------------------------------------#
#
# Check the basics of sigaction
#

if test "$ac_cv_func_sigaction" = yes; then
    AC_MSG_CHECKING(for \`sigaction' field name)
    AC_CACHE_VAL(mercury_cv_sigaction_field,
    AC_TRY_RUN(
        [
            #include <signal.h>
            #include <stdlib.h>

            #define FAULT_ADDRESS ((int *)112)

            extern void handler(int signum, siginfo_t *info, void *context);

            int main() {
                struct sigaction act;
                act.sa_flags = SA_SIGINFO;
                act.sa_sigaction = handler;
                if (sigemptyset(&act.sa_mask) != 0)
                    exit(1);
                if (sigaction(SIGSEGV, &act, NULL) != 0)
                    exit(1);
                /* provoke a SIGSEGV */
                (*FAULT_ADDRESS)++;
                exit(1);
            }

            void handler(int signum, siginfo_t *info, void *context) {
                if (signum == SIGSEGV &&
                    info->si_signo == SIGSEGV &&
                    info->si_code > 0 &&
                    (int *)info->si_addr == FAULT_ADDRESS)
                {
                    exit(0);
                } else {
                    exit(1);
                }
            }
        ],
        [mercury_cv_sigaction_field=sa_sigaction],
        [mercury_cv_sigaction_field=sa_handler]))
    AC_MSG_RESULT($mercury_cv_sigaction_field)
    AC_DEFINE_UNQUOTED(MR_SIGACTION_FIELD,$mercury_cv_sigaction_field)
    if test "$mercury_cv_sigaction_field" = sa_sigaction; then
        AC_DEFINE([MR_HAVE_SIGINFO])
    fi
fi

#-----------------------------------------------------------------------------#
#
# Check the basics of sigcontext_struct
#

AC_MSG_CHECKING(for working \`sigcontext_struct' in second arg)
AC_CACHE_VAL(mercury_cv_sigcontext_struct_2arg,
    mercury_cv_sigcontext_struct_2arg=no

    AC_TRY_RUN(
        [
            /* see runtime/mercury_signal.h for documentation of this code */

            #define sigcontext_struct sigcontext

            #define __KERNEL__
            #include <signal.h>
            #undef __KERNEL__

            #ifdef MR_HAVE_ASM_SIGCONTEXT_H
            #include <asm/sigcontext.h>
            #endif

            #include "mercury_faultaddr.h"

            #include <stdio.h>

            extern void handler(int signum, struct sigcontext_struct info);

            #define FAULT_ADDRESS ((int *)112)

            int main() {
                signal(SIGSEGV, (void (*)(int))handler);

                /* provoke a SIGSEGV */
                (*FAULT_ADDRESS)++;

                exit(1);
            }

            void handler(int signum, struct sigcontext_struct context) {
                int *addr;
                addr = (int *) MR_GET_FAULT_ADDR(context);
                if (signum == SIGSEGV && addr == FAULT_ADDRESS) {
                    exit(0);
                } else {
                    exit(1);
                }
            }
        ],
        [mercury_cv_sigcontext_struct_2arg=yes],
        [true]))
AC_MSG_RESULT($mercury_cv_sigcontext_struct_2arg)

if test "$mercury_cv_sigcontext_struct_2arg" = no; then
    # Check for a sigcontext_struct in the third argument of
    # the signal handler (Linux-68k has this).
    AC_MSG_CHECKING(for working \`sigcontext_struct' in third arg)
    AC_CACHE_VAL(mercury_cv_sigcontext_struct_3arg,
    mercury_cv_sigcontext_struct_3arg=no

    AC_TRY_RUN(
        [
            /* see runtime/mercury_signal.h for documentation of this code */

            #define sigcontext_struct sigcontext

            #define __KERNEL__
            #include <signal.h>
            #undef __KERNEL__

            #ifdef MR_HAVE_ASM_SIGCONTEXT_H
            #include <asm/sigcontext.h>
            #endif

            #include "mercury_faultaddr.h"

            #include <stdio.h>

            extern void handler(int signum, int code, struct
                sigcontext_struct info);

            #define FAULT_ADDRESS ((int *)112)

            int main() {
                signal(SIGSEGV, (void (*)(int))handler);

                /* provoke a SIGSEGV */
                (*FAULT_ADDRESS)++;

                exit(1);
            }

            void handler(int signum, int code, struct sigcontext_struct context) {
                int *addr;
                addr = (int *) MR_GET_FAULT_ADDR(context);
                if (signum == SIGSEGV && addr == FAULT_ADDRESS) {
                    exit(0);
                } else {
                    exit(1);
                }
            }
        ],
        [mercury_cv_sigcontext_struct_3arg=yes],
        [true]))
    AC_MSG_RESULT($mercury_cv_sigcontext_struct_3arg)
else
    mercury_cv_sigcontext_struct_3arg=no
fi

mercury_cv_sigcontext_struct=no

if test "$mercury_cv_sigcontext_struct_2arg" = yes; then
    mercury_cv_sigcontext_struct=yes
    AC_DEFINE(MR_HAVE_SIGCONTEXT_STRUCT_2ARG)
fi

if test "$mercury_cv_sigcontext_struct_3arg" = yes; then
    mercury_cv_sigcontext_struct=yes
    AC_DEFINE(MR_HAVE_SIGCONTEXT_STRUCT_3ARG)
fi

if test "$mercury_cv_sigcontext_struct" = yes; then
    AC_DEFINE(MR_HAVE_SIGCONTEXT_STRUCT)
    AC_DEFINE(MR_HAVE_SIGINFO)

    # check for sigcontext_struct.eip
    AC_MSG_CHECKING(for \`sigcontext_struct' pc access at signals)
    AC_CACHE_VAL(mercury_cv_pc_access,
    mercury_cv_pc_access=no
    AC_TRY_RUN(
        [
            /* see runtime/mercury_signal.h for documentation of this code */

            #define sigcontext_struct sigcontext

            #define __KERNEL__
            #include <signal.h>
            #undef __KERNEL__

            #ifdef MR_HAVE_ASM_SIGCONTEXT_H
            #include <asm/sigcontext.h>
            #endif

            #include "mercury_faultaddr.h"

            #include <stdio.h>

            #if MR_HAVE_SIGCONTEXT_STRUCT_3ARG
            extern void handler(int signum, int code,
                    struct sigcontext_struct context);
            #else
            extern void handler(int signum, struct sigcontext_struct context);
            #endif

            #define FAULT_ADDRESS ((int *)112)

            int main() {
                signal(SIGSEGV, (void (*)(int))handler);

                /* provoke a SIGSEGV */
                (*FAULT_ADDRESS)++;

                exit(1);
            }

            #if MR_HAVE_SIGCONTEXT_STRUCT_3ARG
            void handler(int signum, int code, struct sigcontext_struct context) {
            #else
            void handler(int signum, struct sigcontext_struct context) {
            #endif
                int *addr;
                long pc;
                addr = (int *) MR_GET_FAULT_ADDR(context);
                pc = (long) context.eip;
                if (signum == SIGSEGV && addr == FAULT_ADDRESS && pc != 0) {
                    exit(0);
                } else {
                    exit(1);
                }
            }
        ],
        [mercury_cv_pc_access=eip],
        [true]))
    AC_MSG_RESULT($mercury_cv_pc_access)
    if test "$mercury_cv_pc_access" != no; then
        AC_DEFINE_UNQUOTED(MR_PC_ACCESS,$mercury_cv_pc_access)
    fi
else
    AC_MSG_CHECKING(for \`siginfo_t')
    AC_CACHE_VAL(mercury_cv_siginfo_t,
    mercury_cv_siginfo_t=no
    AC_TRY_RUN(
        [
            #include <stdio.h>
            #include <signal.h>
            #ifdef MR_HAVE_SYS_SIGINFO_H
            #include <sys/siginfo.h>
            #endif
            #ifdef MR_HAVE_SYS_SIGNAL_H
            #include <sys/signal.h>
            #endif
            #ifdef MR_HAVE_UCONTEXT_H
            #include <ucontext.h>
            #endif
            #ifdef MR_HAVE_SYS_UCONTEXT_H
            #include <sys/ucontext.h>
            #endif
            int save_signum = 0;
            int save_cause;
            int save_pc;
            extern void handler(int signum, siginfo_t *info, void *context);
            int main() {
                struct sigaction act;
                act.sa_flags = SA_SIGINFO;
                act.$mercury_cv_sigaction_field = handler;
                if (sigemptyset(&act.sa_mask) != 0)
                    exit(1);
                if (sigaction(SIGSEGV, &act, NULL) != 0)
                    exit(1);
                if (kill(getpid(), SIGSEGV) != 0)
                    exit(1);
                if (save_signum == 0)
                    exit(1);
                exit(0);
            }
            void handler(int signum, siginfo_t *info, void *context) {
                save_signum = signum;
                save_cause = info->si_code;
            }
        ],
        [mercury_cv_siginfo_t=yes],
        [true]))
    AC_MSG_RESULT($mercury_cv_siginfo_t)
    if test "$mercury_cv_siginfo_t" = yes; then
        AC_DEFINE(MR_HAVE_SIGINFO_T)
        AC_DEFINE(MR_HAVE_SIGINFO)

        AC_MSG_CHECKING(for \`siginfo' pc access at signals)
        AC_CACHE_VAL(mercury_cv_pc_access,
        mercury_cv_pc_access=no
        AC_TRY_RUN(
            [
                #include <stdio.h>
                #include <signal.h>
                #include <sys/siginfo.h>
                #ifdef MR_HAVE_UCONTEXT_H
                #include <ucontext.h>
                #else
                #include <sys/ucontext.h>
                #endif
                int save_signum = 0;
                int save_cause;
                int save_pc;
                extern void handler(int signum, siginfo_t *info, void *context);
                int main() {
                    struct sigaction act;
                    act.sa_flags = SA_SIGINFO;
                    act.$mercury_cv_sigaction_field = handler;
                    if (sigemptyset(&act.sa_mask) != 0)
                        exit(1);
                    if (sigaction(SIGSEGV, &act, NULL) != 0)
                        exit(1);
                    if (kill(getpid(), SIGSEGV) != 0)
                        exit(1);
                    if (save_signum == 0)
                        exit(1);
                    exit(0);
                }
                void handler(int signum, siginfo_t *info, void *context) {
                    save_signum = signum;
                    save_cause = info->si_code;
                    /* Don't use array indexing - the square brackets
                       are autoconf quote characters */
                    save_pc = *(((ucontext_t *) context)->uc_mcontext.gregs
                            + REG_PC);
                }
            ],
            [mercury_cv_pc_access=REG_PC;mercury_cv_pc_access_greg=yes],
            [true])
        AC_TRY_RUN(
            [
                #include <stdio.h>
                #include <signal.h>
                #include <sys/siginfo.h>
                #ifdef MR_HAVE_UCONTEXT_H
                #include <ucontext.h>
                #else
                #include <sys/ucontext.h>
                #endif
                int save_signum = 0;
                int save_cause;
                int save_pc;
                extern void handler(int signum, siginfo_t *info, void *context);
                int main() {
                    struct sigaction act;
                    act.sa_flags = SA_SIGINFO;
                    act.$mercury_cv_sigaction_field = handler;
                    if (sigemptyset(&act.sa_mask) != 0)
                        exit(1);
                    if (sigaction(SIGSEGV, &act, NULL) != 0)
                        exit(1);
                    if (kill(getpid(), SIGSEGV) != 0)
                        exit(1);
                    if (save_signum == 0)
                        exit(1);
                    exit(0);
                }
                void handler(int signum, siginfo_t *info, void *context) {
                    save_signum = signum;
                    save_cause = info->si_code;
                    save_pc = *(((ucontext_t *) context)->uc_mcontext.gregs
                            + CTX_EPC);
                }
            ],
            [mercury_cv_pc_access=CTX_EPC;mercury_cv_pc_access_greg=yes],
            [true])
        AC_TRY_RUN(
            [
                #include <stdio.h>
                #include <signal.h>
                #include <sys/siginfo.h>
                #ifdef MR_HAVE_UCONTEXT_H
                #include <ucontext.h>
                #else
                #include <sys/ucontext.h>
                #endif
                int save_signum = 0;
                int save_cause;
                int save_pc;
                extern void handler(int signum, siginfo_t *info, void *context);
                int main() {
                    struct sigaction act;
                    act.sa_flags = SA_SIGINFO;
                    act.$mercury_cv_sigaction_field = handler;
                    if (sigemptyset(&act.sa_mask) != 0)
                        exit(1);
                    if (sigaction(SIGSEGV, &act, NULL) != 0)
                        exit(1);
                    if (kill(getpid(), SIGSEGV) != 0)
                        exit(1);
                    if (save_signum == 0)
                        exit(1);
                    exit(0);
                }
                void handler(int signum, siginfo_t *info, void *context) {
                    save_signum = signum;
                    save_cause = info->si_code;
                    save_pc = ((ucontext_t *) context)->uc_mcontext.sc_pc;
                }
            ],
            [mercury_cv_pc_access=sc_pc;mercury_cv_pc_access_greg=no],
            [true])
        )
        AC_MSG_RESULT($mercury_cv_pc_access)
        if test "$mercury_cv_pc_access" != no; then
            AC_DEFINE_UNQUOTED(MR_PC_ACCESS,$mercury_cv_pc_access)
            if test "$mercury_cv_pc_access_greg" != no; then
                AC_DEFINE(MR_PC_ACCESS_GREG)
            fi
        fi
    fi
fi
#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(for an integer type with the same size as a pointer)
AC_CACHE_VAL(mercury_cv_word_type,
    MERCURY_TRY_STATIC_ASSERT(
        [],
        [sizeof(int) == sizeof(void *)],
        [mercury_cv_word_type=int],
        MERCURY_TRY_STATIC_ASSERT(
            [],
            [sizeof(long) == sizeof(void *)],
            [mercury_cv_word_type=long],
            MERCURY_TRY_STATIC_ASSERT(
                [],
                [sizeof(long long) == sizeof(void *)],
                [mercury_cv_word_type="long long"],
                [mercury_cv_word_type=unknown])))
)
AC_MSG_RESULT($mercury_cv_word_type)
AC_DEFINE_UNQUOTED(MR_WORD_TYPE, $mercury_cv_word_type)
MR_WORD_TYPE=$mercury_cv_word_type
AC_SUBST(MR_WORD_TYPE)

AC_DEFINE_UNQUOTED(MR_ROBDD_int, $mercury_cv_word_type)
AC_SUBST(MR_ROBDD_int)
AC_DEFINE_UNQUOTED(MR_ROBDD_uint, unsigned $mercury_cv_word_type)
AC_SUBST(MR_ROBDD_uint)

if test "$mercury_cv_word_type" = int; then
    AC_DEFINE_UNQUOTED(MR_INTEGER_LENGTH_MODIFIER, "")
    AC_DEFINE_UNQUOTED(MR_ROBDD_int_max, INT_MAX)
elif test "$mercury_cv_word_type" = long; then
    AC_DEFINE_UNQUOTED(MR_INTEGER_LENGTH_MODIFIER, "l")
    AC_DEFINE_UNQUOTED(MR_ROBDD_int_max, LONG_MAX)
elif test "$mercury_cv_word_type" = "long long"; then
    AC_DEFINE_UNQUOTED(MR_INTEGER_LENGTH_MODIFIER, "ll")
    AC_DEFINE_UNQUOTED(MR_ROBDD_int_max, LLONG_MAX)
else
    AC_MSG_ERROR(Cannot determine the length modifier for the MR_Integer type.)
fi
AC_SUBST(MR_INTEGER_LENGTH_MODIFIER)
AC_SUBST(MR_ROBDD_int_max)

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(for an integer type of at least 64 bits)
AC_CACHE_VAL(mercury_cv_int_least64_type,
    MERCURY_TRY_STATIC_ASSERT(
        [#include <limits.h>],
        [sizeof(int) * CHAR_BIT >= 64],
        [mercury_cv_int_least64_type=int],
        MERCURY_TRY_STATIC_ASSERT(
            [#include <limits.h>],
            [sizeof(long) * CHAR_BIT >= 64],
            [mercury_cv_int_least64_type=long],
            MERCURY_TRY_STATIC_ASSERT(
                [#include <limits.h>],
                [sizeof(long long) * CHAR_BIT >= 64],
                [mercury_cv_int_least64_type="long long"],
                MERCURY_TRY_STATIC_ASSERT(
                    [#include <limits.h>],
                    [sizeof(__int64) * CHAR_BIT >= 64],
                    [mercury_cv_int_least64_type=__int64],
                    [mercury_cv_int_least64_type=unknown]))))
)
# Uncomment the following line to test how the system behaves
# in the absence of a 64-bit integer type.
# mercury_cv_int_least64_type=unknown
AC_MSG_RESULT($mercury_cv_int_least64_type)
if test "$mercury_cv_int_least64_type" != unknown; then
    AC_DEFINE_UNQUOTED(MR_INT_LEAST64_TYPE, $mercury_cv_int_least64_type)
    MR_INT_LEAST64_TYPE=$mercury_cv_int_least64_type
    AC_SUBST(MR_INT_LEAST64_TYPE)

    if test "$mercury_cv_int_least64_type" = int; then
        AC_DEFINE_UNQUOTED(MR_INT_LEAST64_LENGTH_MODIFIER, "")
    elif test "$mercury_cv_int_least64_type" = long; then
        AC_DEFINE_UNQUOTED(MR_INT_LEAST64_LENGTH_MODIFIER, "l")
    elif test "$mercury_cv_int_least64_type" = "long long"; then
        AC_DEFINE_UNQUOTED(MR_INT_LEAST64_LENGTH_MODIFIER, "ll")
    elif test "$mercury_cv_int_least64_type" = "__int64"; then
        AC_DEFINE_UNQUOTED(MR_INT_LEAST64_LENGTH_MODIFIER, "I64")
    else
        AC_MSG_ERROR(Cannot determine the length modifier for the MR_int_least64_t type.)
    fi
    AC_SUBST(MR_INT_LEAST64_LENGTH_MODIFIER)
fi

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(for an integer type of at least 32 bits)
AC_CACHE_VAL(mercury_cv_int_least32_type,
    MERCURY_TRY_STATIC_ASSERT(
        [#include <limits.h>],
        [sizeof(int) * CHAR_BIT >= 32],
        [mercury_cv_int_least32_type=int],
        MERCURY_TRY_STATIC_ASSERT(
            [#include <limits.h>],
            [sizeof(long) * CHAR_BIT >= 32],
            [mercury_cv_int_least32_type=long],
            [mercury_cv_int_least32_type=unknown]))
)
AC_MSG_RESULT($mercury_cv_int_least32_type)
AC_DEFINE_UNQUOTED(MR_INT_LEAST32_TYPE, $mercury_cv_int_least32_type)
MR_INT_LEAST32_TYPE=$mercury_cv_int_least32_type
AC_SUBST(MR_INT_LEAST32_TYPE)

if test "$mercury_cv_int_least32_type" = int; then
    AC_DEFINE_UNQUOTED(MR_INT_LEAST32_MAX, INT_MAX)
    AC_DEFINE_UNQUOTED(MR_UINT_LEAST32_MAX, UINT_MAX)
elif test "$mercury_cv_int_least32_type" = long; then
    AC_DEFINE_UNQUOTED(MR_INT_LEAST32_MAX, LONG_MAX)
    AC_DEFINE_UNQUOTED(MR_UINT_LEAST32_MAX, ULONG_MAX)
else
    AC_MSG_ERROR(
[Cannot find the name of the max values
    for MR_INT_LEAST32_TYPE.])
fi
AC_SUBST(MR_INT_LEAST32_MAX)
AC_SUBST(MR_UINT_LEAST32_MAX)
#-----------------------------------------------------------------------------#
AC_TYPE_PID_T

AC_CHECK_TYPES([dev_t, ino_t])
case "$ac_cv_type_dev_t" in
    yes)
        AC_DEFINE_UNQUOTED(MR_HAVE_DEV_T)
        ;;
esac
case "$ac_cv_type_ino_t" in
    yes)
        AC_DEFINE_UNQUOTED(MR_HAVE_INO_T)
        ;;
esac

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(for an integer type of at least 16 bits)
AC_CACHE_VAL(mercury_cv_int_least16_type,
    MERCURY_TRY_STATIC_ASSERT(
        [#include <limits.h>],
        [sizeof(short) * CHAR_BIT >= 16],
        [mercury_cv_int_least16_type=short],
        MERCURY_TRY_STATIC_ASSERT(
            [#include <limits.h>],
            [sizeof(int) * CHAR_BIT >= 16],
            [mercury_cv_int_least16_type=int],
            [mercury_cv_int_least16_type=unknown]))
)
AC_MSG_RESULT($mercury_cv_int_least16_type)
AC_DEFINE_UNQUOTED(MR_INT_LEAST16_TYPE, $mercury_cv_int_least16_type)
MR_INT_LEAST16_TYPE=$mercury_cv_int_least16_type
AC_SUBST(MR_INT_LEAST16_TYPE)

if test "$mercury_cv_int_least16_type" = short; then
    AC_DEFINE_UNQUOTED(MR_INT_LEAST16_MAX, SHRT_MAX)
    AC_DEFINE_UNQUOTED(MR_UINT_LEAST16_MAX, USHRT_MAX)
elif test "$mercury_cv_int_least16_type" = int; then
    AC_DEFINE_UNQUOTED(MR_INT_LEAST16_MAX, INT_MAX)
    AC_DEFINE_UNQUOTED(MR_UINT_LEAST16_MAX, UINT_MAX)
else
    AC_MSG_ERROR(Cannot find the name of the max value for MR_INT_LEAST16_TYPE.)
fi
AC_SUBST(MR_INT_LEAST16_MAX)
AC_SUBST(MR_UINT_LEAST16_MAX)

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(the number of low tag bits available)
AC_CACHE_VAL(mercury_cv_low_tag_bits,
    MERCURY_TRY_STATIC_ASSERT(
        [],
        [sizeof(void *) == 4],
        [mercury_cv_low_tag_bits=2],
        MERCURY_TRY_STATIC_ASSERT(
            [],
            [sizeof(void *) == 8],
            [mercury_cv_low_tag_bits=3],
            MERCURY_TRY_STATIC_ASSERT(
                [],
                [sizeof(void *) == 16],
                [mercury_cv_low_tag_bits=4],
                [mercury_cv_low_tag_bits=0])))
)
AC_MSG_RESULT($mercury_cv_low_tag_bits)
if test "$mercury_cv_low_tag_bits" -lt 2; then
    if test "$BOOTSTRAP_MC" = ""; then
        AC_MSG_ERROR(cannot bootstrap: low tag bits less than 2)
        exit 1
    fi
fi
AC_DEFINE_UNQUOTED(MR_LOW_TAG_BITS, $mercury_cv_low_tag_bits)
LOW_TAG_BITS=$mercury_cv_low_tag_bits
AC_SUBST(LOW_TAG_BITS)

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(the number of bytes per word)
AC_CACHE_VAL(mercury_cv_bytes_per_word,
    MERCURY_TRY_STATIC_ASSERT(
        [],
        [sizeof(void *) == 4],
        [mercury_cv_bytes_per_word=4],
        MERCURY_TRY_STATIC_ASSERT(
            [],
            [sizeof(void *) == 8],
            [mercury_cv_bytes_per_word=8],
            [mercury_cv_bytes_per_word=0]))
)
AC_MSG_RESULT($mercury_cv_bytes_per_word)
AC_DEFINE_UNQUOTED(MR_BYTES_PER_WORD, $mercury_cv_bytes_per_word)
BYTES_PER_WORD=$mercury_cv_bytes_per_word
AC_SUBST(BYTES_PER_WORD)

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(the number of bits per word)
AC_CACHE_VAL(mercury_cv_bits_per_word,
    MERCURY_TRY_STATIC_ASSERT(
        [#include <limits.h>],
        [sizeof(void *) * CHAR_BIT == 32],
        [mercury_cv_bits_per_word=32],
        MERCURY_TRY_STATIC_ASSERT(
            [#include <limits.h>],
            [sizeof(void *) * CHAR_BIT == 64],
            [mercury_cv_bits_per_word=64],
            [mercury_cv_bits_per_word=0]))
)
AC_MSG_RESULT($mercury_cv_bits_per_word)
AC_DEFINE_UNQUOTED(MR_BITS_PER_WORD, $mercury_cv_bits_per_word)
BITS_PER_WORD=$mercury_cv_bits_per_word
AC_SUBST(BITS_PER_WORD)

AC_DEFINE_UNQUOTED(MR_ROBDD_BITS_PER_WORD, $mercury_cv_bits_per_word)
AC_SUBST(MR_ROBDD_BITS_PER_WORD)
case "$mercury_cv_bits_per_word" in
    32)
        mercury_cv_log_bits_per_word=5
        ;;
    64)
        mercury_cv_log_bits_per_word=6
        ;;
    *)
        AC_MSG_ERROR([unexpected number of bits per word: expected 32 or 64])
        ;;
esac
AC_DEFINE_UNQUOTED(MR_ROBDD_LOG_BITS_PER_WORD, $mercury_cv_log_bits_per_word)
AC_SUBST(MR_ROBDD_LOG_BITS_PER_WORD)

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(whether int is 32-bit)
AC_CACHE_VAL(mercury_cv_int_is_32_bit,
    MERCURY_TRY_STATIC_ASSERT(
        [#include <limits.h>],
        [sizeof(int) * CHAR_BIT == 32],
        [mercury_cv_int_is_32_bit=yes],
        [mercury_cv_int_is_32_bit=no])
)
AC_MSG_RESULT($mercury_cv_int_is_32_bit)
if test "$mercury_cv_int_is_32_bit" = yes; then
    AC_DEFINE(MR_INT_IS_32_BIT)
fi
AC_SUBST(MR_INT_IS_32_BIT)

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(whether long is 64-bit)
AC_CACHE_VAL(mercury_cv_long_is_64_bit,
    MERCURY_TRY_STATIC_ASSERT(
        [#include <limits.h>],
        [sizeof(long) * CHAR_BIT == 64],
        [mercury_cv_long_is_64_bit=yes],
        [mercury_cv_long_is_64_bit=no])
)
AC_MSG_RESULT($mercury_cv_long_is_64_bit)
if test "$mercury_cv_long_is_64_bit" = yes; then
    AC_DEFINE(MR_LONG_IS_64_BIT)
fi
AC_SUBST(MR_LONG_IS_64_BIT)

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(whether we can use unboxed floats)
AC_CACHE_VAL(mercury_cv_unboxed_floats,
    MERCURY_TRY_STATIC_ASSERT(
        [],
        [sizeof(double) == sizeof(void *)],
        [mercury_cv_unboxed_floats=yes],
        [mercury_cv_unboxed_floats=no])
)
AC_MSG_RESULT($mercury_cv_unboxed_floats)
if test "$mercury_cv_unboxed_floats" = yes; then
    HAVE_BOXED_FLOATS="--unboxed-float"
else
    AC_DEFINE(MR_BOXED_FLOAT)
    HAVE_BOXED_FLOATS="--no-unboxed-float"
fi
AC_SUBST(HAVE_BOXED_FLOATS)

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(whether float is 64-bit)
AC_CACHE_VAL(mercury_cv_float_is_64_bit,
    MERCURY_TRY_STATIC_ASSERT(
        [#include <limits.h>],
        [sizeof(float) * CHAR_BIT == 64],
        [mercury_cv_float_is_64_bit=yes],
        [mercury_cv_float_is_64_bit=no])
)
AC_MSG_RESULT($mercury_cv_float_is_64_bit)
if test "$mercury_cv_float_is_64_bit" = yes; then
    AC_DEFINE(MR_FLOAT_IS_64_BIT)
fi
AC_SUBST(MR_FLOAT_IS_64_BIT)

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(whether double is 64-bit)
AC_CACHE_VAL(mercury_cv_double_is_64_bit,
    MERCURY_TRY_STATIC_ASSERT(
        [#include <limits.h>],
        [sizeof(double) * CHAR_BIT == 64],
        [mercury_cv_double_is_64_bit=yes],
        [mercury_cv_double_is_64_bit=no])
)
AC_MSG_RESULT($mercury_cv_double_is_64_bit)
if test "$mercury_cv_double_is_64_bit" = yes; then
    AC_DEFINE(MR_DOUBLE_IS_64_BIT)
fi
AC_SUBST(MR_DOUBLE_IS_64_BIT)

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(whether long double is 64-bit)
AC_CACHE_VAL(mercury_cv_long_double_is_64_bit,
    MERCURY_TRY_STATIC_ASSERT(
        [#include <limits.h>],
        [sizeof(long double) * CHAR_BIT == 64],
        [mercury_cv_long_double_is_64_bit=yes],
        [mercury_cv_long_double_is_64_bit=no])
)
AC_MSG_RESULT($mercury_cv_long_double_is_64_bit)
if test "$mercury_cv_long_double_is_64_bit" = yes; then
    AC_DEFINE(MR_LONG_DOUBLE_IS_64_BIT)
fi
AC_SUBST(MR_LONG_DOUBLE_IS_64_BIT)

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING([whether we can use unboxed 64-bit integers])
AC_CACHE_VAL([mercury_cv_unboxed_int64s],
    MERCURY_TRY_STATIC_ASSERT(
        [#include <stdint.h>],
        [sizeof(uint64_t) == sizeof(void *)],
        [mercury_cv_unboxed_int64s=yes],
        [mercury_cv_unboxed_int64s=no])
)
AC_MSG_RESULT($mercury_cv_unboxed_int64s)
if test "$mercury_cv_unboxed_int64s" = yes; then
    HAVE_BOXED_INT64S="--unboxed-int64s"
else
    AC_DEFINE(MR_BOXED_INT64S)
    HAVE_BOXED_INT64S="--no-unboxed-int64s"
fi
AC_SUBST([HAVE_BOXED_INT64S])

#-----------------------------------------------------------------------------#

# The number of detstack slots (words) that a MR_SyncTerm occupies.
SYNC_TERM_SIZE=3
AC_SUBST(SYNC_TERM_SIZE)
AC_DEFINE_UNQUOTED(MR_SYNC_TERM_SIZE, $SYNC_TERM_SIZE)

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(whether architecture is big-endian)
AC_CACHE_VAL(mercury_cv_is_bigender,
    AC_TRY_RUN(
        [
            int main() {
                int     x = 1;
                unsigned char   *x_p;

                x_p = (unsigned char*) &x;

                if (*x_p == 0)
                    exit(0);
                else
                    exit(1);
            }
        ],
        [mercury_cv_is_bigender=yes],
        [mercury_cv_is_bigender=no])
)
AC_MSG_RESULT($mercury_cv_is_bigender)
if test "$mercury_cv_is_bigender" = yes; then
    AC_DEFINE(MR_BIG_ENDIAN)
fi
AC_SUBST(MR_BIG_ENDIAN)

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(whether architecture is little-endian)
AC_CACHE_VAL(mercury_cv_is_littleender,
    AC_TRY_RUN(
        [
            int main() {
                int     x = 1;
                unsigned char   *x_p;

                x_p = (unsigned char*) &x;

                if (*x_p == 1)
                    exit(0);
                else
                    exit(1);
            }
        ],
        [mercury_cv_is_littleender=yes],
        [mercury_cv_is_littleender=no])
)
AC_MSG_RESULT($mercury_cv_is_littleender)
if test "$mercury_cv_is_littleender" = yes; then
    AC_DEFINE(MR_LITTLE_ENDIAN)
fi
AC_SUBST(MR_LITTLE_ENDIAN)

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(whether we can use files as locks)
AC_CACHE_VAL(mercury_cv_have_ocreat_oexcl,
    AC_TRY_COMPILE(
        [
            #ifdef MR_HAVE_SYS_TYPES_H
            #include <sys/types.h>
            #endif
            #ifdef MR_HAVE_SYS_STAT_H
            #include <sys/stat.h>
            #endif
            #ifdef MR_HAVE_FCNTL_H
            #include <fcntl.h>
            #endif
        ],
        [
            #if defined(O_CREAT) && defined(O_EXCL)

            #else
                /* Deliberate syntax error. */
                }
            #endif
        ],
        [mercury_cv_have_ocreat_oexcl=yes],
        [mercury_cv_have_ocreat_oexcl=no])
)
AC_MSG_RESULT($mercury_cv_have_ocreat_oexcl)
if test "$mercury_cv_have_ocreat_oexcl" = yes; then
    AC_DEFINE(MR_HAVE_OCREAT_OEXCL)
fi

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(return values of system)
AC_CACHE_VAL(mercury_cv_normal_system_retval,
    AC_TRY_RUN(
        [
            #include <stdlib.h>
            #ifdef MR_HAVE_SYS_WAIT_H
            #include <sys/wait.h>
            #endif
            int main() {
                #if defined (WIFEXITED) && defined (WEXITSTATUS) && \
                    defined (WIFSIGNALED) && defined (WTERMSIG)
                /*
                ** All the necessary macros for handling the return values of
                ** system() are defined, so we do not need to test the return
                ** value of system()
                */
                exit(0);

                #else
                /*
                ** Normal return values from system() are considered to be
                ** when high 8 bits of the return value give the exit
                ** status, and the low 8 bits give the signal number which
                ** killed the process.
                */
                    if (system("exit 0") == 0 &&
                            system("exit 42") == 42 << 8 ) {
                            /* && system("kill -9 $$") == 9 */
                        exit(0);
                    } else {
                        exit(1);
                    }
                #endif
            }
        ],
        [mercury_cv_normal_system_retval=yes],
        [mercury_cv_normal_system_retval=no],
        AC_TRY_COMPILE(
            [
                #include <sys/wait.h>
            ],
            [
                #if defined (WIFEXITED) && defined (WEXITSTATUS) && \
                    defined (WIFSIGNALED) && defined (WTERMSIG)

                #else
                    /* Deliberate syntax error. */
                    }
                #endif
            ],
            [mercury_cv_normal_system_retval=yes],
            [mercury_cv_normal_system_retval=no])
    )
)
AC_MSG_RESULT($mercury_cv_normal_system_retval)
if test "$mercury_cv_normal_system_retval" = no; then
        # Warn since VC++6 compiler fails this test
    AC_MSG_WARN(Unable to interpret return values from system)
fi

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(to see if we can handle contexts blocking on IO)
AC_CACHE_VAL(mercury_cv_can_do_pending_io,
    AC_TRY_RUN(
        [
            #include <sys/types.h>
            #include <sys/time.h>
            int main() {
                fd_set f;
                struct timeval zero;
                int err;

                FD_ZERO(&f);
                zero.tv_sec = 0;
                zero.tv_usec = 0;
                err = select(1, &f, &f, &f, &zero);
                exit(err != 0);
            }
        ],
        [mercury_cv_can_do_pending_io=yes],
        [mercury_cv_can_do_pending_io=no])
)
AC_MSG_RESULT($mercury_cv_can_do_pending_io)
if test "$mercury_cv_can_do_pending_io" = yes; then
    AC_DEFINE(MR_CAN_DO_PENDING_IO)
fi
AC_SUBST(MR_CAN_DO_PENDING_IO)

#-----------------------------------------------------------------------------#
# There is a problem on some BSD based systems that FD_ZERO is defined
# defined in terms of bzero() but the appropriate header file for bzero()
# is not automatically included. The following deals with this situation
# on MacOS 10.3. (It appears to be fixed in MacOS 10.4.)

AC_MSG_CHECKING(to see if strings.h is needed for bzero)
AC_CACHE_VAL(mercury_cv_bzero_needs_strings_header, [
    save_CC="$CC"

    case "$CC" in
        *gcc*)
            CC="$CC -Wall -Werror"
            ;;
    esac

    cat > conftest.c << EOF
        #ifdef MR_BZERO_NEEDS_STRINGS_HEADER
            #include <strings.h>
        #endif
        #include <sys/types.h>
        #include <sys/time.h>
        #include <unistd.h>

        void MR_fd_zero(fd_set *fdset);

        void
        MR_fd_zero(fd_set *fdset)
        {
            FD_ZERO(fdset);
        }
EOF

    if $CC -c conftest.c < /dev/null >&AC_FD_CC 2>&1
    then
        mercury_cv_bzero_needs_strings_header=no
    else
        if $CC -DMR_BZERO_NEEDS_STRINGS_HEADER -c conftest.c \
            < /dev/null >&AC_FD_CC 2>&1
        then
            mercury_cv_bzero_needs_strings_header=yes
        else
            mercury_cv_bzero_needs_strings_header=no
        fi
    fi

    rm -f conftest*
    CC="$save_CC"
])

AC_MSG_RESULT($mercury_cv_bzero_needs_strings_header)
if test "$mercury_cv_bzero_needs_strings_header" = yes; then
    AC_DEFINE(MR_BZERO_NEEDS_STRINGS_HEADER)
fi
AC_SUBST(MR_BZERO_NEEDS_STRINGS_HEADER)

#-----------------------------------------------------------------------------#
#
# With some versions of gcc on Darwin __builtin_setjmp is broken.
# (See gcc bug 22099)

# XXX we should only disable the use of __builtin_setjmp for those
# versions of gcc that are broken - the problem will be fixed in gcc 4.2
case "$host" in
    i*86*apple*darwin*)
        AC_DEFINE(MR_DARWIN_SETJMP_WORKAROUND)
        ;;
esac

AC_SUBST(MR_DARWIN_SETJMP_WORKAROUND)

#-----------------------------------------------------------------------------#
#
# On sparc, x86, and probably on other architectures,
# if we're using non-local gotos, then for egcs 1.1, we need
# `-fno-gcse -fno-function-cse', and for gcc 2.8 we need `-fno-function-cse'.
# Otherwise gcc generates code which initializes registers in the function
# prologue and expects them to be valid later on, which doesn't work with
# non-local gotos, since we jump directly into the middle of a function.
# (Actually I don't know if `-fno-functions-cse' is really needed.
# Maybe you can get away without it. But better to err on the safe side...)
#
# For x86, x86-64 and possibly other architectures, when using non-local
# gotos with gcc 4.x we need `-fno-move-loop-invariants'.
#
# For x86-64 and possibly other architectures, when using gcc specific
# features and parallelism with gcc 4.6 and 4.7 (and possibly future
# versions), we need -fno-reorder-functions to avoid segmentation faults.
# We don't yet know exactly which grades are affected.
#
# For alpha, mips, and ARM, and probably on other architectures, when using
# non-local gotos we need -fomit-frame-pointer, otherwise when compiling
# with --no-c-optimize we run into a problem similar to the one above:
# Gcc generates code which initializes the frame pointer in the
# function prologue, and then expects it to be valid later on, which
# doesn't work with non-local gotos, since we jump directly into the
# middle of a function.
#
# For x86, gcc global registers only work with -fno-builtin
# and -fno-omit-frame-pointer.
#
# NB. Any changes here may also require changes to scripts/mgnuc.in.
#
CFLAGS_FOR_REGS=
CFLAGS_FOR_GOTOS=
LIBRARY_PATH_FOR_GOTOS=
case "$ac_cv_prog_gcc" in yes)
    case "`$CC --version < /dev/null`" in
        2.8*)
            CFLAGS_FOR_GOTOS="-fno-defer-pop -fno-function-cse"
            ;;
        *)
            CFLAGS_FOR_GOTOS="-fno-defer-pop -fno-function-cse -fno-gcse"
            ;;
    esac
    CFLAGS_FOR_GOTOS="$CFLAGS_FOR_GOTOS $CFLAGS_FOR_NO_MOVE_LOOP_INVARIANTS"
    case "$host" in
        mips-*)
            CFLAGS_FOR_GOTOS="$CFLAGS_FOR_GOTOS -fomit-frame-pointer"
            ;;
        i*86-*|x86_64*)
            # On x86-64 and x86, GCC labels do not work correctly with
            # -ftree-dominator-opts.
            CFLAGS_FOR_GOTOS="$CFLAGS_FOR_GOTOS $CFLAGS_FOR_NO_TREE_DOMINATOR_OPTS"
            CFLAGS_FOR_REGS="-fno-builtin -fno-omit-frame-pointer"
            ;;
        # We need a special-case hack here, because the auto-config gets it
        # wrong on the Alpha: the little example program works fine
        # in fast.gc mode, but big programs don't, due to problems with
        # the `gp' register. Ideally we ought to write a big enough example
        # program to trigger the problem, but that would be too much work.
        alpha*-dec-*)
            MERCURY_MSG(
"asm labels work on the Alpha but plain gcc labels do not")
            mercury_cv_asm_labels=yes
            mercury_cv_gcc_labels=no
            CFLAGS_FOR_GOTOS="$CFLAGS_FOR_GOTOS -fomit-frame-pointer"
            ;;
        # Similarly for rs6000.
        rs6000-*)
            MERCURY_MSG("gcc labels do not work on the RS/6000")
            mercury_cv_asm_labels=no
            mercury_cv_gcc_labels=no
            ;;
        arm*-*)
            CFLAGS_FOR_GOTOS="$CFLAGS_FOR_GOTOS -fomit-frame-pointer"
            ;;
        # On hppa64, ia64 and powerpc64 the test programs appear to work, but
        # larger programs die with an Illegal Instruction fault.
        hppa64-*)
            MERCURY_MSG("gcc labels do not work on HPPA64")
            mercury_cv_asm_labels=no
            mercury_cv_gcc_labels=no
            ;;
        ia64-*)
            MERCURY_MSG("gcc labels do not work on IA64")
            mercury_cv_asm_labels=no
            mercury_cv_gcc_labels=no
            ;;
        powerpc64-*)
            MERCURY_MSG("gcc labels do not work on PPC64")
            mercury_cv_asm_labels=no
            mercury_cv_gcc_labels=no
            ;;
        # On s390 the configure test goes into an infinite loop, so we need
        # to avoid running it.
        #
        # s390 systems can report themselves as s390-ibm-linux-gnu
        # or s390x-ibm-linux-gnu (and possibly others).
        s390*)
            MERCURY_MSG("gcc labels do not work on S/390")
            mercury_cv_asm_labels=no
            mercury_cv_gcc_labels=no
            ;;
    esac
    ;;
esac

# Determine if $CFLAGS_FOR_NO_REORDER_FUNCTIONS is needed.
case "$C_COMPILER_TYPE" in
    # Check if the option is not required, and otherwise include the option.
    # No need to check gcc < 3.3 as the option didn't exist.
    gcc_3* | gcc_4_1* | gcc_4_2* | gcc_4_3* | gcc_4_4* | gcc_4_5*)
        ;;
    *)
        # XXX Pass -fno-reorder-functions when using GCC 4.6 onwards with
        # the global register or non-local goto extensions. This works around
        # problems in *.par* grades. (See bug #294 for further details.)
        CFLAGS_FOR_REGS="$CFLAGS_FOR_REGS $CFLAGS_FOR_NO_REORDER_FUNCTIONS"
        CFLAGS_FOR_GOTOS="$CFLAGS_FOR_GOTOS $CFLAGS_FOR_NO_REORDER_FUNCTIONS"
        ;;
esac

# When we use nonlocal gotos, gcc generates warnings about labels of the form
# mercury__<modulename>__<predname> being used but not defined. The labels
# actually ARE defined, but with assembly code giving them names of the form
# _entry_mercury__<modulename>__<predname>. The assembly code makes them global
# symbols using the "_entry_" names. The generated code does refer to the
# labels using the "entry-less" names as well, and the gcc code generator
# processes those references just fine, but some other part of gcc seems
# to forget that fact when it generates the warnings. We therefore filter
# out those warnings with util/mfiltercc.
#
# Versions of gcc from 4.8 onward generate this warning not on a single line,
# which is what mfiltercc looks for, but as a very long multiline message,
# tracing the several macros involved, and putting arrow (carets) under their
# definitions, like this:
#
# /home/public/mercury-install-2014-01-10/lib/mercury/inc/mercury_goto.h:30:12: warning: ‘mercury__nrev__nreverse_2_0’ used but never defined [enabled by default]
#   MR_PASTE2(mercury__, label)
#             ^
# /home/public/mercury-install-2014-01-10/lib/mercury/inc/mercury_goto.h:881:14: note: in definition of macro ‘MR_declare_static’
#   static void label(void) __asm__("_entry_" MR_STRINGIFY(label))
#               ^
# /home/public/mercury-install-2014-01-10/lib/mercury/inc/mercury_std.h:268:26: note: in expansion of macro ‘MR_PASTE2_2’
#  #define MR_PASTE2(a,b)   MR_PASTE2_2(a,b)
#                           ^
# /home/public/mercury-install-2014-01-10/lib/mercury/inc/mercury_goto.h:30:2: note: in expansion of macro ‘MR_PASTE2’
#   MR_PASTE2(mercury__, label)
#   ^
# /home/public/mercury-install-2014-01-10/lib/mercury/inc/mercury_goto.h:1672:20: note: in expansion of macro ‘MR_add_prefix’
#   MR_declare_static(MR_add_prefix(e));
#                     ^
# nrev.c:79:1: note: in expansion of macro ‘MR_decl_static’
#  MR_decl_static(nrev__nreverse_2_0)
#  ^
#
# The problem is, mfiltercc removes the first line, but lets the rest
# through. This will confuse users even more than the unfiltered error
# message would, since they get the all the context but no information about
# the supposed "error" itself. For gcc versions that by default generate such
# long error messages, the inclusion of CFLAGS_FOR_ERRMSG_FILTER in
# CFLAGS_FOR_GOTOS will cause them to deviate from this default and switch
# them off.
#
# We *could* try to find a way to define labels that gcc does not complain
# about, at all. However, given that we *have* to use assembly code to make
# internal labels globally visible, any technique we could use to do that
# would be very vulnerable: it would be quite likely to be broken by new
# versions of gcc. Our current mechanism is vulnerable in the same way,
# but after such breakage, having to fix the mechanism to make it work
# is easier than having to fix it to make it work *and* to make it
# warning-free.
#
CFLAGS_FOR_GOTOS="$CFLAGS_FOR_GOTOS $CFLAGS_FOR_ERRMSG_FILTER"

AC_SUBST(CFLAGS_FOR_REGS)
AC_SUBST(CFLAGS_FOR_GOTOS)

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(whether we can use gcc labels)

# Set env vars for compiling with gcc non-local gotos
save_CC="$CC"
save_LIBRARY_PATH="$LIBRARY_PATH"
CC="$CC $CFLAGS_FOR_GOTOS"
if test "$LIBRARY_PATH_FOR_GOTOS" != ""; then
    LIBRARY_PATH="$LIBRARY_PATH_FOR_GOTOS:$LIBRARY_PATH"
    export LIBRARY_PATH
fi

AC_CACHE_VAL(mercury_cv_gcc_labels,
    AC_TRY_RUN(
        [
            extern void     exit(int);
            void            *entry_foo_1;
            void            *entry_bar_1;
            void            *succip;
            int             global;
            void            *dummy_identity_function(void *);

            foo() {
                entry_foo_1 = && foo_1;
                goto *dummy_identity_function(&&return_label);
            return_label:
                return;
            foo_1:
                __asm__ __volatile__("":::"memory");
                if (global != 42) exit(1);
                goto *entry_bar_1;
            }
            bar() {
                entry_bar_1 = && bar_1;
                goto *dummy_identity_function(&&return_label);
            return_label:
                return;
            bar_1:
                __asm__ __volatile__("":::"memory");
                if (global != 42) exit(1);
                goto *succip;
            }
            main() {
                global = 42;
                foo();
                bar();
                succip = &&last;
                goto *dummy_identity_function(&&return_label);
            return_label:
                goto *entry_foo_1;
                exit(1);
            last:
                __asm__ __volatile__("":::"memory");
                if (global != 42) exit(1);
                exit(0);
            }
            void *dummy_identity_function(void *p) {
                return p;
            }
        ],
        [mercury_cv_gcc_labels=yes],
        [mercury_cv_gcc_labels=no])
)
AC_MSG_RESULT($mercury_cv_gcc_labels)
if test "$mercury_cv_gcc_labels" = yes; then
    AC_DEFINE(MR_HAVE_GCC_LABELS)
fi

if test "$mercury_cv_gcc_labels" = yes; then
    AC_MSG_CHECKING(whether we can use asm labels)

    AC_CACHE_VAL(mercury_cv_asm_labels,
    AC_TRY_RUN(
        [
            void *volatile volatile_global_pointer;
            extern void mercury__label1 (void)
                __asm__("entry_" "mercury__label1" );
            main() {
                void *addr;

                volatile_global_pointer = &&mercury__label1;
                addr = &mercury__label1;
                goto *addr;
                exit(1);

            mercury__label1:
                __asm__(".globl entry_" "mercury__label1" "\n" "entry_"
                    "mercury__label1" ":\n"
                    "/* this is a comment */");
                exit(0);
            }
        ],
        [mercury_cv_asm_labels=yes],
        [mercury_cv_asm_labels=no])
    )
    AC_MSG_RESULT($mercury_cv_asm_labels)
else
    mercury_cv_asm_labels=${mercury_cv_asm_labels-no}
fi
HAVE_ASM_LABELS=$mercury_cv_asm_labels
AC_SUBST(HAVE_ASM_LABELS)
if test $HAVE_ASM_LABELS = 1; then
    AC_DEFINE(MR_HAVE_ASM_LABELS)
fi

# Restore env vars modified above
CC="$save_CC"
LIBRARY_PATH="$save_LIBRARY_PATH"

#-----------------------------------------------------------------------------#
# We need to ensure that runtime/mercury_conf.h exists, since some of the
# programs we attempt to compile below indirectly include it.

test -f runtime/mercury_conf.h || {
    cat > runtime/mercury_conf.h <<EOF
#define MR_WORD_TYPE $MR_WORD_TYPE
EOF
}

#-----------------------------------------------------------------------------#

if test $mercury_cv_gcc_labels = yes || test $mercury_cv_asm_labels = yes; then
    AC_MSG_CHECKING(whether we can use gcc labels and global registers)

    # Set env vars for compiling with gcc non-local gotos and registers
    save_CC="$CC"
    save_LIBRARY_PATH="$LIBRARY_PATH"
    CC="$CC $CFLAGS_FOR_GOTOS $CFLAGS_FOR_REGS"
    if test "$LIBRARY_PATH_FOR_GOTOS" != ""; then
        LIBRARY_PATH="$LIBRARY_PATH_FOR_GOTOS:$LIBRARY_PATH"
        export LIBRARY_PATH
    fi

    AC_CACHE_VAL(mercury_cv_gcc_model_fast,
    AC_TRY_RUN(
        [
            #define MR_USE_GCC_NONLOCAL_GOTOS
            #define MR_USE_GCC_GLOBAL_REGISTERS
            #include "mercury_engine.h"
            changequote(<<,>>)
            MercuryEngine MR_engine_base;
            changequote([,])
            main() {
                MR_mr0 = 20;
                MR_mr7 = 22;
                if (MR_mr0 + MR_mr7 != 42)
                    exit(1);
                exit(0);
            }
        ],
        [mercury_cv_gcc_model_fast=yes],
        [mercury_cv_gcc_model_fast=no])
    )
    AC_MSG_RESULT($mercury_cv_gcc_model_fast)

    # Restore env vars modified above
    CC="$save_CC"
    LIBRARY_PATH="$save_LIBRARY_PATH"
else
    mercury_cv_gcc_model_fast=no
fi

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(whether we can use global registers without gcc labels)

# Set env vars for compiling with gcc registers
save_CC="$CC"
CC="$CC $CFLAGS_FOR_REGS"

AC_CACHE_VAL(mercury_cv_gcc_model_reg,
AC_TRY_RUN(
    [
        #define MR_USE_GCC_GLOBAL_REGISTERS
        #include "mercury_engine.h"
        changequote(<<,>>)
        MercuryEngine MR_engine_base;
        changequote([,])
        main() {
            MR_mr0 = 20;
            MR_mr7 = 22;
            if (MR_mr0 + MR_mr7 != 42)
                exit(1);
            exit(0);
        }
    ],
    [mercury_cv_gcc_model_reg=yes],
    [mercury_cv_gcc_model_reg=no])
)
AC_MSG_RESULT($mercury_cv_gcc_model_reg)

# Restore env vars modified above
CC="$save_CC"

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(whether we can support time profiling on this system)
AC_CACHE_VAL(mercury_cv_profiling,
AC_TRY_CPP(
    [
        /* The following code comes from runtime/mercury_timing.h */
        #include    <errno.h>
        #include    <limits.h>
        #include    <string.h>
        #include    <signal.h>

        #ifdef MR_HAVE_SYS_PARAM_H
        #include    <sys/param.h>
        #endif

        #ifdef MR_HAVE_UNISTD_H
        #include    <unistd.h>
        #endif

        #ifdef MR_HAVE_SYS_TIME_H
        #include    <sys/time.h>
        #endif

        #ifdef HZ
          #define MR_CLOCK_TICKS_PER_SECOND     HZ
        #elif defined(MR_HAVE_SYSCONF) && defined(_SC_CLK_TCK)
          #define MR_CLOCK_TICKS_PER_SECOND     ((int) sysconf(_SC_CLK_TCK))
        #elif defined(CLOCKS_PER_SEC)
          #define MR_CLOCK_TICKS_PER_SECOND     CLOCKS_PER_SEC
        #elif defined(CLK_TCK)
          #define MR_CLOCK_TICKS_PER_SECOND     CLK_TCK
        #else
          /* just leave it undefined */
        #endif

        #if !defined(MR_CLOCK_TICKS_PER_SECOND) || \
            !defined(SIGPROF) || \
            !defined(MR_HAVE_SETITIMER)
        #error "Time profiling not supported on this system"
        #endif
    ],
    [mercury_cv_profiling=yes],
    [mercury_cv_profiling=no]))
AC_MSG_RESULT($mercury_cv_profiling)

#-----------------------------------------------------------------------------#
#
# Add an option that disables the deep profiler
#

AC_ARG_ENABLE(deep-profiler,
   AC_HELP_STRING([--enable-deep-profiler=<directory>],
                  [install deep profiler CGI script in <directory>]),
   enable_deep_profiler="$enableval",enable_deep_profiler=default)

AC_MSG_CHECKING(whether to enable the deep profiler)

# We test for all the features needed by the deep profiler.
if test "$MR_HAVE_SYS_TYPES_H" = 1 && \
    test "$MR_HAVE_SYS_STAT_H" = 1 && \
    test "$MR_HAVE_FCNTL_H" = 1 && \
    test "$MR_HAVE_UNISTD_H" = 1 && \
    test "$MR_HAVE_DIRENT_H" = 1 && \
    test "$ac_cv_func_getpid" = yes && \
    test "$ac_cv_func_fork" = yes && \
    test "$ac_cv_func_opendir" = yes && \
    test "$ac_cv_func_readdir" = yes && \
    test "$ac_cv_func_closedir" = yes && \
    test "$mercury_cv_have_ocreat_oexcl" = yes
then
    mercury_cv_can_enable_deep_profiler=yes
    AC_DEFINE(MR_DEEP_PROFILER_ENABLED)
else
    mercury_cv_can_enable_deep_profiler=no
fi

case "$host" in
    *apple*darwin*)
        mercury_cv_default_cgi_dir=/Library/WebServer/CGI-Executables
        ;;
    *)
        mercury_cv_default_cgi_dir=/usr/lib/cgi-bin
        ;;
esac

mercury_cv_cgi_dir_must_exist=no
case "$enable_deep_profiler" in
    default)
        mercury_cv_enable_deep_profiler=$mercury_cv_can_enable_deep_profiler
        mercury_cv_cgi_dir=$mercury_cv_default_cgi_dir
        ;;
    no)
        mercury_cv_enable_deep_profiler=no
        ;;
    *)
        if test $enable_deep_profiler = yes; then
            mercury_cv_cgi_dir=$mercury_cv_default_cgi_dir
        else
            mercury_cv_cgi_dir=$enable_deep_profiler
            mercury_cv_cgi_dir_must_exist=yes
        fi

        if test $mercury_cv_can_enable_deep_profiler = no; then
            AC_MSG_RESULT($enable_deep_profiler)
            AC_MSG_ERROR(
[--enable-deep-profiler specified but system
    does not support it])
        fi

        mercury_cv_enable_deep_profiler=yes
        ;;
esac

AC_MSG_RESULT($mercury_cv_enable_deep_profiler)

if test $mercury_cv_enable_deep_profiler = yes; then
    AC_MSG_CHECKING([where to install deep profiler CGI script])
    AC_MSG_RESULT([$mercury_cv_cgi_dir])
    if test -d $mercury_cv_cgi_dir; then
        true
    else
        if test $mercury_cv_cgi_dir_must_exist = yes; then
            AC_MSG_ERROR([$mercury_cv_cgi_dir does not exist.])
        else
            AC_MSG_WARN([$mercury_cv_cgi_dir does not exist.])
        fi
    fi
fi

ENABLE_DEEP_PROFILER=$mercury_cv_enable_deep_profiler
AC_SUBST(ENABLE_DEEP_PROFILER)
CGIDIR=$mercury_cv_cgi_dir
AC_SUBST(CGIDIR)

#-----------------------------------------------------------------------------#

# Check if we are using a pthreads implementation on Win32.
# NOTE: we currently only support this on MinGW or MinGW-w64.

if test "$MR_HAVE_PTHREAD_H" = 1
then
    case "$host" in
        *mingw*)
            MERCURY_HAVE_PTHREADS_WIN32
            if test $mercury_cv_have_pthreads_win32 = "yes"
            then
                AC_DEFINE([MR_PTHREADS_WIN32])
            fi
            WIN32_GC_THREADLIB="-DGC_WIN32_PTHREADS"
            ;;
        *)
            # XXX Building with GC_WIN32_THREADS instead of GC_WIN32_PTHREADS
            # can lead to trouble if the Mercury implementation makes pthread
            # calls which are not intercepted by Boehm GC.
            WIN32_GC_THREADLIB="-DGC_WIN32_THREADS"
            ;;
    esac
else
    WIN32_GC_THREADLIB="-DGC_WIN32_THREADS"
fi

AC_ARG_WITH([gc-win32-pthreads],
   AC_HELP_STRING([--with-gc-win32-pthreads],
            [Force the use of pthreads-win32 or winpthreads with Boehm GC.
             This is the default for MinGW if detected.]),
   [with_gc_win32_pthreads="$withval"],
   [with_gc_win32_pthreads="no"])

case "$with_gc_win32_pthreads" in
    yes)
        # Force the use of a MinGW* pthreads library with the Boehm GC.
        WIN32_GC_THREADLIB="-DGC_WIN32_PTHREADS"
        ;;
    no)
        ;;
    *)
        AC_MSG_ERROR([unexpected argument to --with-gc-win32-pthreads])
        ;;
esac

#-----------------------------------------------------------------------------#

# Figure out what options we need to pass to the C compiler for multithreading.
# We may need to pass different options to tell (a) the Mercury runtime and
# library (b) the Boehm collector and (c) the C library that they need to be
# thread-safe.
#
# For Linux, Solaris, and HPUX,
# the thread-enabled version of the Boehm
# collector contains a reference to dlopen(), so
# if threads are enabled, we need to link with the
# appropriate extra library for that (-ldl on Linux
# and Solaris and -lrt on HPUX).
# XXX That should only be done if conservative GC
#     is enabled.
#
# Note that changes here may require changes in scripts/ml.in.
#
LDFLAGS_FOR_THREADS=
LD_LIBFLAGS_FOR_THREADS=
ENABLE_BOEHM_THREAD_LOCAL_ALLOC=
ENABLE_BOEHM_PARALLEL_MARK=

# This following variable is used for passing any other C compiler
# flags to the version of the Boehm GC built in parallel grades.
#
BOEHM_CFLAGS_FOR_THREADS="-DGC_THREADS"

# This should be set if both the system malloc and Boehm GC (in the default
# configuration) may both call sbrk.
avoid_sbrk=

case "$host" in
    *solaris*)
        CFLAGS_FOR_THREADS="-DGC_THREADS -D_REENTRANT"
        THREAD_LIBS="-lpthread -lrt -ldl"
        ENABLE_BOEHM_THREAD_LOCAL_ALLOC="-DTHREAD_LOCAL_ALLOC"
        ENABLE_BOEHM_PARALLEL_MARK="-DPARALLEL_MARK"
        ;;

    *linux*)
        # Note that for old versions of Linux / glibc,
        # you may also need to make sure that you don't
        # pass -ansi to gcc.
        CFLAGS_FOR_THREADS="-DGC_THREADS -D_THREAD_SAFE -D_REENTRANT"
        THREAD_LIBS="-lpthread -ldl"
        ENABLE_BOEHM_THREAD_LOCAL_ALLOC="-DTHREAD_LOCAL_ALLOC"
        ENABLE_BOEHM_PARALLEL_MARK="-DPARALLEL_MARK"
        # HBLKSIZE and CPP_LOG_HBLKSIZE control a poorly-documented parameter
        # in the Boehm GC, this parameter indirectly controls the size of
        # thread-local free lists among other things.
        # CPP_LOG_HBLKSIZE must always be the base-2 log of HBLKSIZE.
        # (With the current collector these settings only appear to work
        # correctly on Linux.)
        # XXX disabled as it aborts on GC_free when --enable-gc-munmap is used
        # BOEHM_CFLAGS_FOR_THREADS="-DGC_THREADS -DHBLKSIZE=32768 \
        # -DCPP_LOG_HBLKSIZE=15"
        avoid_sbrk=yes
        ;;

    *cygwin*)
        THREAD_LIBS=""
        case "$mercury_cv_cc_type" in
            msvc)
                CFLAGS_FOR_THREADS="-DGC_THREADS -MD"
                LDFLAGS_FOR_THREADS="-MD"
                LD_LIBFLAGS_FOR_THREADS="-MD"
                ;;
            *)
                CFLAGS_FOR_THREADS="$WIN32_GC_THREADLIB"
                BOEHM_CFLAGS_FOR_THREADS="-DGC_THREADS $WIN32_GC_THREADLIB"
                ENABLE_BOEHM_THREAD_LOCAL_ALLOC="-DTHREAD_LOCAL_ALLOC"
                ENABLE_BOEHM_PARALLEL_MARK="-DPARALLEL_MARK"
                ;;
        esac
        # avoid_sbrk?
        ;;

    i*86-pc-mingw*)
        THREAD_LIBS=""
        case "$mercury_cv_cc_type" in
            msvc)
                CFLAGS_FOR_THREADS="-DGC_THREADS -MD"
                LDFLAGS_FOR_THREADS="-MD"
                LD_LIBFLAGS_FOR_THREADS="-MD"
                ;;
            *)
                if test "$MR_HAVE_PTHREAD_H" = 1
                then
                    # By default the MinGW port of GCC targets the i386
                    # architecture. We need to tell it to target a later
                    # architecture for the GCC built-in atomic ops to be
                    # available.
                    CFLAGS_FOR_THREADS="$WIN32_GC_THREADLIB -march=i686"
                    BOEHM_CFLAGS_FOR_THREADS="-DGC_THREADS $WIN32_GC_THREADLIB"
                    THREAD_LIBS="-lpthread"
                else
                    # The compiler is unconditionally linked against the thread
                    # libraries so if pthreads is not present then we need to
                    # set THREAD_LIBS to empty in order to avoid linker errors.
                    CFLAGS_FOR_THREADS="$WIN32_GC_THREADLIB"
                    BOEHM_CFLAGS_FOR_THREADS="-DGC_THREADS $WIN32_GC_THREADLIB"
                    THREAD_LIBS=
                fi
                ENABLE_BOEHM_THREAD_LOCAL_ALLOC="-DTHREAD_LOCAL_ALLOC"
                ENABLE_BOEHM_PARALLEL_MARK="-DPARALLEL_MARK"
                ;;
        esac
        ;;

    *-w64-mingw*)
        CFLAGS_FOR_THREADS="$WIN32_GC_THREADLIB"
        BOEHM_CFLAGS_FOR_THREADS="-DGC_THREADS $WIN32_GC_THREADLIB"
        THREAD_LIBS="-lpthread"
        ENABLE_BOEHM_THREAD_LOCAL_ALLOC="-DTHREAD_LOCAL_ALLOC"
        ENABLE_BOEHM_PARALLEL_MARK="-DPARALLEL_MARK"
        ;;

    *apple*darwin*)
        CFLAGS_FOR_THREADS="-DGC_THREADS"
        THREAD_LIBS=""
        ENABLE_BOEHM_THREAD_LOCAL_ALLOC="-DTHREAD_LOCAL_ALLOC"
        ENABLE_BOEHM_PARALLEL_MARK="-DPARALLEL_MARK"
        ;;

    *-freebsd*)
        CFLAGS_FOR_THREADS="-DGC_THREADS"
        THREAD_LIBS="-lpthread"
        ENABLE_BOEHM_THREAD_LOCAL_ALLOC="-DTHREAD_LOCAL_ALLOC"
        ENABLE_BOEHM_PARALLEL_MARK="-DPARALLEL_MARK"
        avoid_sbrk=yes
        ;;

    *-openbsd*)
        CFLAGS_FOR_THREADS="-DGC_THREADS"
        THREAD_LIBS="-lpthread"
        ENABLE_BOEHM_THREAD_LOCAL_ALLOC="-DTHREAD_LOCAL_ALLOC"
        ENABLE_BOEHM_PARALLEL_MARK="-DPARALLEL_MARK"
        avoid_sbrk=yes
        ;;

### # The threads stuff on HPUX is not yet enabled because
### # we don't have an HPUX box to test it on.
### *hpux*)
###     CFLAGS_FOR_THREADS="" # XXX incomplete
###     THREAD_LIBS="-lpthread -lrt" ;;

    *)
        # Multithreading not (yet) supported on other architectures.
        # For multithreading, we need (1) Posix threads and
        # (2) a port of the Boehm gc for that architecture
        # that works with threads.
        CFLAGS_FOR_THREADS=""
        BOEHM_CFLAGS_FOR_THREADS=""
        ;;
esac

if test "$avoid_sbrk" = yes; then
    if test "$ac_cv_func_mmap" = yes; then
        BOEHM_CFLAGS_FOR_THREADS="$BOEHM_CFLAGS_FOR_THREADS -DUSE_MMAP"
        if test "$enable_gc_mmap" = no; then
            AC_MSG_NOTICE([using mmap for Boehm GC in threaded grades])
        fi
    else
        AC_MSG_WARN([malloc and Boehm GC both use sbrk; probably unsafe!])
    fi
fi

AC_SUBST(CFLAGS_FOR_THREADS)
AC_SUBST(THREAD_LIBS)
AC_SUBST(LDFLAGS_FOR_THREADS)
AC_SUBST(LD_LIBFLAGS_FOR_THREADS)
AC_SUBST(ENABLE_BOEHM_THREAD_LOCAL_ALLOC)
AC_SUBST(ENABLE_BOEHM_PARALLEL_MARK)
AC_SUBST(BOEHM_CFLAGS_FOR_THREADS)

save_LIBS="$LIBS"
LIBS="$THREAD_LIBS"
mercury_check_for_functions \
        pthread_mutexattr_setpshared
LIBS="$save_LIBS"

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(whether we can use thread-local storage class extension)
AC_CACHE_VAL(mercury_cv_thread_local_storage,
    AC_TRY_COMPILE(
        [],
        [static __thread int thread_local_variable;],
        [mercury_cv_thread_local_storage=yes],
        [mercury_cv_thread_local_storage=no])
)
AC_MSG_RESULT($mercury_cv_thread_local_storage)

if test "$mercury_cv_thread_local_storage" = yes; then
    case "$host" in
        *-solaris2.*)
            # Thread-local storage is disabled on Solaris as there is a linker
            # problem with shared objects and TLS.
            ;;
        *apple*darwin*)
            # XXX only attempt to use __thread with GCC on Mac OS X.
            # clang does not support it, although the above test "passes"
            # with some versions of clang.
            case "$C_COMPILER_TYPE" in
                gcc*)
                    AC_DEFINE([MR_THREAD_LOCAL_STORAGE])
                    ;;
            esac
            ;;
        *-openbsd*)
            # The above test "passes" but OpenBSD (6.2) does not support TLS
            # natively, so is emulated by gcc/clang using something like the
            # pthread thread-specific data API. Moreover, something goes wrong
            # with the emulation when a Mercury program is dynamically linked
            # to the Mercury runtime (MR_thread_engine_base is assigned in one
            # place but remains NULL when read elsewhere).
            ;;
        *)
            AC_DEFINE([MR_THREAD_LOCAL_STORAGE])
            ;;
    esac
fi

#-----------------------------------------------------------------------------#

# It is necessary to pass -DNO_GETCONTEXT and -DHAVE_DL_ITERATE_PHDR when
# building Boehm GC with musl libc.

case $ac_cv_func_getcontext in
    yes) BOEHM_NO_GETCONTEXT= ;;
    no)  BOEHM_NO_GETCONTEXT="-DNO_GETCONTEXT" ;;
esac
AC_SUBST(BOEHM_NO_GETCONTEXT)

case $ac_cv_func_dl_iterate_phdr in
    yes) BOEHM_HAVE_DL_ITERATE_PTR="-DHAVE_DL_ITERATE_PHDR" ;;
    no)  BOEHM_HAVE_DL_ITERATE_PTR= ;;
esac
AC_SUBST(BOEHM_HAVE_DL_ITERATE_PTR)

#-----------------------------------------------------------------------------#

# On some systems a prefix is attached to exported symbols. We need to account
# for this when checking object files for namespace cleanliness.

SYMPREFIX=

case "$host" in
    *apple*darwin*) SYMPREFIX="_" ;;
    i*86-*-mingw*)  SYMPREFIX="_" ;;
esac

AC_SUBST([SYMPREFIX])

#-----------------------------------------------------------------------------#

#
# Figure out which is the best grade to use for various different purposes.
# In particular, choose the default grade for compiling applications.
#

# BEST_LLDS_BASE_GRADE is the most efficient of the LLDS->C base grades
# that can be supported. If the user explicitly chooses a base grade
# using --with-llds-base-grade then we use that instead.
#
# NOTE: the base grade can also be one of asm_jump, fast and jump.
# However, we do not publicly document those grades or encourage their
# use by non-developers.

AC_ARG_WITH([llds-base-grade],
   AC_HELP_STRING([--with-llds-base-grade=<base_grade>],
          [Force the use of <base_grade> as the base LLDS grade.
          <base_grade> must be one of asm_fast, reg or none]),
   [user_llds_base_grade="$withval";mercury_cv_user_base_grade=yes],
   [mercury_cv_user_base_grade=no]
)

case "$mercury_cv_user_base_grade" in
    yes)
        case "$user_llds_base_grade" in
            asm_fast|asm_jump|fast|jump|reg|none)
                BEST_LLDS_BASE_GRADE="$user_llds_base_grade"
                ;;

            yes)
                AC_MSG_ERROR(
[missing argument to --with-llds-base-grade=... option])
                ;;

            no)
               AC_MSG_ERROR([invalid option --without-llds-base-grade])
                ;;

            *)
                AC_MSG_ERROR(
[invalid argument to --with-llds-base-grade: $user_llds_base_grade])
                ;;
            esac
            ;;
    no)
        if test $mercury_cv_asm_labels = yes -a \
                $mercury_cv_gcc_model_fast = yes
        then
            BEST_LLDS_BASE_GRADE=asm_fast
        elif test $mercury_cv_gcc_model_reg = yes; then
            BEST_LLDS_BASE_GRADE=reg
        else
            BEST_LLDS_BASE_GRADE=none
        fi

        # We used to force the use of the none base grade if the C compiler
        # is GCC 4.{1,2,3,4}, as the asm_fast and reg grades had problems
        # with those versions. This is now fixed but this code may be useful
        # in the future.
        # case "$mercury_cv_gcc_version" in
        #     4_1_*|4_2_*|4_3_*|4_4_*)
        #         BEST_LLDS_BASE_GRADE=none
        #     ;;
        # esac
     ;;
esac

# Force the use of the none grade on Apple x86_64 systems when using
# GCC 4.1 or 4.2. The asm_fast and reg grades do not work with those versions.
# They do work properly on x86_64 systems with GCC version 4.3 onwards.
#
case "$C_COMPILER_TYPE" in
    gcc_4_1_*|gcc_4_2_*)
        case "$host" in x86_64*apple*darwin*) BEST_LLDS_BASE_GRADE=none ;;
    esac
esac

# Force the use of the none grade on x86/Darwin systems.
# XXX we should check whether asm_fast or reg work with recent GCCs.
# The asm_fast and reg grades also do not currently work on Linux/PPC.
#
case "$host" in
    i*86*apple*darwin*|powerpc*-linux-gnu)
        BEST_LLDS_BASE_GRADE=none
        ;;
esac

# BEST_DEBUG_BASE_GRADE is the most efficient base grade that supports
# debugging (`--trace deep').

BEST_DEBUG_BASE_GRADE=$BEST_LLDS_BASE_GRADE

# DEFAULT_BASE_GRADE is the best overall base grade; it should be reasonably
# efficient, and it should support debugging unless that would compromise
# efficiency too much.

case $BEST_LLDS_BASE_GRADE in
    none|fast|jump)
        DEFAULT_BASE_GRADE=hlc
        ;;
    asm_fast|asm_jump|reg)
        DEFAULT_BASE_GRADE=$BEST_LLDS_BASE_GRADE
        ;;
    *)
        AC_MSG_ERROR(invalid BEST_LLDS_BASE_GRADE $BEST_LLDS_BASE_GRADE)
        ;;
esac

# DEFAULT_GRADE is the default grade used to compile applications.
# This is the same as DEFAULT_BASE_GRADE except that we also want to
# enable conservative garbage collection by default.
# We also allow users to override the default grade by setting
# "DEFAULT_GRADE" in the environment before invoking configure.

AC_ARG_WITH(default-grade,
    AC_HELP_STRING([--with-default-grade=GRADE],
                   [Have the built compiler default to using GRADE]),
    [case "$withval" in
        # Handle `--with-default-grade' (no `=GRADE') and
        # `--without-default-grade' flags.
        yes|no)
            AC_MSG_ERROR(
[Must supply a valid grade if using --with-default-grade.])
            ;;
        *)
            # Perhaps do better checking on the supplied value here.
            DEFAULT_GRADE="$withval"
            export DEFAULT_GRADE
            ;;
     esac], [])

if test "$DEFAULT_GRADE" = ""; then
    DEFAULT_GRADE=$DEFAULT_BASE_GRADE.gc
fi

MERCURY_MSG("using grade \`$DEFAULT_GRADE' as the default grade for applications")
AC_SUBST(DEFAULT_GRADE)

#-----------------------------------------------------------------------------#
#
# Choose which grade to use for the compiler
#

# BEST_GRADE_FOR_COMPILER is the best grade for compiling
# the Mercury compiler, if there is already an installed Mercury compiler.

case $BEST_LLDS_BASE_GRADE in
    asm_fast)
        BEST_GRADE_FOR_COMPILER=asm_fast.gc
        ;;
    *)
        BEST_GRADE_FOR_COMPILER=hlc.gc
        ;;
esac

# BOOTSTRAP_GRADE is the most efficient grade which we can use to bootstrap the
# compiler from the source distribution, if there is no installed Mercury
# compiler. This grade needs to be one for which the C files are compatible
# with the C files that we ship in the source distribution. The C files in the
# source distribution are generated in hlc.gc.pregen or asm_fast.gc.pregen.
# In the former case, we _must_ use the grade hlc.gc.pregen, in the latter case
# we can, in principle, use any of the base LLDS grades (it's the same C code).
# We don't use fast and jump as they don't support code addresses in static
# initializers and we don't use asm_jump as it isn't regularly tested.

if test "$BOOTSTRAP_HLC" = "yes"
then
    BOOTSTRAP_GRADE=hlc.gc.pregen
else
    case $BEST_LLDS_BASE_GRADE in
        asm_jump|jump|fast)
            if test $mercury_cv_gcc_model_reg = yes; then
                BOOTSTRAP_GRADE=reg.gc.pregen
            else
                BOOTSTRAP_GRADE=none.gc.pregen
            fi
            ;;
        *)
            BOOTSTRAP_GRADE=$BEST_LLDS_BASE_GRADE.gc.pregen
            ;;
    esac
fi

# Choose whether to use $BOOTSTRAP_GRADE or $BEST_GRADE_FOR_COMPILER.
# Previously built C files will be checked for compatibility
# with these settings, and otherwise removed, later.

if test "$BOOTSTRAP_MC" = ""; then
    GRADE="$BOOTSTRAP_GRADE"
    AC_MSG_WARN(
[Mercury compiler not yet installed -
****    cannot use grade \`$BEST_GRADE_FOR_COMPILER'
****    using grade \`$GRADE' to compile the compiler
****    after installation is complete you may reinstall
****    from scratch to have a faster compiler.
****    NOTE: before reinstalling from scratch, ensure that the
****    source tree is clean by running \`make realclean'. Not doing
****    this may result in a broken Mercury installation.])
else
    GRADE=$BEST_GRADE_FOR_COMPILER
    MERCURY_MSG("using grade \`$GRADE' to compile the compiler")
fi
AC_SUBST(GRADE)

#-----------------------------------------------------------------------------#
#
# Define options for selecting which grades of the library to install
#

AC_ARG_ENABLE(inefficient-grades,
    AC_HELP_STRING([--enable-inefficient-grades],
                   [install inefficient versions of the library]),
    enable_inefficient_grades="$enableval",enable_inefficient_grades=no)

AC_ARG_ENABLE(most-grades,
    AC_HELP_STRING([--disable-most-grades],
                   [install only the essential versions of the library]),
    enable_most_grades="$enableval",enable_most_grades=yes)

AC_ARG_ENABLE(nogc-grades,
    AC_HELP_STRING([--enable-nogc-grades],
                   [install no-gc versions of the library]),
    enable_nogc_grades="$enableval",enable_nogc_grades=no)

# Not publicly documented since the agc grades are currently broken.
#
#  --enable-agc-grades     install accurate GC versions of the library
AC_ARG_ENABLE(agc-grades, [],
    enable_agc_grades="$enableval",enable_agc_grades=no)

AC_ARG_ENABLE(debug-grades,
    AC_HELP_STRING([--disable-debug-grades],
                   [do not install the debugging versions of the library]),
    enable_debug_grades="$enableval",enable_debug_grades=yes)

AC_ARG_ENABLE(decl-debug-grades,
    AC_HELP_STRING([--disable-decl-debug-grades],
                   [do not install the declarative debugging versions
                    of the library]),
    enable_decl_debug_grades="$enableval",enable_decl_debug_grades=yes)

AC_ARG_ENABLE(ssdebug-grades,
    AC_HELP_STRING([--enable-ssdebug-grades],
                   [install the ssdebug versions of the library]),
    enable_ssdebug_grades="$enableval",enable_ssdebug_grades=no)

AC_ARG_ENABLE(prof-grades,
    AC_HELP_STRING([--disable-prof-grades],
                   [do not install profiling versions of the library]),
    enable_prof_grades="$enableval",enable_prof_grades=yes)

AC_ARG_ENABLE(old-prof-grades,
    AC_HELP_STRING([--disable-old-prof-grades],
                   [do not install non-deep profiling versions of the library]),
    enable_old_prof_grades="$enableval",enable_old_prof_grades=yes)

AC_ARG_ENABLE(trail-grades,
    AC_HELP_STRING([--disable-trail-grades],
                   [do not install trailing versions of the library]),
    enable_trail_grades="$enableval",enable_trail_grades=yes)

AC_ARG_ENABLE(par-grades,
    AC_HELP_STRING([--disable-par-grades],
                   [do not install thread-safe versions of the library]),
    enable_par_grades="$enableval",enable_par_grades=yes)

AC_ARG_ENABLE(mm-grades,
    AC_HELP_STRING([--enable-mm-grades],
                   [install minimal model versions of the library]),
    enable_mm_grades="$enableval",enable_mm_grades=no)

AC_ARG_ENABLE(mmos-grades,
    AC_HELP_STRING([--enable-mmos-grades],
                   [install experimental minimal model own stack versions of the library]),
    enable_mmos_grades="$enableval",enable_mmos_grades=no)

AC_ARG_ENABLE(dmm-grades,
   AC_HELP_STRING([--enable-dmm-grades],
                  [install minimal model debug versions of the library]),
    enable_dmm_grades="$enableval",enable_dmm_grades=no)

AC_ARG_ENABLE(hlc-prof-grades,
   AC_HELP_STRING([--enable-hlc-prof-grades],
                  [install profiling versions of the high level C grade]),
   enable_hlc_prof_grades="$enableval",enable_hlc_prof_grades=no)

AC_ARG_ENABLE(hlc-low-level-debug-grades,
   AC_HELP_STRING([--enable-hlc-low-level-debug-grades],
                  [install a low-level debugging version of the high level C grade]),
   enable_hlc_ll_debug_grades="$enableval",enable_hlc_ll_debug_grades=no)

AC_ARG_ENABLE(rbmm-grades,
   AC_HELP_STRING([--enable-rbmm-grades],
                  [install experimental region-based memory management versions of the library]),
   enable_rbmm_grades="$enableval",enable_rbmm_grades=no)

AC_ARG_ENABLE(stseg-grades,
    AC_HELP_STRING([--enable-stseg-grades],
        [install stack segments versions of the library]),
    [enable_stseg_grades="$enableval"],[enable_stseg_grades="no"])

AC_ARG_ENABLE(minimal-install,
    AC_HELP_STRING([--enable-minimal-install],
        [install the minimal system required to bootstrap Mercury]),
    [enable_min_install="$enableval"],[enable_min_install="no"])

AC_ARG_ENABLE(csharp-grade,
   AC_HELP_STRING([--disable-csharp-grade], [do not install the C# grade]),
   enable_csharp_grade="$enableval",enable_csharp_grade=yes)

AC_ARG_ENABLE(java-grade,
   AC_HELP_STRING([--disable-java-grade], [do not install the Java grade]),
   enable_java_grade="$enableval",enable_java_grade=yes)

# We don't enable the Erlang grades by default because it is still
# experimental.
AC_ARG_ENABLE(erlang-grade,
   AC_HELP_STRING([--enable-erlang-grade], [install the Erlang grade]),
   enable_erlang_grade="$enableval",enable_erlang_grade=no)

AC_ARG_ENABLE(libgrades,
    AC_HELP_STRING([--enable-libgrades=...],
            [install exactly the given versions of the library.
            The versions are specified using a comma-separated
            list.]),
    enable_libgrades_given=yes;enable_libgrades="$enableval",enable_libgrades_given=no)

AC_ARG_ENABLE(additional-libgrades,
    AC_HELP_STRING([--enable-additional-libgrades=...],
            [install the given versions of the library in addition
            to those requested by other options. The versions are
            specified using a comma-separated list.]),
    enable_additional_libgrades_given=yes;enable_additional_libgrades="$enableval",enable_additional_libgrades_given=no)

if test "$enable_most_grades" = no; then
    enable_inefficient_grades=no
    enable_nogc_grades=no
    enable_agc_grades=no
    enable_prof_grades=no
    enable_old_prof_grades=no
    enable_trail_grades=no
    enable_mm_grades=no
    enable_mmos_grades=no
    enable_dmm_grades=no
    enable_hlc_prof_grades=no
    enable_hlc_ll_debug_grades=no
    enable_rbmm_grades=no
    enable_par_grades=no
    enable_stseg_grades=no
    enable_csharp_grade=no
    enable_java_grade=no
    enable_erlang_grade=no
fi

#-----------------------------------------------------------------------------#
#
# Choose which grades of the library to install
#

# Compute all the LLDS base grades.
ALL_LLDS_BASE_GRADES="none"
if test $mercury_cv_gcc_model_reg = yes; then
    ALL_LLDS_BASE_GRADES="$ALL_LLDS_BASE_GRADES reg"
fi
if test $mercury_cv_gcc_labels = yes; then
    ALL_LLDS_BASE_GRADES="$ALL_LLDS_BASE_GRADES jump"
    if test $mercury_cv_gcc_model_fast = yes; then
        ALL_LLDS_BASE_GRADES="$ALL_LLDS_BASE_GRADES fast"
    fi
fi
if test $mercury_cv_asm_labels = yes; then
    if test $mercury_cv_gcc_labels = yes; then
        ALL_LLDS_BASE_GRADES="$ALL_LLDS_BASE_GRADES asm_jump"
    fi
    if test $mercury_cv_gcc_model_fast = yes; then
        ALL_LLDS_BASE_GRADES="$ALL_LLDS_BASE_GRADES asm_fast"
    fi
fi

# Start with the selected LLDS base grades:
# normally just the best LLDS base grade,
# unless --enable-inefficient-grades was selected,
# in which case we use all the LLDS grades.

if test "$enable_inefficient_grades" = yes; then
    BASE_LIBGRADES="$ALL_LLDS_BASE_GRADES"
else
    BASE_LIBGRADES="$BEST_LLDS_BASE_GRADE"
fi

# Find all the corresponding `.gc' (--conservative-gc) grades.
# Use both `.gc' and non-`.gc' versions, unless --disable-nogc-grades was set,
# in which case use only the `.gc' versions.

GC_LIBGRADES=""
for grade in $BASE_LIBGRADES; do
    GC_LIBGRADES="$GC_LIBGRADES $grade.gc"
done

if test "$enable_nogc_grades" = yes; then
    LIBGRADES="$BASE_LIBGRADES $GC_LIBGRADES"
else
    LIBGRADES="$GC_LIBGRADES"
fi

# Add `hlc' (--high-level-code, i.e. MLDS back-end) grades.
LIBGRADES="$LIBGRADES hlc.gc"

# Add `.agc' (--gc accurate) grades.
# Currently only hlc.agc is supported.
if test "$enable_agc_grades" = yes; then
    LIBGRADES="$LIBGRADES hlc.agc"
fi

# Add grades for profiling.
if test "$enable_prof_grades" = yes; then
    if test "$enable_old_prof_grades" = yes; then
        # Add `.prof' (--profiling) grades, if time profiling is supported.
        # We want to allow profiling of any installed grade which is likely
        # to be the most efficient grade for some application.
        # This means we want to support time profiling for
        #   - both GC and non-GC grades
        #     since non-GC grades might be best for some applications.
        #   - both LLDS and hlc grades
        # It is also convenient if we support a time profiling variant of
        # the default grade, if this is not the same as the most efficient
        # grade.
        if test "$mercury_cv_profiling" = yes; then
            # Start with profiling versions of the default grade.
            LIBGRADES="$LIBGRADES $DEFAULT_BASE_GRADE.gc.prof"
            if test "$enable_nogc_grades" = yes; then
                LIBGRADES="$LIBGRADES $DEFAULT_BASE_GRADE.prof"
            fi
            # Add profiling versions of the best LLDS grade,
            # if this is different than the default grade.
            if test "$BEST_LLDS_BASE_GRADE" != "$DEFAULT_BASE_GRADE"; then
                LIBGRADES="$LIBGRADES $BEST_LLDS_BASE_GRADE.gc.prof"
                if test "$enable_nogc_grades" = yes; then
                    LIBGRADES="$LIBGRADES $BEST_LLDS_BASE_GRADE.prof"
                fi
            fi
            # Add profiling versions of the hlc grade,
            # if this is different than the ones added above.
            if test "$enable_hlc_prof_grades" = "yes" &&
               test hlc != "$DEFAULT_BASE_GRADE" &&
               test hlc != "$BEST_LLDS_BASE_GRADE"
            then
                LIBGRADES="$LIBGRADES hlc.gc.prof"
                if test "$enable_nogc_grades" = yes; then
                    LIBGRADES="$LIBGRADES hlc.prof"
                fi
            fi
        fi

        # Add a `.memprof' (--memory-profiling) grade.
        # Hopefully memory allocation should be pretty much
        # independent of the grade, so we don't need to support
        # both GC and non-GC versions, or both LLDS and MLDS versions.
        LIBGRADES="$LIBGRADES $DEFAULT_BASE_GRADE.gc.memprof"
    fi

    # Add a `.profdeep' (--deep-profiling) grade, if deep profiling is enabled.
    # By default, we also use stack segments with deep profiling.
    if test $mercury_cv_enable_deep_profiler = yes; then
        LIBGRADES="$LIBGRADES $BEST_LLDS_BASE_GRADE.gc.profdeep.stseg"
    fi
fi

if test "$enable_trail_grades" = yes; then
    # Add `.trseg' (--use-trail --trail-segments) grades.
    # We don't install the fixed size trail `.tr' grades by
    # default as they are less useful.
    LIBGRADES="$LIBGRADES $DEFAULT_BASE_GRADE.gc.trseg"
    if test $DEFAULT_BASE_GRADE = hlc; then
        LIBGRADES="$LIBGRADES $BEST_LLDS_BASE_GRADE.gc.debug.trseg.stseg"
    else
        # $DEFAULT_BASE_GRADE must be an LLDS grade
        LIBGRADES="$LIBGRADES $DEFAULT_BASE_GRADE.gc.debug.trseg.stseg hlc.gc.trseg"
    fi
    if test "$enable_inefficient_grades" = yes; then
        if test "$enable_nogc_grades" = yes; then
            LIBGRADES="$LIBGRADES $DEFAULT_BASE_GRADE.trseg"
        fi
        if test $mercury_cv_profiling = yes &&
           test "$enable_prof_grades" = yes
        then
            LIBGRADES="$LIBGRADES $DEFAULT_BASE_GRADE.gc.prof.trseg"
        fi
    fi
fi

# Add `.mm' (--minimal-model) grades.
if test "$enable_mm_grades" = yes; then
    LIBGRADES="$LIBGRADES \
        $BEST_LLDS_BASE_GRADE.gc.mmsc \
        $BEST_LLDS_BASE_GRADE.gc.mmsc.debug"
    if test "$enable_dmm_grades" = yes; then
        LIBGRADES="$LIBGRADES \
            $BEST_LLDS_BASE_GRADE.gc.dmmsc \
            $BEST_LLDS_BASE_GRADE.gc.dmmsc.debug"
    fi
fi

# Add `.mmos' grades.
if test "$enable_mmos_grades" = yes; then
    LIBGRADES="$LIBGRADES \
        $BEST_LLDS_BASE_GRADE.gc.mmos \
        $BEST_LLDS_BASE_GRADE.gc.mmos.debug"
    if test "$enable_dmm_grades" = yes; then
        LIBGRADES="$LIBGRADES \
            $BEST_LLDS_BASE_GRADE.gc.dmmos \
            $BEST_LLDS_BASE_GRADE.gc.dmmos.debug"
    fi
fi

# Add `.debug' (--debug) and `.decldebug' (--decl-debug) grades.
# By default, we also use stack segments in debugging grades.
#
if test "$enable_debug_grades" = yes; then
    LIBGRADES="$LIBGRADES $BEST_DEBUG_BASE_GRADE.gc.debug.stseg"
    if test "$enable_decl_debug_grades" = yes; then
        LIBGRADES="$LIBGRADES $BEST_DEBUG_BASE_GRADE.gc.decldebug.stseg"
    fi
fi

# Add low-level debug grades.
if test "$enable_hlc_ll_debug_grades" = yes; then
    LIBGRADES="$LIBGRADES hlc.gcd.ll_debug"
fi

# Add rbmm grades.
if test "$enable_rbmm_grades" = yes; then
    # As of Oct 2007, all the users of RBMM grades are implementors
    # who may need the debugging and profiling versions.
    LIBGRADES="$LIBGRADES \
        $BEST_LLDS_BASE_GRADE.gc.rbmm \
        $BEST_LLDS_BASE_GRADE.gc.rbmmd \
        $BEST_LLDS_BASE_GRADE.gc.rbmmp \
        $BEST_LLDS_BASE_GRADE.gc.rbmmdp"
fi

# Add the .par (thread-safe) grade, if it is supported for this system
# Only enable it if this system has pthreads installed.
# For the low-level C backend we also enable stack segments by default.
#
if test "$enable_par_grades" = yes -a "$CFLAGS_FOR_THREADS" != "" \
    -a "$MR_HAVE_PTHREAD_H" = 1
then
    LIBGRADES="$LIBGRADES hlc.par.gc $BEST_LLDS_BASE_GRADE.par.gc.stseg"
fi

# Add the .stseg (stack segments) version of the library.
if test "$enable_stseg_grades" = "yes"
then
    LIBGRADES="$LIBGRADES $BEST_LLDS_BASE_GRADE.gc.stseg"
    if test "$enable_trail_grades" = "yes"
    then
        LIBGRADES="$LIBGRADES $BEST_LLDS_BASE_GRADE.gc.trseg.stseg"
    fi
fi

# Add C# back-end grade, if a C# compiler is installed.
if test "$CSC" != "" -a "$enable_csharp_grade" = yes
then
    LIBGRADES="$LIBGRADES csharp"
fi

# Add Java back-end grade, if Java is installed.
if test $mercury_cv_java = yes -a "$enable_java_grade" = yes
then
    LIBGRADES="$LIBGRADES java"
fi

# Add Erlang back-end grade, if Erlang is installed.
if test $mercury_cv_erlang = yes -a "$enable_erlang_grade" = yes
then
    LIBGRADES="$LIBGRADES erlang"
fi

# Add `.ssdebug' grades.
if test "$enable_ssdebug_grades" = yes; then
    # LLDS has better debug grades available.
    case "$LIBGRADES" in
        *hlc.gc*) LIBGRADES="$LIBGRADES hlc.gc.ssdebug" ;;
    esac
    case "$LIBGRADES" in
        *csharp*) LIBGRADES="$LIBGRADES csharp.ssdebug" ;;
    esac
    case "$LIBGRADES" in
        *java*) LIBGRADES="$LIBGRADES java.ssdebug" ;;
    esac
fi

# If this option is enabled then install only the grade that is being
# used to build the compiler.
# Also, override the grade that was chosen as the default grade above
# since it may not match the one that is going to be used to build
# the compiler.
#
if test "$enable_min_install" = yes
then
    LIBGRADES="$BEST_GRADE_FOR_COMPILER"
    DEFAULT_GRADE="$BEST_GRADE_FOR_COMPILER"
fi

# Allow the user to override the default list of library grades

if test "$enable_libgrades_given" = no -a -f .enable_lib_grades ; then
    MERCURY_MSG("using .enable_libgrades as the source for the set of grades to install")
    enable_libgrades=`cat .enable_lib_grades`
    enable_libgrades_given=yes
fi

if test "$enable_libgrades_given" = yes; then
    if test "$enable_libgrades" = no; then
        AC_MSG_ERROR(
[--enable-libgrades requires a comma-separated
    list of grades as argument])

    elif test "$enable_libgrades" = yes; then
        AC_MSG_ERROR(
[--enable-libgrades requires a comma-separated
    list of grades as argument])

    else
        # Generate only the grades specified by the user.
        raw_grades=`echo $enable_libgrades | sed 's/,/ /g'`
        cat /dev/null > .canonical_grades
        grade_error=false
        for raw_grade in $raw_grades
        do
            if ! test -x scripts/canonical_grade
            then
                echo $raw_grade >> .canonical_grades
            elif scripts/canonical_grade --grade $raw_grade > .confgrade
            then
                cat .confgrade >> .canonical_grades
            else
                MERCURY_MSG("--enable-libgrades: $raw_grade is not a grade")
                grade_error=true
            fi
        done

        canonical_grades=`sort -u .canonical_grades`
        rm -f .confgrade .canonical_grades > /dev/null 2>&1

        if $grade_error
        then
            AC_MSG_ERROR(
[invalid grade(s) specified for --enable-libgrades])
        fi

        LIBGRADES=$canonical_grades
    fi
else
    cat /dev/null > .canonical_grades
    conf_grade_error=false
    added_grade_error=false

    for raw_grade in $LIBGRADES
    do
        if ! test -x scripts/canonical_grade
        then
            echo $raw_grade >> .canonical_grades
        elif scripts/canonical_grade --grade $raw_grade > .confgrade
        then
            cat .confgrade >> .canonical_grades
        else
            conf_grade_error=true
        fi
    done

    if test "$enable_additional_libgrades_given" = yes; then
        if test "$enable_additional_libgrades" = no; then
            AC_MSG_ERROR(
[--enable-additional-libgrades requires a comma-separated
    list of grades as argument])

        elif test "$enable_additional_libgrades" = yes; then
            AC_MSG_ERROR(
[--enable-additional-libgrades requires a comma-separated
    list of grades as argument])

        else
            # Add the grades specified by the user to the list
            added_grades=`echo $enable_additional_libgrades | sed 's/,/ /g'`
            for raw_grade in $added_grades
            do
                if ! test -x scripts/canonical_grade
                then
                    echo $raw_grade >> .canonical_grades
                elif scripts/canonical_grade --grade $raw_grade > .confgrade
                then
                    cat .confgrade >> .canonical_grades
                else
                    MERCURY_MSG("--enable-additional-libgrades: $raw_grade is not a grade")
                    added_grade_error=true
                fi
            done
        fi
    fi

    if $conf_grade_error
    then
        AC_MSG_ERROR(
[internal error: configure script generates invalid grade(s)])
    fi

    if $added_grade_error
    then
        AC_MSG_ERROR(
[invalid grade(s) specified for --enable-additional-libgrades])
    fi

    canonical_grades=`sort -u .canonical_grades`
    rm -f .confgrade .canonical_grades > /dev/null 2>&1

    LIBGRADES=$canonical_grades
fi

# The echo deletes the initial space character.
LIBGRADES=`echo $LIBGRADES`

# Check that the default grade is in the set of grades to install.
have_default_grade="no"
for libgrade in $LIBGRADES
do
    if test "$DEFAULT_GRADE" = "$libgrade"
    then
        have_default_grade="yes"
    fi
done

LIBGRADE_OPTS=
for libgrade in $LIBGRADES
do
    LIBGRADE_OPTS="$LIBGRADE_OPTS --libgrade $libgrade"
done

AC_SUBST(LIBGRADES)
AC_SUBST(LIBGRADE_OPTS)

#-----------------------------------------------------------------------------#

# Determine how many of the r registers (r1, r2, ...) are real registers.
# Determine how many temporaries may be allocated real registers.
# Find out whether branch instructions have a delay slot.

case "$host" in
    i*86-*)
        # NUM_REAL_REGS=2
        # but succip and sp are real regs, so subtract 2
        NUM_REAL_R_REGS=0
        NUM_REAL_R_TEMPS=0
        HAVE_DELAY_SLOT=
        ;;
    x86_64*)
        # NUM_REAL_REGS=4
        # but succip and sp are real reg, so subtract 2
        NUM_REAL_R_REGS=2
        NUM_REAL_R_TEMPS=0
        HAVE_DELAY_SLOT=
        ;;
    alpha*-*)
        # NUM_REAL_REGS=7
        # but succip, sp, and hp are real regs, so subtract 3
        NUM_REAL_R_REGS=4
        NUM_REAL_R_TEMPS=6
        HAVE_DELAY_SLOT=
        ;;
    mips-*)
        # NUM_REAL_REGS=8
        # but succip, sp, and hp are real regs, so subtract 3
        NUM_REAL_R_REGS=5
        NUM_REAL_R_TEMPS=6
        HAVE_DELAY_SLOT=--have-delay-slot
        ;;
    rs6000-*|powerpc-*)
        # NUM_REAL_REGS=10
        # but succip, sp, hp, maxfr, and curfr are real regs, so subtract 5
        NUM_REAL_R_REGS=5
        NUM_REAL_R_TEMPS=6
        HAVE_DELAY_SLOT=
        ;;
    sparc-*)
        # NUM_REAL_REGS=10
        # but succip, sp, hp, maxfr, and curfr are real regs, so subtract 5
        NUM_REAL_R_REGS=5
        NUM_REAL_R_TEMPS=6
        HAVE_DELAY_SLOT=--have-delay-slot
        ;;
    hppa-*)
        # NUM_REAL_REGS=8
        # but succip, sp, and hp are real regs, so subtract 3
        NUM_REAL_R_REGS=5
        NUM_REAL_R_TEMPS=6
        HAVE_DELAY_SLOT=
        ;;
    arm*-*)
        # NUM_REAL_REGS=4
        # but succip and sp are real regs, so subtract 2
        NUM_REAL_R_REGS=2
        NUM_REAL_R_TEMPS=0
        HAVE_DELAY_SLOT=
        ;;
    *)
        NUM_REAL_R_REGS=0
        NUM_REAL_R_TEMPS=6
        HAVE_DELAY_SLOT=
        ;;
esac
AC_SUBST(NUM_REAL_R_REGS)
AC_SUBST(NUM_REAL_R_TEMPS)
AC_SUBST(HAVE_DELAY_SLOT)

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(whether the assembler handles .type)
AC_CACHE_VAL(mercury_cv_cannot_grok_asm_type_directive, [
    case "$host" in
        *-sunos4.*)
            mercury_cv_cannot_grok_asm_type_directive=yes
            can_grok_type_directive=no
            ;;
        *)
            mercury_cv_cannot_grok_asm_type_directive=no
            can_grok_type_directive=yes
            ;;
    esac
])
AC_MSG_RESULT($can_grok_type_directive)
if test $mercury_cv_cannot_grok_asm_type_directive = "yes"; then
    AC_DEFINE(MR_CANNOT_GROK_ASM_TYPE_DIRECTIVE)
fi

#-----------------------------------------------------------------------------#

AC_PATH_PROG(AS,as)
AC_MSG_CHECKING(whether the assembler does full preprocessing)

AC_CACHE_VAL(mercury_cv_asm_does_full_preprocessing, [
if test "$AS" != ""; then
    # check that it really works
    cat > conftest.s << EOF

# define foo(x) \
    /* foo */

    foo(x)
EOF
    if
        echo $AS conftest.s >&AC_FD_CC 2>&1 &&
        $AS conftest.s </dev/null >&AC_FD_CC 2>&1
    then
        mercury_cv_asm_does_full_preprocessing="yes"
    else
        mercury_cv_asm_does_full_preprocessing="no"
    fi
    rm -f conftest*
else
    mercury_cv_asm_does_full_preprocessing="no"
fi
])
AC_MSG_RESULT($mercury_cv_asm_does_full_preprocessing)
if test $mercury_cv_asm_does_full_preprocessing = "no"; then
    AS="$GCC_PROG -c -x assembler-with-cpp"
fi
AC_SUBST(AS)

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(whether structure assignment conflicts with global registers)
AC_CACHE_VAL(mercury_cv_cannot_use_structure_assignment,
AC_TRY_RUN(
    [
        changequote(<<,>>)

        /* MR_USE_GCC_GLOBAL_REGISTERS are essential to the purpose of the test. */
        /* We define MR_USE_GCC_NONLOCAL_GOTOS because some platforms */
        /* (e.g. SPARCs) cannot have MR_USE_GCC_GLOBAL_REGISTERS without it. */
        #define MR_USE_GCC_GLOBAL_REGISTERS
        #define MR_USE_GCC_NONLOCAL_GOTOS
        #include "mercury_imp.h"
        #include "mercury_trace_spy.h"

        MR_Spy_Point    spied_procs[2];

        int main() {
            spied_procs[1] = spied_procs[0];
        #if __GNUC__ >= 3
            /* gcc 3.1 seems to have problems with structure assignment
               and global registers, but which this simple test case
               does not trigger. So just force the test to fail for gcc 3.x. */
            exit(1);
        #else
            exit(0);
        #endif
        }

        changequote([,])
    ],
    [mercury_cv_cannot_use_structure_assignment=no],
    [mercury_cv_cannot_use_structure_assignment=yes])
)
AC_MSG_RESULT($mercury_cv_cannot_use_structure_assignment)
if test $mercury_cv_cannot_use_structure_assignment = "yes"; then
    AC_DEFINE(MR_CANNOT_USE_STRUCTURE_ASSIGNMENT)
fi

#-----------------------------------------------------------------------------#

# Allow the user to override the default value of MACOSX_DEPLOYMENT_TARGET.
# We use the version of the host system for the default value.
# XXX if we ever support Mac OS X as a cross-compiling target we should
# force the user to set a value using --with-macosx-deployment-target.

AC_ARG_WITH([macosx-deployment-target],
    AC_HELP_STRING([--with-macosx-deployment-target=<target>],
          [Specify the deployment target on Mac OS X.]),
    [mercury_cv_macosx_deployment_target="$withval"],
    [mercury_cv_macosx_deployment_target="auto"]
)

case "$mercury_cv_macosx_deployment_target" in
    yes)
        AC_MSG_ERROR([missing argument to --with-macosx-deployment-target=... option])
        exit 1
        ;;
    no)
        AC_MSG_ERROR([invalid option --without-macosx-deployment-target])
        exit 1
        ;;
esac

DEPLOYMENT_TARGET="no"

case "$host" in
    *apple*darwin*)
        case "$mercury_cv_macosx_deployment_target" in
            # NOTE: the sw_vers utility tells us which version of Mac OS X
            # we are using as opposed to which version of Darwin we are using.
            # uname only reports the latter.
            auto)
                DEPLOYMENT_TARGET=`sw_vers -productVersion | cut -d \. -f 1 -f 2`
                ;;

            *)
                DEPLOYMENT_TARGET="$mercury_cv_macosx_deployment_target"
                ;;
        esac

        if test "$DEPLOYMENT_TARGET" = "no"
        then
            AC_MSG_ERROR([cannot determine deployment target for this system])
        fi
        ;;
esac

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING([whether Mercury supports shared libraries on this system])
# We ought to use $target here rather than $host - but we don't
# support cross-compilation at the moment anyhow.

# Set up some default values that should work on most systems.
# See Mmake.common.in for documentation on the meaning of these variables.
LINK_EXE=$CC
LINK_SHARED_OBJ="$CC -shared"
LINK_SHARED_OBJ_SH="$CC -shared"
if test "$MERCURY_HAVE_GCC" = "yes"
then
    SHARED_LIBS="\`$CC -print-libgcc-file-name\` \$(MATH_LIB) -lc"
    SHARED_LIBS_SH="\`$CC -print-libgcc-file-name\` \$MATH_LIB -lc"
else
    SHARED_LIBS='$(MATH_LIB) -lc'
    SHARED_LIBS_SH='$MATH_LIB -lc'
fi
EXE_RPATH_OPT="-Wl,-rpath,"
EXE_RPATH_SEP=" -Wl,-rpath,"
SHLIB_RPATH_OPT="-Wl,-rpath,"
SHLIB_RPATH_SEP=" -Wl,-rpath,"

SHLIB_USE_INSTALL_NAME=""
SHLIB_INSTALL_NAME_FLAG="-install_name "

CFLAGS_FOR_PIC="-fpic -DMR_PIC"
EXT_FOR_PIC_OBJECTS=pic_o
EXT_FOR_SHARED_LIB=$LIB_SUFFIX
USE_DLLS=no
# The following variables specify options to $LINK_SHARED_OBJ to
# allow/disallow unresolved symbols when building shared libraries.
ALLOW_UNDEFINED=""
ERROR_UNDEFINED=""
DEFAULT_LINKAGE="shared"

case "$host" in
    i*86-*-linux|i*86-*-linux-gnu|x86_64-*-linux*)
        case $ac_cv_prog_gcc in
            yes)
                AC_MSG_RESULT(yes)
                EXT_FOR_SHARED_LIB=so
                AC_MSG_CHECKING(if linker supports --no-undefined)
                rm -f conftest*
                if test "$mercury_cv_thread_local_storage" = yes; then
                    # --no-undefined doesn't work with TLS
                    # and older versions of glibc.
                    echo "__thread int x;
                    int main(void) { return x; }" > conftest.c
                    $CC -Wl,--no-undefined -fPIC -shared \
                        -o conftest conftest.c 2>/dev/null
                else
                    echo "int main(void) { return 0; }" > conftest.c
                    $CC -Wl,--no-undefined -o conftest conftest.c
                fi
                if test -f conftest; then
                    AC_MSG_RESULT(yes)
                    ERROR_UNDEFINED="-Wl,--no-undefined"

                    AC_MSG_CHECKING(for ld-linux.so)
                    rm -f conftest*
                    echo "int main(void) { return 0; }" > conftest.c
                    $CC -o conftest conftest.c
                    ld_linux=`ldd ./conftest | \
                        awk '/ld-linux/{print $1;}'`
                    case $ld_linux in
                        /lib/ld-linux.so*)
                            AC_MSG_RESULT($ld_linux)
                            SHARED_LIBS="$SHARED_LIBS $ld_linux"
                            SHARED_LIBS_SH="$SHARED_LIBS_SH $ld_linux"
                            ;;
                        *)
                            AC_MSG_RESULT(no)
                            ;;
                    esac
                else
                    AC_MSG_RESULT(no)
                fi
                rm -f conftest*
                ;;
            *)
                AC_MSG_RESULT(no)
                ;;
        esac

        # On x86_64 machines we use -fPIC rather than -fpic
        # in order to avoid limits on the size of the global
        # offset table implied by the latter.
        case "$host" in
            x86_64-*-linux*)
                CFLAGS_FOR_PIC="-fPIC -DMR_PIC"
                ;;
        esac
        ;;
    m68*-linux|m68*-linux-gnu)
        AC_MSG_RESULT(yes)
        EXT_FOR_SHARED_LIB=so
        ;;
    *-freebsd*)
        # Tested on FreeBSD 10.1 i386/amd64.
        AC_MSG_RESULT(yes)
        EXT_FOR_SHARED_LIB=so
        ;;
    *-openbsd*)
        AC_MSG_RESULT(yes)
        EXT_FOR_SHARED_LIB=so
        ;;
    i*86-*-solaris2.*)
        AC_MSG_RESULT(yes)

        # XXX For Solaris/SPARC, we don't link in libgcc.a.
        # Why not?  Do we need to do the same thing for Solaris/x86?
        #SHARED_LIBS="$(MATH_LIB) -lc"  # don't link in libgcc.a
        #SHARED_LIBS_SH="$MATH_LIB -lc" # don't link in libgcc.a

        # XXX We need -mimpure-text because libraries such as
        # libreadline.a might be installed with only the .a rather
        # than the .so, and not compiled with -fpic,
        # and we need to link against them when creating shared libs.
        LINK_SHARED_OBJ="$CC -shared -mimpure-text"
        LINK_SHARED_OBJ_SH="$CC -shared -mimpure-text"

        # XXX We should use the following.
        # However, this causes problems on mundroo.cs.mu.oz.au,
        # due to the readline library being installed only as
        # libreadline.a, with no corresponding libreadline.so.
        #ERROR_UNDEFINED="-Wl,-z,defs"

        EXE_RPATH_OPT="-R"
        EXE_RPATH_SEP=" -R"
        SHLIB_RPATH_OPT="-R"
        SHLIB_RPATH_SEP=" -R"
        EXT_FOR_SHARED_LIB=so
        DEFAULT_LINKAGE=static
        ;;
    sparc-sun-solaris2.*)
        AC_MSG_RESULT(yes)
        SHARED_LIBS="$(MATH_LIB) -lc"   # don't link in libgcc.a
        SHARED_LIBS_SH="$MATH_LIB -lc"  # don't link in libgcc.a
        LINK_SHARED_OBJ="$CC -shared -mimpure-text"
        LINK_SHARED_OBJ_SH="$CC -shared -mimpure-text"
        ERROR_UNDEFINED="-Wl,-z,defs"
        EXE_RPATH_OPT="-R"
        EXE_RPATH_SEP=" -R"
        SHLIB_RPATH_OPT="-R"
        SHLIB_RPATH_SEP=" -R"
        CFLAGS_FOR_PIC="-fpic -DMR_PIC" # used only for libgc.{a,so}
        EXT_FOR_PIC_OBJECTS=o

        # Note that despite the above definition of CFLAGS_FOR_PIC,
        # we don't use `-fpic' for shared libraries on Solaris
        # (the definition of EXT_FOR_PIC_OBJECTS=o means that
        # CFLAGS_FOR_PIC will be used only for building libgc.a
        # and libgc.so.)
        #
        # The reason for this is that shared libraries work without it
        # (well, at least they are shared on disk -- although the code is
        # not shared at runtime), and using `-fpic' would reduce efficiency.
        #
        # It works because the Solaris dynamic linker will actually do
        # the fixups at runtime for non-PIC code. (The code is mapped
        # copy-on-write, and when the linker does the fixups, it gets copied.
        # Hence the lack of runtime sharing.) We need to link with
        # `gcc -shared -mimpure-text' rather than `gcc -shared', because
        # `gcc -shared' passes `-z text' to the linker, which causes it to
        # report an error if any runtime fixups would be needed.
        #
        # If you *do* use `-fpic', you must also use `-DMR_PIC'.
        #
        # See runtime/mercury_goto.h for the code that handles PIC on SPARCs.
        # Note that mixing PIC and non-PIC code is fine on SPARCs.

        EXT_FOR_SHARED_LIB=so
        ;;
    *-cygwin*)
        # Disabled for now, since it hasn't been tested.
        # xxx_MSG_RESULT(yes)
        # EXT_FOR_SHARED_LIB=dll
        # USE_DLLS=yes
        AC_MSG_RESULT(disabled for now because it is untested)

        CFLAGS_FOR_PIC=
        EXT_FOR_PIC_OBJECTS=o
        DEFAULT_LINKAGE=static
        ;;
    *mingw*)
        # disabled for now, since it hasn't been tested
        # xxx_MSG_RESULT(yes)
        # EXT_FOR_SHARED_LIB=dll
        # USE_DLLS=yes
        AC_MSG_RESULT(disabled for now because it is untested)

        CFLAGS_FOR_PIC=
        EXT_FOR_PIC_OBJECTS=o
        DEFAULT_LINKAGE=static
        ;;
    *apple*darwin*)
        # If the compiler is gcc or clang then use Darwin style dynamic
        # linking. Otherwise use static linking.
        case "$C_COMPILER_TYPE" in
            gcc*|clang*)
                AC_MSG_RESULT(yes)
                SHLIB_USE_INSTALL_NAME="--shlib-linker-use-install-name"
                SHLIB_INSTALL_NAME_FLAG="-install_name "
                EXT_FOR_SHARED_LIB=dylib
                CFLAGS_FOR_PIC="-fPIC -DMR_PIC"
                ERROR_UNDEFINED="-undefined error"
                # The MACOSX_DEPLOYMENT_TARGET environment variable needs to be
                # set so we can use the `-undefined dynamic_lookup' option.
                SET_MACOSX_DEPLOYMENT_TARGET="\
                    MACOSX_DEPLOYMENT_TARGET=$DEPLOYMENT_TARGET; \
                    export MACOSX_DEPLOYMENT_TARGET"
                AC_SUBST(SET_MACOSX_DEPLOYMENT_TARGET)
                LINK_SHARED_OBJ="$CC -multiply_defined suppress \
                     -dynamiclib -single_module"
                LINK_SHARED_OBJ_SH="$CC -multiply_defined \
                    suppress -dynamiclib -single_module"
                ALLOW_UNDEFINED="-undefined dynamic_lookup"
            ;;

            *)
                CFLAGS_FOR_PIC=
                EXT_FOR_PIC_OBJECTS=o
                DEFAULT_LINKAGE=static
                AC_MSG_RESULT(no)
            ;;
        esac
        ;;
    arm*-linux|arm*-linux-gnu)
        AC_MSG_RESULT(yes)
        EXT_FOR_SHARED_LIB=so
        # One can build gcc with a default PIC register other than sl
        # so let's force it. If you change this,
        # update MR_ARM_PIC_REG in runtime/mercury_goto.h as well.
        CFLAGS_FOR_PIC="-mpic-register=sl $CFLAGS_FOR_PIC"
        ;;
    *)
        # CFLAGS_FOR_PIC is used by boehm_gc/Makefile when creating libgc.a.
        # If the system doesn't support shared libraries,
        # then we should set it to empty, not `-fpic'.
        CFLAGS_FOR_PIC=
        EXT_FOR_PIC_OBJECTS=o
        DEFAULT_LINKAGE=static
        AC_MSG_RESULT(no)
        ;;
esac

#-----------------------------------------------------------------------------#
#
# Note that changes here may require changes in scripts/mgnuc.in and
# compiler/compile_target_code.m.

# Older versions of clang / LLVM cannot compile the C code generated by the
# Mercury compiler at higher optimization levels.
#
OPT_FLAGS_FOR_CLANG="-O0"

case "$mercury_cv_cc_type" in

    gcc*)
        # We no longer pass -ansi to GCC on any system for the following
        # reasons:
        #
        # * It restricts the dialect of C used to C90 and that prevents us
        #   from using various C99 library features. Every version of GCC we
        #   support provides those features so there is no point in doing this.
        # * It causes complications with the header files on some systems.
        #   Notably with the definitions of the types off_t and off64_t
        #   on MinGW systems.
        # * This setting also affects the compilation of user written C code in
        #   foreign_proc and foreign code pragmas.
        #
        # NOTE: the --no-ansi option supported by the mgnuc script does
        # more than simply pass the value of CFLAGS_FOR_ANSI to the C compiler.
        # (See that script for details.)
        #
        CFLAGS_FOR_ANSI=

        # For a full list of the other gcc warnings that we don't enable,
        # and why, see scripts/mgnuc.in.
        CFLAGS_FOR_WARNINGS="-Wall -Wwrite-strings -Wshadow -Wmissing-prototypes -Wno-unused -Wno-uninitialized -Wstrict-prototypes"

        # Enabling -fomit-frame-pointer causes setjmp/longjmp to misbehave
        # with MinGW on Windows XP.
        case "$host" in
            *mingw*) CFLAGS_FOR_OPT="-O2" ;;
            *)       CFLAGS_FOR_OPT="-O2 -fomit-frame-pointer" ;;
        esac

        # Disable the GCC optimization -ftree-vrp with GCC 4.2. on Darwin
        # since enabling it causes code in the hlc.gc grade to be miscompiled.
        case "$host" in
            *apple*darwin*)
                case "$C_COMPILER_TYPE" in
                    gcc_4_2_*)
                        CFLAGS_FOR_OPT="$CFLAGS_FOR_OPT -fno-tree-vrp"
                    ;;
                esac
            ;;
        esac

        CFLAGS_FOR_DEBUG="-g"
        CFLAGS_FOR_NO_STRICT_ALIASING="-fno-strict-aliasing"
        MCFLAGS_FOR_CC=
        ;;

    clang*)

        # We do not compile with -ansi when using clang because that (currently) puts
        # clang into C89 mode and we want to use C99's inline keyword with clang.
        CFLAGS_FOR_ANSI=

        # XXX we need go through the warning and optimization options for clang
        # more carefully.
        CFLAGS_FOR_WARNINGS="-w"

        # Disable C compiler optimizations for old versions of clang.
        # (See above for details.)
        case "$C_COMPILER_TYPE" in
            clang_0_*|clang_1_*|clang_2_*)
                CFLAGS_FOR_OPT="-O0 -fomit-frame-pointer"
            ;;
            *)
                CFLAGS_FOR_OPT="-O2 -fomit-frame-pointer"
                OPT_FLAGS_FOR_CLANG="-O2"
            ;;
        esac
        CFLAGS_FOR_DEBUG="-g"
        CFLAGS_FOR_NO_STRICT_ALIASING="-fno-strict-aliasing"
        MCFLAGS_FOR_CC=
        ;;

    msvc*)
        CFLAGS_FOR_ANSI=
        # Suppress the MSVC banner message.
        CFLAGS_FOR_WARNINGS="-nologo"
        CFLAGS_FOR_OPT=
        # XXX See README.MS-Visual C for details of why we disable this.
        #CFLAGS_FOR_DEBUG="-Zi"
        CFLAGS_FOR_DEBUG=
        CFLAGS_FOR_NO_STRICT_ALIASING=

        # Using the MSVC compiler implies that we must use
        # a maximum jump table size of 512 to avoid a fixed limit
        # in the compiler.
        MCFLAGS_FOR_CC="--max-jump-table-size 512"
        ;;

    *)
        CFLAGS_FOR_ANSI=
        CFLAGS_FOR_OPT="-O"
        CFLAGS_FOR_WARNINGS=
        CFLAGS_FOR_DEBUG="-g"
        CFLAGS_FOR_NO_STRICT_ALIASING=
        MCFLAGS_FOR_CC=
        ;;
esac

CFLAGS_FOR_OPT="$CFLAGS_FOR_OPT $CFLAGS_FOR_NO_STRICT_ALIASING"

AC_SUBST([CFLAGS_FOR_ANSI])
AC_SUBST([CFLAGS_FOR_WARNINGS])
AC_SUBST([CFLAGS_FOR_OPT])
AC_SUBST([CFLAGS_FOR_DEBUG])
AC_SUBST([CFLAGS_FOR_NO_STRICT_ALIASING])
AC_SUBST([MCFLAGS_FOR_CC])
AC_SUBST([OPT_FLAGS_FOR_CLANG])
AC_SUBST([C_COMPILER_TYPE])

#-----------------------------------------------------------------------------#
#
# Work out how to link statically
#

# Note that changes here may require changes in scripts/ml.in.

LD_STATIC_FLAGS=
case "$C_COMPILER_TYPE" in
    gcc*|clang*)
        LD_STATIC_FLAGS=-static
        ;;
esac
case "$FULLARCH" in
    *-sun-solaris*)
        case "$COMPILER" in
            cc)
                LD_STATIC_FLAGS="-B static"
                ;;
        esac
        ;;
esac

# We can't put the call to MSG_CHECKING at the start of the case statement
# above, because of the nested check in the lcc case. So we just put it here
# immediately before the MSG_RESULT call.
AC_MSG_CHECKING([options for static linking])
AC_MSG_RESULT($LD_STATIC_FLAGS)
case "$LD_STATIC_FLAGS" in
    "")
        AC_MSG_WARN(
[\`mmc --linkage static' and \`ml --static' not implemented with
****    this C compiler (\`$CC').])
esac

#-----------------------------------------------------------------------------#

case "$host" in
    *-cygwin*)
        EXT_FOR_EXE=".exe"
        HOST_ENV_TYPE=cygwin
        TARGET_ENV_TYPE=cygwin
        ;;
    *mingw*)
        EXT_FOR_EXE=".exe"
        HOST_ENV_TYPE=msys
        TARGET_ENV_TYPE=msys
        ;;
    *)
        EXT_FOR_EXE=
        HOST_ENV_TYPE=posix
        TARGET_ENV_TYPE=posix
        ;;
esac

#-----------------------------------------------------------------------------#

AC_SUBST(LINK_EXE)
AC_SUBST(LD_STATIC_FLAGS)
AC_SUBST(DEFAULT_LINKAGE)
AC_SUBST(LINK_SHARED_OBJ)
AC_SUBST(LINK_SHARED_OBJ_SH)
AC_SUBST(ERROR_UNDEFINED)
AC_SUBST(ALLOW_UNDEFINED)
AC_SUBST(EXE_RPATH_OPT)
AC_SUBST(EXE_RPATH_SEP)
AC_SUBST(SHLIB_RPATH_OPT)
AC_SUBST(SHLIB_RPATH_SEP)
AC_SUBST(SHLIB_USE_INSTALL_NAME)
AC_SUBST(SHLIB_INSTALL_NAME_FLAG)
AC_SUBST(CFLAGS_FOR_PIC)
AC_SUBST(EXT_FOR_PIC_OBJECTS)
AC_SUBST(EXT_FOR_SHARED_LIB)
AC_SUBST(EXT_FOR_EXE)
AC_SUBST(HOST_ENV_TYPE)
AC_SUBST(TARGET_ENV_TYPE)
AC_SUBST(USE_DLLS)
AC_SUBST(SHARED_LIBS)
AC_SUBST(SHARED_LIBS_SH)
AC_SUBST(DEFAULT_LINKAGE)
if test "$USE_DLLS" = "yes"; then
    AC_DEFINE(MR_USE_DLLS)
fi

#-----------------------------------------------------------------------------#
#
# Sanitizers
#

AC_ARG_ENABLE([sanitizers],
    AC_HELP_STRING([--enable-sanitizers],
        [enable AddressSanitizer and UndefinedBehaviorSanitizer]),
    [ac_sanitizers="$enableval"], [ac_sanitizers=no])

case "$C_COMPILER_TYPE" in
    gcc*)
        if test "$ac_sanitizers" = yes
        then
            CFLAGS_FOR_SANITIZERS="-fsanitize=address,undefined"
            LDFLAGS_FOR_SANITIZERS="-fsanitize=address,undefined -pthread"
        fi
        ;;
    *)
        CFLAGS_FOR_SANITIZERS=
        LDFLAGS_FOR_SANITIZERS=
        ;;
esac

AC_SUBST(CFLAGS_FOR_SANITIZERS)
AC_SUBST(LDFLAGS_FOR_SANITIZERS)

#-----------------------------------------------------------------------------#

if test "$BOOTSTRAP_MC" = ""; then
    BOOTSTRAP_MC=mmc
fi
BOOTSTRAP_MC_COMPILER="$BOOTSTRAP_MC"
BOOTSTRAP_MC_ARGS="$HAVE_BOXED_FLOATS --conf-low-tag-bits $LOW_TAG_BITS --bits-per-word $BITS_PER_WORD --bytes-per-word $BYTES_PER_WORD"
BOOTSTRAP_MC="$BOOTSTRAP_MC_COMPILER $BOOTSTRAP_MC_ARGS"
AC_SUBST(BOOTSTRAP_MC_COMPILER)
AC_SUBST(BOOTSTRAP_MC_ARGS)
AC_SUBST(BOOTSTRAP_MC)

#-----------------------------------------------------------------------------#

# The following allows us to share some subroutines between the
# `ml' and `mgnuc' scripts.

top="`pwd`"
INIT_GRADE_OPTIONS="$top"/scripts/init_grade_options.sh-subr
PARSE_GRADE_OPTIONS="$top"/scripts/parse_grade_options.sh-subr
FINAL_GRADE_OPTIONS="$top"/scripts/final_grade_options.sh-subr
PARSE_ML_OPTIONS="$top"/scripts/parse_ml_options.sh-subr
CANONICAL_GRADE="$top"/scripts/canonical_grade.sh-subr

# The following allows us to duplicate a subroutine in mgnuc.

MGNUC_FILE_OPTS=$top/scripts/mgnuc_file_opts.sh-subr

# demangling hangs on cygwin, so don't enable it.
case "$host" in
    *-cygwin*)
        DEMANGLE=false
        ;;
    *)
        DEMANGLE=true
        ;;
esac

AC_SUBST_FILE(INIT_GRADE_OPTIONS)
AC_SUBST_FILE(PARSE_GRADE_OPTIONS)
AC_SUBST_FILE(FINAL_GRADE_OPTIONS)
AC_SUBST_FILE(PARSE_ML_OPTIONS)
AC_SUBST_FILE(CANONICAL_GRADE)
AC_SUBST_FILE(MGNUC_FILE_OPTS)
AC_SUBST(DEMANGLE)

# mercury_config includes configure.help to include the help for configure
# in its `--help' output and man page.
${CONFIG_SHELL-/bin/sh} "$0" --help | sed -e 's/`/\\`/g' > configure.help
CONFIGURE_HELP=$top/configure.help
AC_SUBST_FILE(CONFIGURE_HELP)

#-----------------------------------------------------------------------------#
#
# Check for the POSIX struct tms and times() function.
# This is used for the time__times procedure in library/times.m.
#

AC_MSG_CHECKING(for struct tms and times function)
AC_CACHE_VAL(mercury_cv_have_posix_times,
    AC_TRY_LINK(
        [
            #include <sys/types.h>
            #include <sys/times.h>
        ],
        [
            struct tms t;
            long Ut, St, CUt, CSt;
            long Ret;

            Ret = (long) times(&t);

            Ut = (long) t.tms_utime;
            St = (long) t.tms_stime;
            CUt = (long) t.tms_cutime;
            CSt = (long) t.tms_cstime;
        ],
        [mercury_cv_have_posix_times=yes],
        [mercury_cv_have_posix_times=no]))

# Figure out whether the test succeeded.
if test "$mercury_cv_have_posix_times" = yes; then
    AC_MSG_RESULT(yes)
    AC_DEFINE(MR_HAVE_POSIX_TIMES)
else
    AC_MSG_RESULT(no)
fi

#-----------------------------------------------------------------------------#
#
# Check for the environ global variable.
#

AC_MSG_CHECKING(for environ global variable)
AC_CACHE_VAL(mercury_cv_have_environ,
AC_TRY_LINK(
    [
        #include <unistd.h>

        extern char **environ;
    ],
    [
        environ
    ],
    [mercury_cv_have_environ=yes],
    [mercury_cv_have_environ=no]))

# Figure out whether the test succeeded.
if test "$mercury_cv_have_environ" = yes; then
    AC_MSG_RESULT(yes)
    AC_DEFINE(MR_HAVE_ENVIRON)
else
    AC_MSG_RESULT(no)
fi

#-----------------------------------------------------------------------------#
#
# Check for the WIN32 Sleep function
#

AC_MSG_CHECKING(for Sleep function)
AC_CACHE_VAL(mercury_cv_have_capital_s_sleep,
AC_TRY_LINK(
    [
        #include <windows.h>
    ],
    [
        Sleep(1);
    ],
    [mercury_cv_have_capital_s_sleep=yes],
    [mercury_cv_have_capital_s_sleep=no]))

if test "$mercury_cv_have_capital_s_sleep" = yes; then
    AC_MSG_RESULT(yes)
    AC_DEFINE(MR_HAVE_CAPITAL_S_SLEEP)
else
    AC_MSG_RESULT(no)
fi

#-----------------------------------------------------------------------------#
#
# Check for the various IEEE predicates
#

MERCURY_CHECK_FOR_IEEE_FUNC(isnan)
MERCURY_CHECK_FOR_IEEE_FUNC(isnanf)
MERCURY_CHECK_FOR_IEEE_FUNC(isinf)
MERCURY_CHECK_FOR_IEEE_FUNC(isinff)
MERCURY_CHECK_FOR_IEEE_FUNC(finite)
MERCURY_CHECK_FOR_IEEE_FUNC(isfinite)

#-----------------------------------------------------------------------------#
#
# Check whether sockets work (we need them for the external debugger)
#

#
# check whether we need -lsocket
#
AC_CHECK_LIB(socket, socket, SOCKET_LIBRARY=-lsocket, SOCKET_LIBRARY="")

#
# check whether we need -lnsl
#
AC_CHECK_LIB(nsl, inet_addr, NSL_LIBRARY=-lnsl, NSL_LIBRARY="")

#
# temporarily add -lsocket to LIBS, for use by TRY_LINK
#
save_LIBS="$LIBS"
LIBS="$LIBS $SOCKET_LIBRARY $NSL_LIBRARY"

#
# use TRY_LINK to see whether we can use sockets
#
AC_MSG_CHECKING(whether we can use sockets (for Morphine))
AC_CACHE_VAL(mercury_cv_sockets_work,
AC_TRY_LINK(
    [
        changequote(<<,>>)
        /*
        ** The following code was copied from
        ** runtime/mercury_trace_external.c
        */

        #include <stdio.h>
        #include <string.h>
        #include <errno.h>
        #include <stdarg.h>
        #include <stdlib.h>
        #include <sys/types.h>
        #include <unistd.h>
        #include <sys/socket.h>
        #include <sys/un.h>
        #include <arpa/inet.h>
        #include <netinet/in.h>
        #include <netdb.h>

        static int MR_debug_socket = 1;

        void
        fatal_error(const char *s)
        {
            fprintf(stderr, "", s);
            exit(1);
        }
    ],
    [
        /* this is the body of the test function */
        {
            int fd;
            int len;
            FILE *file_in;
            FILE *file_out;
            int addr_family;
            char *unix_socket;
            char *inet_socket;
            struct sockaddr_un unix_address;
            struct sockaddr_in inet_address;
            struct sockaddr* addr;

            /*
            ** We presume that the user's program has been invoked from
            ** within the debugger (e.g. Opium).
            ** The debugger (or the user) should set the
            ** MERCURY_DEBUGGER_UNIX_SOCKET or MERCURY_DEBUGGER_INET_SOCKET
            ** environment variable to tell the user's program which socket
            ** it needs to connect to.
            */
            unix_socket = getenv("MERCURY_DEBUGGER_UNIX_SOCKET");
            inet_socket = getenv("MERCURY_DEBUGGER_INET_SOCKET");
            if (unix_socket == NULL && inet_socket == NULL) {
                fatal_error("you must set either the "
                    "MERCURY_DEBUGGER_UNIX_SOCKET\n"
                    "or MERCURY_DEBUGGER_INET_SOCKET "
                    "environment variable");
            }
            if (unix_socket != NULL && inet_socket != NULL) {
                fatal_error("you must set only one of the "
                    "MERCURY_DEBUGGER_UNIX_SOCKET "
                    "and MERCURY_DEBUGGER_INET_SOCKET\n"
                    "environment variables");
            }
            if (unix_socket) {
                addr_family = AF_UNIX;
                (memset)(&unix_address, 0, sizeof(unix_address));
                unix_address.sun_family = AF_UNIX;
                strcpy(unix_address.sun_path, unix_socket);
                addr = (struct sockaddr *) &unix_address;
                len = strlen(unix_address.sun_path) +
                    sizeof(unix_address.sun_family);
            } else {
                char hostname[255];
                char port_string[255];
                unsigned short port;
                int host_addr;

                /*
                ** Parse the MERCURY_DEBUGGER_INET_SOCKET environment variable.
                ** It should be in the format "<hostname> <port>",
                ** where <hostname> is numeric (e.g. "123.456.78.90").
                */

                if (sscanf(inet_socket, "%254s %254s", hostname, port_string)
                    != 2)
                {
                    fatal_error("MERCURY_DEBUGGER_INET_SOCKET invalid");
                }
                host_addr = inet_addr(hostname);
                if (host_addr == -1) {
                    fatal_error("MERCURY_DEBUGGER_INET_SOCKET: "
                        "invalid address");
                }
                if (sscanf(port_string, "%hu", &port) != 1) {
                    fatal_error("MERCURY_DEBUGGER_INET_SOCKET: "
                        "invalid port");
                }

                fprintf(stderr, "Mercury runtime: host = %s, port = %d\n",
                        hostname, port);

                inet_address.sin_family = AF_INET;
                inet_address.sin_addr.s_addr = host_addr;
                inet_address.sin_port = htons(port);
                addr_family = AF_INET;
                addr = (struct sockaddr *) &inet_address;
                len = sizeof(inet_address);
            }

            /*
            ** Open the socket.
            */

            fd = socket(addr_family, SOCK_STREAM, 0);

            /*
            ** Connect to the socket
            */
            connect(fd, addr, len);

            /*
            ** Convert the socket fd to a Mercury stream
            */
            file_in = fdopen(fd, "r");
            file_out = fdopen(fd, "w");
        }
        changequote([,])
    ],
    [mercury_cv_sockets_work=yes],
    [mercury_cv_sockets_work=no]))

#
# figure out whether the test succeeded
#
if test "$mercury_cv_sockets_work" = yes; then
    AC_MSG_RESULT(yes)
else
    AC_MSG_RESULT(no)
fi

#
# restore the previous value of LIBS
#
LIBS="$save_LIBS"

#-----------------------------------------------------------------------------#
#
# Add an option that enables the external debugger support.
# By default, external debugger support is enabled if and only if sockets work.
# But it is also possible to explicitly disable this by passing the
# `--disable-extern-debug' option. This is needed on some platforms because
# the test for sockets working sometimes gets some false positives
# (e.g. if they work with dynamic linking but not static linking,
# then the test will pass, but it will cause problems later on
# when we try to link things statically).
#

AC_ARG_ENABLE(extern-debug,
    AC_HELP_STRING([--disable-extern-debug],
                   [disable the external (separate process) debugger]),
    mercury_cv_enable_extern_debug="$enableval",
    mercury_cv_enable_extern_debug="$mercury_cv_sockets_work")

AC_MSG_CHECKING(whether to enable the external debugger)
if test "$mercury_cv_enable_extern_debug" = "yes"; then
    if test "$mercury_cv_sockets_work" = yes; then
        AC_MSG_RESULT(yes)
        AC_DEFINE(MR_USE_EXTERNAL_DEBUGGER)
    else
        AC_MSG_RESULT(no)
        AC_MSG_ERROR(Cannot enable external debugger because sockets do not work)
    fi
else
    AC_MSG_RESULT(no)

    # Don't link with the socket library if we're not going to use it.
    SOCKET_LIBRARY=
    NSL_LIBRARY=
fi

AC_SUBST(SOCKET_LIBRARY)
AC_SUBST(NSL_LIBRARY)

#-----------------------------------------------------------------------------#
#
# Check for dynamic linking, unless the --disable-dynamic-link option
# was specified.
#

AC_ARG_ENABLE(dynamic-link,
    AC_HELP_STRING([--disable-dynamic-link],
                   [disable the use of dynamic linking]),
mercury_cv_enable_dynamic_link="$enableval",mercury_cv_enable_dynamic_link=yes)

if test "$mercury_cv_enable_dynamic_link" = "yes"; then
    MERCURY_CHECK_FOR_HEADERS(dlfcn.h)
    AC_CHECK_LIB(dl, dlopen, DL_LIBRARY="-ldl", DL_LIBRARY="")
    AC_SUBST(DL_LIBRARY)

    # Temporarily add $DL_LIBRARY to LIBS while we check for dlopen etc.
    save_LIBS="$LIBS"
    LIBS="$LIBS $DL_LIBRARY"

    mercury_check_for_functions dlopen dlclose dlsym dlerror

    # restore old value of LIBS
    LIBS="$save_LIBS"
fi

TRACE_BASE_LIBS_SYSTEM="$SOCKET_LIBRARY $NSL_LIBRARY $DL_LIBRARY"
AC_SUBST(TRACE_BASE_LIBS_SYSTEM)

#-----------------------------------------------------------------------------#
#
# Check whether to use the MercuryFile struct
#

AC_MSG_CHECKING(whether to enable the new MercuryFile struct)
AC_ARG_ENABLE(new-mercuryfile-struct,
    AC_HELP_STRING([--enable-new-mercuryfile-struct],
                   [enable new MercuryFile struct.]),
    ac_new_mercuryfile_struct=yes,ac_new_mercuryfile_struct=no)

if test "$ac_new_mercuryfile_struct" = "yes"; then
    AC_MSG_RESULT(yes)
    AC_DEFINE(MR_NEW_MERCURYFILE_STRUCT)
else
    AC_MSG_RESULT(no)
fi

#-----------------------------------------------------------------------------#
#
# Check whether to disable the use of tagged trail entries
#

AC_ARG_ENABLE([tagged-trail],
    AC_HELP_STRING([--disable-tagged-trail],
        [do not use tagged trail entries, even if they are supported.]),
    [ac_tagged_trail="$enableval"], [ac_tagged_trail=yes])

if test "$ac_tagged_trail" = "no"
then
    AC_DEFINE(MR_FORCE_NO_TAGGED_TRAIL)
fi

#-----------------------------------------------------------------------------#

AC_ARG_WITH(msvcrt,
    AC_HELP_STRING([--with-msvcrt],
                   [use the MS Visual C runtime if using MS C compiler.]),
    mercury_cv_msvcrt=yes,mercury_cv_msvcrt=no)

AC_MSG_CHECKING(whether to use the MS Visual C Runtime)
if test "$mercury_cv_msvcrt" = "yes"; then
    if test "$ac_microsoft" = "yes"; then
        AC_MSG_RESULT(yes)
        USE_MSVCRT=yes
    else
        AC_MSG_RESULT(no as not using the MS C compiler)
        USE_MSVCRT=no
    fi
else
    AC_MSG_RESULT(no)
    USE_MSVCRT=no
fi
AC_SUBST(USE_MSVCRT)

#-----------------------------------------------------------------------------#
#
# Check for readline
#

MERCURY_CHECK_READLINE

#-----------------------------------------------------------------------------#
#
# Check for libhwloc, http://www.open-mpi.org/projects/hwloc/
#
PKG_PROG_PKG_CONFIG

AC_ARG_WITH([hwloc],
    [AS_HELP_STRING([--without-hwloc],
        [Do not use libhwloc to detect the processors available.])],
    [],
    [with_hwloc=check])

if test "$with_hwloc" != "no"; then
    PKG_CHECK_MODULES(libhwloc, hwloc >= 1.1,
        [
            AC_DEFINE(MR_HAVE_HWLOC)
            hwloc_static_libs="`pkg-config --libs --static hwloc`"
        ],
        [
            case "$LIBGRADES" in
                $BEST_LLDS_BASE_GRADE.par.gc*)
                    MERCURY_MSG(["Warning: libhwloc not found, thread pinning in"])
                    MERCURY_MSG(["low-level C parallel grades may be less accurate."])
                    ;;
                *)
                    ;;
            esac
            hwloc_static_libs=""
        ])
else
    libhwloc_LIBS=""
    libhwloc_CFLAGS=""
    hwloc_static_libs=""
fi

HWLOC_LIBS="$libhwloc_LIBS"
HWLOC_CFLAGS="$libhwloc_CFLAGS"
HWLOC_STATIC_LIBS="$hwloc_static_libs"
AC_SUBST(HWLOC_LIBS)
AC_SUBST(HWLOC_CFLAGS)
AC_SUBST(HWLOC_STATIC_LIBS)

#-----------------------------------------------------------------------------#
#
# Check for flex and bison
#

# We only require flex and bison if we are going to compile the trace
# directory. We don't need them if we are reconfiguring an existing
# installation. We omit these tests in that case because otherwise users
# would have to flex and bison available in order to install the binary
# distribution.
#
if test "$reconfiguring" = "no"
then

    AC_PROG_LEX
    if test "`basename $LEX`" != "flex"
    then
        AC_MSG_ERROR([You need flex to build Mercury])
    fi

    AC_PROG_YACC
    # AC_PROG_YACC may bind YACC to e.g. "bison -y"; we care only about
    # the program name.
    YACCPROG=`echo $YACC | sed 's/ .*//'`
    if test "`basename $YACCPROG`" != "bison"
    then
        AC_MSG_ERROR([You need bison to build Mercury])
    fi
fi

#-----------------------------------------------------------------------------#
#
# Check for a XUL browser and xsltproc and if we find both then set the
# default xml_browser_command and xml_tmp_filename mdb options.
#

AC_PATH_PROGS(XUL_BROWSER, firefox mozilla, "")
AC_PATH_PROGS(XSLTPROC, xsltproc, "")

if test "$XUL_BROWSER" != "" && test "$XSLTPROC" != ""; then
    DEFAULT_XML_BROWSER_CMD="$XSLTPROC $DEFAULT_MERCURY_DEBUGGER_INIT_DIR/xul_tree.xsl /tmp/mdbtmp.xml > /tmp/mdbtmp.xul && $XUL_BROWSER file:///tmp/mdbtmp.xul"
    DEFAULT_XML_TMP_FILENAME="/tmp/mdbtmp.xml"
else
    DEFAULT_XML_BROWSER_CMD=""
    DEFAULT_XML_TMP_FILENAME=""
fi

AC_SUBST(DEFAULT_XML_BROWSER_CMD)
AC_SUBST(DEFAULT_XML_TMP_FILENAME)

#-----------------------------------------------------------------------------#
#
# Set the default web_browser_cmd.
#

AC_PATH_PROGS(WEB_BROWSER, xdg-open, "")
case "$host" in
    *apple*darwin*)
        DEFAULT_WEB_BROWSER_CMD="open"
        ;;
    *-cygwin* | *mingw*)
        DEFAULT_WEB_BROWSER_CMD="start"
        ;;
    *)
        DEFAULT_WEB_BROWSER_CMD="xdg-open"
        ;;
esac

AC_SUBST(DEFAULT_WEB_BROWSER_CMD)

#-----------------------------------------------------------------------------#

# We need to check that any existing .c files are compatible with the selected
# grade with respect to whether --highlevel-code was used to generate them.
#
case "$GRADE" in
    hl*)
        need_hl_c_files="yes"
        ;;
    *)
        need_hl_c_files="no"
        ;;
esac

mercury_check_c_files () {
    c_dir=$1
    c_date_dir=$2

    case $GRADE in
        *.pregen*)
            need_low_tag_bits=2
            need_unboxed_floats=no
            need_unboxed_int64s=no
            need_pregen_dist=yes
            ;;
        *)
            need_low_tag_bits=$mercury_cv_low_tag_bits
            need_unboxed_floats=$mercury_cv_unboxed_floats
            need_unboxed_int64s=$mercury_cv_unboxed_int64s
            need_pregen_dist=no
            ;;
    esac

    # If the ALLOW_BOTH_HI_LO_LEVEL_CODE environment variable is set to
    # "allow", then don't pay attention to the setting of HIGHLEVEL_CODE;
    # allow both high and low level code. This is intended for situations
    # where a workspace already exists and is compiled. When configure.ac
    # changes, it has to rerun, but it should *not* cause the deletion
    # of the workspace's .c files just because the workspace's Mmake.params
    # file chose a different code level.
    #
    # In both cases, keep everything on one line to avoid any
    # line ending differences.
    if test "$ALLOW_BOTH_HI_LO_LEVEL_CODE" = "allow"
    then
        maybe_delete_level="-e /HIGHLEVEL_CODE/d"
        printf ";TAG_BITS=%s;UNBOXED_FLOAT=%s;UNBOXED_INT64S=%s;PREGENERATED_DIST=%s;" \
            $need_low_tag_bits \
            $need_unboxed_floats \
            $need_unboxed_int64s \
            $need_pregen_dist \
            > confscratch.ref
    else
        maybe_delete_level=""
        printf ";TAG_BITS=%s;UNBOXED_FLOAT=%s;UNBOXED_INT64S=%s;PREGENERATED_DIST=%s;HIGHLEVEL_CODE=%s;" \
            $need_low_tag_bits \
            $need_unboxed_floats \
            $need_unboxed_int64s \
            $need_pregen_dist \
            $need_hl_c_files > confscratch.ref
    fi

    for c_file in $c_dir/*.c
    do
        case $c_file in
        */'*.c')
            ;;
        */*_init.c)
            ;;
        *)
            sed -n -e "/END_OF_C_GRADE_INFO/q" $maybe_delete_level -e "/=/p" \
                < $c_file | tr -s '* \r\n' ';' > confscratch
            if cmp -s confscratch confscratch.ref
            then
                some_kept=true
            else
                c_date_file="$c_date_dir/`basename $c_file .c`.c_date"
                to_delete="$to_delete $c_file $c_date_file"
            fi
            ;;
        esac
    done
}

some_kept=false
to_delete=""
if test "$BOOTSTRAP_MC" != "" ; then
    MERCURY_MSG("checking whether any C files need to be rebuilt...")
    for directory in library compiler profiler deep_profiler mdbcomp browser slice ssdb mfilterjavac
    do
        mercury_check_c_files $directory $directory
        mercury_check_c_files $directory/Mercury/cs $directory/Mercury/c_dates
    done
    if test "$to_delete" = "" ; then
        case $some_kept in
            true)
                MERCURY_MSG("no - they are compatible with autoconfigured settings")
                ;;
            false)
                MERCURY_MSG("there are no existing Mercury-generated C files")
                ;;
          esac
    else
        case $some_kept in
            true)
                MERCURY_MSG(
"the following C files must be rebuilt in a grade
    that is consistent with autoconfigured settings
    $to_delete")
                ;;
            false)
                MERCURY_MSG(
"all Mercury generated C files must be rebuilt
    in a grade that is consistent with the
    autoconfigured settings")
                ;;
        esac
        MERCURY_MSG("installation may take longer than usual")
    fi
    rm -f confscratch confscratch.ref $to_delete
fi

#-----------------------------------------------------------------------------#

AC_MSG_CHECKING(for C shell executable)
cshpath=""
for prog in tcsh csh
do
    for dir in `echo $PATH | tr '[:]' '[ ]'`
    do
        if test -x "$dir/$prog"
        then
            if test "$cshpath" = ""
            then
                cshpath="$dir/$prog"
            fi
        fi
    done
done

if test "$cshpath" = ""
then
    AC_MSG_RESULT(no)
    CSHPATH="no csh path"
else
    AC_MSG_RESULT($cshpath)
    CSHPATH="$cshpath"
fi
AC_SUBST(CSHPATH)

#-----------------------------------------------------------------------------#

# When running the debugger tests we want to use the local mdbrc file,
# not the installed one, so that we can test changes to this file
# without having to install first. We therefore create a copy of the mdbrc.in
# for use with the test suite.

test_mdbrc=tests/mdbrc

cat "`pwd`"/scripts/mdbrc.in | sed \
    -e 's/DEFAULT_MERCURY_DEBUGGER_INIT_DIR/TESTS_MDB_INIT_DIR/g' \
    -e 's/DEFAULT_MERCURY_DEBUGGER_DOC/TESTS_MDB_DOC/g' > $test_mdbrc.in

TESTS_MDB_INIT_DIR="`pwd`"/scripts
TESTS_MDB_DOC="`pwd`"/doc/mdb_doc
AC_SUBST(TESTS_MDB_INIT_DIR)
AC_SUBST(TESTS_MDB_DOC)

conf_h_copy=/tmp/mercury_conf.h.$$
trap "rm -f $conf_h_copy" 0 1 2 3 13 15
if test -f runtime/mercury_conf.h
then
    /bin/rm -f $conf_h_copy
    cp runtime/mercury_conf.h $conf_h_copy
    had_old_conf_h=true
else
    had_old_conf_h=false
fi
# The references to $conf_h_copy and $had_old_conf_h below are in code that
# autoconf puts into a subshell, so we must export them.
export conf_h_copy
export had_old_conf_h

executable_output_files='
scripts/mmc
scripts/mercury.bat
scripts/mprof
scripts/mprof.bat
scripts/mercury_update_interface
scripts/mgnuc
scripts/ml
scripts/c2init
scripts/mmake
scripts/mdb
scripts/mdb.bat
scripts/mdprof
scripts/mtags
scripts/canonical_grade
scripts/mkfifo_using_mknod
scripts/mercury_config
scripts/prepare_install_dir
tools/lmc
tools/dotime
'

nonexecutable_output_files='
Mmake.common
scripts/Mmake.vars
scripts/parse_ml_options.sh-subr
scripts/mdbrc
scripts/Mercury.config
scripts/Mercury.config.bootstrap
bindist/bindist.INSTALL
bindist/bindist.Makefile
runtime/mercury_dotnet.cs
java/runtime/Constants.java
java/runtime/Native.java
library/erlang_conf.hrl
compiler/COMP_FLAGS
library/LIB_FLAGS
mdbcomp/MDBCOMP_FLAGS
browser/MDB_FLAGS
ssdb/SSDB_FLAGS
slice/SLICE_FLAGS
profiler/PROF_FLAGS
deep_profiler/DEEP_FLAGS
tests/TESTS_FLAGS
tests/DEFNS_FOR_TESTS
mfilterjavac/MFILTERJAVAC_FLAGS
'

# The order in which we output files matters, because in some cases, one of the
# files we output (e.g. scripts/parse_ml_options.sh-subr) will be bodily
# included in other files we output (e.g. scripts/ml, scripts/c2init etc).
# If we attempted to create the latter files first, the bodily inclusion would
# include the empty file instead.
output_files="$nonexecutable_output_files $executable_output_files"

# IMPORTANT NOTE
# --------------
# Any new entries here may need to be handled by scripts/mercury_config.in.
# Failing to do this correctly may break the binary distributions
# This is especially true of files in the runtime or java directories;
# if you add new .in files to either of those you *must* update
# scripts/mercury_config.in.

AC_OUTPUT($output_files
"$test_mdbrc"
,
[
# Only do this when compiling the source, not when reconfiguring
# an installation.
case $reconfiguring in no)
    for header in $CONFIG_HEADERS ; do
        if test "$header" = "runtime/mercury_conf.h"; then
            if $had_old_conf_h && \
                cmp runtime/mercury_conf.h "$conf_h_copy" > /dev/null && \
                test -f runtime/mercury_conf.h.date
            then
                # The date file need not be updated, so don't update it,
                # since that would lead to the recompilation of a bunch
                # of files.
                true
            else
                touch runtime/mercury_conf.h.date
            fi
        fi
    done
    # conftest.junk is used to avoid a warning if there
    # are no files in the list passed to chmod
    touch conftest.junk
    chmod +x `echo $CONFIG_FILES | \
        sed -e 's/[[^ ]]*Mmake.common//' ` conftest.junk
    rm -f conftest.junk

    # The --prefix is hard-coded in the scripts, which are regenerated
    # every time you run configure, and also (unfortunately) in the .so files.
    # The following rm commands are here to ensure that things will work
    # correctly if you rerun configure with a new --prefix and then
    # don't do a `make clean' before running `make'.
    rm -f runtime/libmer_rt.so library/libmer_std.so bytecode/libmbi.so
    rm -f trace/libmer_trace.so browser/libmer_browser.so
    rm -f mdbcomp/libmer_mdbcomp.so
    rm -f runtime/libmer_rt.dylib library/libmer_std.dylib
    rm -f boehm_gc/libgc.dylib bytecode/libmbi.dylib
    rm -f trace/libmer_trace.dylib browser/libmer_browser.dylib
    rm -f mdbcomp/libmer_mdbcomp.dylib
    ;;
esac],
[reconfiguring=$reconfiguring]
)

for file in $nonexecutable_output_files
do
    chmod a-x "$file"
done

MERCURY_MSG("")
MERCURY_MSG("using ${DEFAULT_GRADE} as the default grade for applications")
MERCURY_MSG("using ${BEST_GRADE_FOR_COMPILER} to compile the compiler")

if test "${BOOTSTRAP_MC_COMPILER}" != ""
then
    MERCURY_MSG("")
    MERCURY_MSG("using ${BOOTSTRAP_MC_COMPILER} as the installed compiler")
fi
if test "${PREFIX}" != "" -a "${PREFIX}" != "${DEFAULT_PREFIX}"
then
    MERCURY_MSG("")
    MERCURY_MSG("using ${PREFIX} as the install prefix")
fi

MERCURY_MSG("")
MERCURY_MSG("the set of library grades to install will be")
num_installed_grades=0
> .configured_library_grades
for grade in $LIBGRADES
do
    MERCURY_MSG("   $grade")
    echo "$grade" >> .configured_library_grades
    num_installed_grades=`expr $num_installed_grades + 1`
done
MERCURY_MSG("")

if test "$have_default_grade" = "no"
then
    AC_MSG_WARN([the default grade for applications is $DEFAULT_GRADE,
****    but this grade is not going to be installed.])
fi

min_minutes_per_grade=10
max_minutes_per_grade=30
min_minutes=`expr $num_installed_grades \* $min_minutes_per_grade`
max_minutes=`expr $num_installed_grades \* $max_minutes_per_grade`
min_hours=`expr '(' $min_minutes + 30 ')' / 60`
max_hours=`expr '(' $max_minutes + 30 ')' / 60`
MERCURY_MSG("Configuring to install $num_installed_grades grades.")
if test $max_hours -gt 2 -a $min_hours -ne $max_hours
then
    MERCURY_MSG("This will likely take $min_hours to $max_hours hours.")
else
    MERCURY_MSG("This will likely take $min_minutes to $max_minutes minutes.")
fi
MERCURY_MSG("")
# Putting commas directly into the message confuses m4, even if escaped.
COMMA=","
MERCURY_MSG("You can make the install faster by installing fewer grades${COMMA}")
MERCURY_MSG("as shown by the fine-tuning section of the INSTALL file${COMMA}")
MERCURY_MSG("or by compiling the files of each grade in parallel${COMMA}")
MERCURY_MSG("which you can do via a command such as make PARALLEL=-j2 install.")

#-----------------------------------------------------------------------------#
