#-----------------------------------------------------------------------------#
# Copyright (C) 1999 The University of Melbourne.
# This file may only be copied under the terms of the GNU General
# Public Licence - see the file COPYING in the Mercury distribution.
#-----------------------------------------------------------------------------#
#
# aclocal.m4
#
# This file contains Mercury-specific autoconf tests.
#
# We ought to move most of the code in configure.in and
# bindist/bindist.configure.in into this file...
#
#-----------------------------------------------------------------------------#
#
# Check for readline and related header files and libraries
#
AC_DEFUN(MERCURY_CHECK_READLINE,
[

# check for the readline header files
AC_CHECK_HEADER(readline/readline.h, HAVE_READLINE_READLINE_H=1)
if test "$HAVE_READLINE_READLINE_H" = 1; then
	AC_DEFINE(HAVE_READLINE_READLINE)
fi
AC_CHECK_HEADER(readline/history.h, HAVE_READLINE_HISTORY_H=1)
if test "$HAVE_READLINE_HISTORY_H" = 1; then
	AC_DEFINE(HAVE_READLINE_HISTORY)
fi

# check for the readline library
AC_CHECK_LIB(readline, readline, mercury_cv_have_readline=yes,
	mercury_cv_have_readline=no)

# check for the libraries that readline depends on
if test $mercury_cv_have_readline = yes; then
	MERCURY_MSG('looking for termcap or curses (needed by readline)...')
	AC_CHECK_LIB(termcap, tgetent, mercury_cv_termcap_lib=-ltermcap,
	 [AC_CHECK_LIB(curses,  tgetent, mercury_cv_termcap_lib=-lcurses,
	  [AC_CHECK_LIB(ncurses, tgetent, mercury_cv_termcap_lib=-lncurses,
	   mercury_cv_termcap_lib=none)])])
fi

# Now figure out whether we can use readline, and define variables according.
# Note that on most systems, we don't actually need the header files in
# order to use readline. (Ain't C grand? ;-).

if test $mercury_cv_have_readline = no ||
	test $mercury_cv_termcap_lib = none
then
	TERMCAP_LIBRARY=""
	READLINE_LIBRARIES=""
	AC_DEFINE(MR_NO_USE_READLINE)
else
	TERMCAP_LIBRARY="$mercury_cv_termcap_lib"
	READLINE_LIBRARIES="-lreadline $TERMCAP_LIBRARY"
fi
AC_SUBST(TERMCAP_LIBRARY)
AC_SUBST(READLINE_LIBRARIES)

])

#-----------------------------------------------------------------------------#
