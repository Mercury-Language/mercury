#-----------------------------------------------------------------------------#
# Copyright (C) 1999,2001 The University of Melbourne.
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
AC_ARG_WITH(readline,
[  --without-readline      Don't use the GPL'd GNU readline library],
mercury_cv_with_readline="$withval", mercury_cv_with_readline=yes)

if test "$mercury_cv_with_readline" = yes; then

	# check for the readline header files
	AC_CHECK_HEADER(readline/readline.h, HAVE_READLINE_READLINE_H=1)
	if test "$HAVE_READLINE_READLINE_H" = 1; then
		AC_DEFINE(HAVE_READLINE_READLINE)
	fi
	AC_CHECK_HEADER(readline/history.h, HAVE_READLINE_HISTORY_H=1)
	if test "$HAVE_READLINE_HISTORY_H" = 1; then
		AC_DEFINE(HAVE_READLINE_HISTORY)
	fi

	# check for the libraries that readline depends on
	MERCURY_MSG('looking for termcap or curses (needed by readline)...')
	AC_CHECK_LIB(termcap, tgetent, mercury_cv_termcap_lib=-ltermcap,
	 [AC_CHECK_LIB(curses,  tgetent, mercury_cv_termcap_lib=-lcurses,
	  [AC_CHECK_LIB(ncurses, tgetent, mercury_cv_termcap_lib=-lncurses,
	   mercury_cv_termcap_lib='')])])

	# check for the readline library
	AC_CHECK_LIB(readline, readline, mercury_cv_have_readline=yes,
		mercury_cv_have_readline=no, $mercury_cv_termcap_lib)
else
	mercury_cv_have_readline=no
fi

# Now figure out whether we can use readline, and define variables according.
# Note that on most systems, we don't actually need the header files in
# order to use readline. (Ain't C grand? ;-).

if test $mercury_cv_have_readline = no; then
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
#
# Microsoft.NET configuration
#
AC_DEFUN(MERCURY_CHECK_DOTNET,
[
AC_PATH_PROG(ILASM, ilasm)
AC_PATH_PROG(GACUTIL, gacutil)

AC_MSG_CHECKING(for Microsoft.NET Framework SDK)
AC_CACHE_VAL(mercury_cv_microsoft_dotnet, [
if test "$ILASM" != ""; then
	changequote(<<,>>) 
	MS_DOTNET_SDK_DIR=`expr "$ILASM" : '\(.*\)[/\\]*[bB]in[/\\]*ilasm'`
	changequote([,]) 
	mercury_cv_microsoft_dotnet="yes"
else
	MS_DOTNET_SDK_DIR=""
	mercury_cv_microsoft_dotnet="no"
fi
])
AC_MSG_RESULT($mercury_cv_microsoft_dotnet)
ILASM=`basename "$ILASM"`
GACUTIL=`basename "$GACUTIL"`

AC_PATH_PROG(MS_CL, cl)
AC_MSG_CHECKING(for Microsoft.NET Visual C++)
AC_CACHE_VAL(mercury_cv_microsoft_visual_cpp, [
if test "$MS_CL" != ""; then
	changequote(<<,>>) 
	MS_VISUALCPP_DIR=`expr "$MS_CL" : '\(.*\)[/\\]*[bB]in[/\\]*cl'`
	changequote([,]) 
	mercury_cv_microsoft_visual_cpp="yes"
else
	MS_VISUALCPP_DIR=""
	mercury_cv_microsoft_visual_cpp="no"
fi
])
AC_MSG_RESULT($mercury_cv_microsoft_visual_cpp)
MS_CL=`basename "$MS_CL"`

AC_SUBST(ILASM)
AC_SUBST(GACUTIL)
AC_SUBST(MS_CL)
AC_SUBST(MS_DOTNET_SDK_DIR)
AC_SUBST(MS_VISUALCPP_DIR)
])

#-----------------------------------------------------------------------------#
