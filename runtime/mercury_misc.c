/*
** Copyright (C) 1996-2000,2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include	"mercury_conf.h"
#ifndef MR_HIGHLEVEL_CODE
  #include	"mercury_imp.h"
#endif
#include	"mercury_string.h"
#include	"mercury_misc.h"

#include	<stdio.h>
#include	<stdarg.h>
#include	<errno.h>

static void MR_print_warning(const char *prog, const char *fmt, va_list args);
static void MR_do_perror(const char *prog, const char *message);

void
MR_warning(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	MR_print_warning("Mercury runtime", fmt, args);
	va_end(args);
	
}

void
MR_mdb_warning(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	MR_print_warning("mdb", fmt, args);
	va_end(args);
}

static void
MR_print_warning(const char *prog, const char *fmt, va_list args)
{

	fflush(stdout);		/* in case stdout and stderr are the same */

	fprintf(stderr, prog);
	fprintf(stderr, ": ");
	vfprintf(stderr, fmt, args);
	fprintf(stderr, "\n");

	fflush(stderr);
}

void
MR_perror(const char *message)
{
	MR_do_perror("Mercury runtime", message);
}

void
MR_mdb_perror(const char *message)
{
	MR_do_perror("mdb", message);
}

static void
MR_do_perror(const char *prog, const char *message)
{
	int saved_errno;

	saved_errno = errno;
	fflush(stdout);		/* in case stdout and stderr are the same */

	fprintf(stderr, prog);
	fprintf(stderr, ": ");
	errno = saved_errno;
	perror(message);
}

/*
** XXX will need to modify this to kill other threads if MR_THREAD_SAFE
** (and cleanup resources, etc....)
*/

void 
MR_fatal_error(const char *fmt, ...)
{
	va_list args;

	fflush(stdout);		/* in case stdout and stderr are the same */

	fprintf(stderr, "Mercury runtime: ");
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");

#ifndef MR_HIGHLEVEL_CODE
	MR_trace_report(stderr);
#endif

	fflush(NULL);		/* flushes all stdio output streams */

	exit(EXIT_FAILURE);
}

/*
**  Note that MR_hash_string is actually defined as a macro in mercury_imp.h,
**  if we're using GNU C.  We define it here whether or not we're using
**  gcc, so that users can easily switch between gcc and cc without
**  rebuilding the libraries.
*/

#undef MR_hash_string

int 
MR_hash_string(MR_Word s)
{
	MR_HASH_STRING_FUNC_BODY
}
