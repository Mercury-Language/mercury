
/*
** Copyright (C) 2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: mb_util.c,v 1.1 2001-01-24 07:42:28 lpcam Exp $
*/

/* Imports */
#include	<stdio.h>
#include	<stdarg.h>
#include	<stdlib.h>
#include	<string.h>

#include	"mb_util.h"

/* Exported definitions */

int MB_strcmp(MB_CString_Const a, MB_CString_Const b);
void MB_util_error(const char *fmt, ...);
void MB_fatal(const char* message);

/* Local declarations */

static char
rcs_id[]	= "$Id: mb_util.c,v 1.1 2001-01-24 07:42:28 lpcam Exp $";

/* Implementation */

/* Prints an error to standard err (doesn't exit) */
void
MB_util_error(const char *fmt, ...)
{
	va_list arg_p;

	fprintf(stderr, "Error: ");
	va_start(arg_p, fmt);
	vfprintf(stderr, fmt, arg_p);
	va_end(argp);
	fprintf(stderr, "\n");
}

/* prints an error and aborts program */
void
MB_fatal(const char* message)
{
	MB_util_error(message);
	fprintf(stderr, " NOTE: The program will now abort.\n");

	abort();

	return; /* not reached */
}

/* compare two strings */
int MB_strcmp(MB_CString_Const a, MB_CString_Const b) {
	return strcmp(a, b);
}



