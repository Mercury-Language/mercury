#define NOSAY	1	/* To disable SAYings */
/*
** Copyright (C) 1997,2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
*/

/* Imports */
#include	<stdio.h>
#include	<stdarg.h>
#include	<stdlib.h>
#include	<string.h>

#include	"mb_mem.h"
#include	"mb_util.h"

/* Exported definitions */

void		MB_util_error(const char *fmt, ...);
void		MB_fatal(const char *message);
int		MB_str_cmp(MB_CString_Const a, MB_CString_Const b);
MB_CString	MB_str_new(MB_Word len);
MB_CString	MB_str_new_cat(MB_CString_Const a, MB_CString_Const b);
MB_CString	MB_str_dup(MB_CString_Const str);
void		MB_str_delete(MB_CString str);


/* Local declarations */

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

void MB_SAY(const char *fmt, ...)
{
#if NOSAY

#else
	va_list arg_p;
	va_start(arg_p, fmt);
	vfprintf(stderr, fmt, arg_p);
	va_end(argp);
	fprintf(stderr, "\n");
	fflush(stdout); /* in case redirected to stdout */
#endif
}

/* prints an error and aborts program */
void
MB_fatal(const char *message)
{
	MB_util_error(message);
	fprintf(stderr, " NOTE: The program will now abort.\n");

	abort();

	return; /* not reached */
}

/* compare two strings */
int MB_str_cmp(MB_CString_Const a, MB_CString_Const b) {
	return strcmp(a, b);
}

MB_CString
MB_str_new(MB_Word len)
{
	MB_CString c = MB_GC_NEW_ARRAY_ATOMIC(char, len + 1);
	if (c == NULL) MB_fatal("Not enough string space");

	return c;
}

MB_CString
MB_str_new_cat(MB_CString_Const a, MB_CString_Const b)
{
	MB_Word len_a = strlen(a);
	MB_Word len_b = strlen(b);
	MB_CString new_str = MB_str_new(len_a + len_b + 1);

	memcpy(new_str, a, len_a);
	memcpy(new_str + len_a, b, len_b);

	new_str[len_a + len_b] = 0;

	return new_str;
}

MB_CString
MB_str_dup(MB_CString_Const str)
{
	MB_CString c = MB_str_new(strlen(str) + 1);
	strcpy(c, str);
	return c;
}

void
MB_str_delete(MB_CString str)
{
	MB_GC_free(str);
}

