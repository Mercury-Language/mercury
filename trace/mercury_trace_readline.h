/*
** Copyright (C) 1998-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trace_readline.h: simple interface functions to read a line.
**
** Main authors: fjh, zs.
*/

#include <stdio.h>	/* for FILE */

/*
** Print the prompt to the `out' file, read a line from the `in' file,
** possibly using GNU readline for command-line editing, and return it
** in a malloc'd buffer holding the line (without the final newline).
** If EOF occurs on a nonempty line, treat the EOF as a newline; if EOF
** occurs on an empty line, return NULL.
*/
char *	MR_trace_readline(const char *prompt, FILE *in, FILE *out);

/*
** Read a line from a file, and return a pointer to a malloc'd buffer
** holding the line (without the final newline). If EOF occurs on a
** nonempty line, treat the EOF as a newline; if EOF occurs on an empty
** line, return NULL.  Don't use GNU readline.
*/
char *	MR_trace_readline_raw(FILE *in);
