/*
** Copyright (C) 1998-2000,2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** A simple interface to read a line, normally done using GNU readline.
**
** This module is compiled with warnings disabled (mgnuc --no-check),
** since the GNU readline headers don't use prototypes, const, etc.
**
** Main authors: fjh, zs.
*/

#include "mercury_imp.h"
#include "mercury_array_macros.h"
#include "mercury_memory.h"
#include "mercury_std.h"

#include "mercury_trace_readline.h"

#ifndef MR_NO_USE_READLINE
  #ifdef HAVE_READLINE_READLINE
    #include "readline/readline.h"
  #else
    FILE *rl_instream;
    FILE *rl_outstream;
  #endif
  #ifdef HAVE_READLINE_HISTORY
    #include "readline/history.h"
  #endif
  #ifdef HAVE_UNISTD_H
     #include <unistd.h>	/* for isatty() */
  #endif
#endif

#include <stdio.h>
#include <stdlib.h>

/* The initial size of the array of characters used to hold the line. */
#define	MR_INIT_BUF_LEN		80

/*
** Print the prompt to the `out' file, read a line from the `in' file,
** and return it in a MR_malloc'd buffer holding the line (without the
** final newline).
** If EOF occurs on a nonempty line, treat the EOF as a newline; if EOF
** occurs on an empty line, return NULL.
*/
char *
MR_trace_readline(const char *prompt, FILE *in, FILE *out)
{
#if (defined(isatty) || defined(HAVE_ISATTY)) \
 && (defined(fileno) || defined(HAVE_FILENO)) \
 && !defined(MR_NO_USE_READLINE)
	/* use readline, if the input file is a terminal */
	if (isatty(fileno(in))) {
		char	*line;

		rl_instream = in;
		rl_outstream = out;

		line = readline((char *) prompt);

		/*
		** readline() allocates with malloc(), and we want
		** to return something allocated with MR_malloc(),
		** but that's OK, because MR_malloc() and malloc()
		** are interchangable.
		*/
#if 0
		{
			char *tmp = line;

			line = MR_copy_string(line);
			free(tmp);
		}
#endif

		if (line != NULL && line[0] != '\0') {
			add_history(line);
		}

		return line;
	}
#endif /* have isatty && have fileno && !MR_NO_USE_READLINE */

	/* otherwise, don't use readline */
	fprintf(out, "%s", prompt);
	fflush(out);
	return MR_trace_readline_raw(in);
}

/*
** Read a line from a file, and return a pointer to a MR_malloc'd buffer
** holding the line (without the final newline). If EOF occurs on a
** nonempty line, treat the EOF as a newline; if EOF occurs on an empty
** line, return NULL.
*/
char *
MR_trace_readline_raw(FILE *fp)
{
	char	*contents;
	int	content_max;
	int	c;
	int	i;

	contents = NULL;
	content_max = 0;

	i = 0;
	while ((c = getc(fp)) != EOF && c != '\n') {
		MR_ensure_big_enough(i, content, char, MR_INIT_BUF_LEN);
		contents[i++] = c;
	}

	if (c == '\n' || i > 0) {
		MR_ensure_big_enough(i, content, char, MR_INIT_BUF_LEN);
		contents[i] = '\0';
		return contents;
	} else {
		MR_free(contents);
		return NULL;
	}
}
