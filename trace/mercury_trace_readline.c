/*
** Copyright (C) 1998-2002 The University of Melbourne.
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
#include "mercury_wrapper.h"

#include "mercury_trace_readline.h"
#include "mercury_trace_completion.h"

#ifndef MR_NO_USE_READLINE
  #ifdef MR_HAVE_READLINE_READLINE_H
    #include "readline/readline.h"
  #else
    extern FILE *rl_instream;
    extern FILE *rl_outstream;
    extern char (*rl_completion_entry_function)(const char *, int);
    extern const char *rl_readline_name;
    extern void (*rl_prep_term_function)(int);
    extern void (*rl_deprep_term_function)(void);
  #endif
  #ifdef MR_HAVE_READLINE_HISTORY_H
    #include "readline/history.h"
  #endif
  #ifdef MR_HAVE_UNISTD_H
     #include <unistd.h>	/* for isatty() */
  #endif
#endif

#include <stdio.h>
#include <stdlib.h>

/* The initial size of the array of characters used to hold the line. */
#define	MR_INIT_BUF_LEN		80

static	void	MR_dummy_prep_term_function(int ignored);
static	void	MR_dummy_deprep_term_function(void);

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
#if (defined(isatty) || defined(MR_HAVE_ISATTY)) \
 && (defined(fileno) || defined(MR_HAVE_FILENO)) \
 && !defined(MR_NO_USE_READLINE)
	char	*line;
 	MR_bool	in_isatty;

	in_isatty = isatty(fileno(in));
	if (in_isatty || MR_force_readline) {

		rl_instream = in;
		rl_outstream = out;

		/*
		** The cast to (void *) silences a spurious "assignment from
		** incompatible pointer type" warning (old versions of
		** readline are very sloppy about declaring the types of
		** function pointers).
		*/
		rl_completion_entry_function =
			(void *) &MR_trace_line_completer;
		rl_readline_name = "mdb";

		if (!in_isatty) {
			/*
			** This is necessary for tests/debugger/completion,
			** otherwise we get lots of messages about readline
			** not being able to get the terminal settings.
			** This is possibly a bit flaky, but it's only
			** used by our tests.
			*/
			rl_prep_term_function =
				(void *) MR_dummy_prep_term_function;
			rl_deprep_term_function =
				(void *) MR_dummy_deprep_term_function;
		}	

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

static void
MR_dummy_prep_term_function(int ignored)
{
}

static void
MR_dummy_deprep_term_function(void)
{
}
