/*
** Copyright (C) 2005 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trace_term.h declares the runtime's facilities to manipulate terms.
** We use this capability to implement conditional breakpoints.
*/

#include <stdio.h>

#ifndef MERCURY_TRACE_TERM_H
#define MERCURY_TRACE_TERM_H

typedef	struct MR_CTerm_Struct	*MR_CTerm;
typedef	struct MR_CArgs_Struct	*MR_CArgs;

struct MR_CTerm_Struct {
	char		*term_functor;
	MR_CArgs	term_args;
};

struct MR_CArgs_Struct {
	MR_CTerm	args_head;
	MR_CArgs	args_tail;
};

/*
** Read a term from the string starting at str, and leave *rest pointing
** to the first character after the term. In case of a syntax error, the return
** value will be NULL, and *rest is not valid.
*/

extern	MR_CTerm	MR_create_cterm(char *str, char **rest);

/*
** Print this term to the given file.
*/

extern	void		MR_print_cterm(FILE *fp, MR_CTerm term);

/*
** Release the memory taken by the structures inside term. The memory taken
** by the strings representing function symbols are not released; their freeing
** is up to the caller.
*/

extern	void		MR_delete_cterm(MR_CTerm term);

#endif /* MERCURY_TRACE_TERM_H */
