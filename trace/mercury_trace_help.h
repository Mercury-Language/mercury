/*
** Copyright (C) 1998,2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trace_help.h
**
** Defines the interface of the help system for the internal debugger.
*/

#ifndef	MERCURY_TRACE_HELP_H
#define MERCURY_TRACE_HELP_H

#include "mercury_trace_completion.h"

/*
** These function add a help node, which must a category or an item
** within a category. It returns NULL if the addition was successful,
** and a pointer to an error message otherwise.
*/

extern	const char	*MR_trace_add_cat(const char *category, int slot,
				const char *text);

extern	const char	*MR_trace_add_item(const char *category,
				const char *item, int slot, const char *text);

/*
** These functions print help to standard output.
**
** MR_trace_help prints a list of the top-level help nodes.
** MR_trace_help_word prints the text of all the help nodes with the given
**	name. If there are none, it prints a list of the top-level help nodes.
** MR_trace_help_cat_item prints the text of the node at path cat/item.
*/

extern	void		MR_trace_help(void);
extern	void		MR_trace_help_word(const char *word);

extern	void		MR_trace_help_cat_item(const char *cat,
				const char *item);

/* A Readline completer for help topics. */
extern  MR_Completer_List *MR_trace_help_completer(const char *word,
				size_t word_len);

#endif	/* MERCURY_TRACE_HELP_H */
