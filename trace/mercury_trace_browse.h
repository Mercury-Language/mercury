/*
** Copyright (C) 1998-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trace_browse.h
**
** Defines the interface of the term browser and the interactive query
** facility for the internal debugger.
*/

#ifndef	MERCURY_TRACE_BROWSE_H
#define MERCURY_TRACE_BROWSE_H

/*
** Interactively browse a term.
*/
extern 	void	MR_trace_browse(Word type_info, Word value);

/*
** Display a term (non-interactively).
*/
extern	void	MR_trace_print(Word type_info, Word value);


/*
** Invoke an interactive query.
*/

/* This must kept in sync with query_type in browser/interactive.m. */
typedef enum { MR_NORMAL_QUERY, MR_CC_QUERY, MR_IO_QUERY } MR_Query_Type;

extern	void	MR_trace_query(MR_Query_Type type, const char *options,
			int num_imports, /* const */ char *imports[]);

#endif	/* MERCURY_TRACE_BROWSE_H */
