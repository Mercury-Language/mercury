/*
** Copyright (C) 1998-2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trace_browse.h
**
** Defines the interface of the term browser and the interactive query
** facility for the internal and external debuggers.
*/

#ifndef	MERCURY_TRACE_BROWSE_H
#define MERCURY_TRACE_BROWSE_H

#include "mercury_conf.h"	/* for MR_USE_EXTERNAL_DEBUGGER */
#include "mercury_types.h"	/* for MR_Word, MR_String */

/*
** Interactively browse a term.
*/
extern 	void	MR_trace_browse(MR_Word type_info, MR_Word value);
#ifdef MR_USE_EXTERNAL_DEBUGGER
extern 	void	MR_trace_browse_external(MR_Word type_info, MR_Word value);
#endif

/*
** Display a term (non-interactively).
*/
extern	void	MR_trace_print(MR_Word type_info, MR_Word value);


/*
** Invoke an interactive query.
*/

/* This must kept in sync with query_type in browser/interactive.m. */
typedef enum { MR_NORMAL_QUERY, MR_CC_QUERY, MR_IO_QUERY } MR_Query_Type;

extern	void	MR_trace_query(MR_Query_Type type, const char *options,
			int num_imports, /* const */ char *imports[]);

#ifdef MR_USE_EXTERNAL_DEBUGGER
extern	void	MR_trace_query_external(MR_Query_Type type, MR_String options,
			int num_imports, MR_Word imports_list);
#endif

#endif	/* MERCURY_TRACE_BROWSE_H */
