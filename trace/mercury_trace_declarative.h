/*
** Copyright (C) 1998-2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef MERCURY_TRACE_DECLARATIVE_H
#define MERCURY_TRACE_DECLARATIVE_H

#include "mercury_imp.h"
#include "mercury_trace.h"

/*
** When in declarative debugging mode, the internal debugger calls
** MR_trace_decl_debug for each event.  
*/

extern	Code	*MR_trace_decl_debug(MR_Trace_Cmd_Info *cmd,
			MR_Event_Info *event_info);

/*
** The internal (interactive) debugger calls this function to enter
** declarative debugging mode.  It returns TRUE if successful, and
** FALSE if there was some problem that prevented this mode from
** being entered.
*/

extern	bool	MR_trace_start_decl_debug(const char *out,
			MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info,
			MR_Event_Details *event_details, Code **jumpaddr);

/*
** The following macros are provided to help C code manipulate the
** Mercury data structure.  The values here must match the corresponding
** values in the definitions in browser/declarative_execution.m.
*/

typedef Word MR_Trace_Node;

#define	MR_trace_call_node_last_interface(node)				\
		MR_field(MR_mktag(0), (node), (Integer) 1)

#define MR_trace_cond_node_status(node)					\
		MR_field(MR_mktag(3), (node), (Integer) 3)

#define MR_trace_neg_node_status(node)					\
		MR_field(MR_mktag(3), (node), (Integer) 3)

#define MR_TRACE_STATUS_SUCCEEDED	(Word) 0
#define MR_TRACE_STATUS_FAILED		(Word) 1
#define MR_TRACE_STATUS_UNDECIDED	(Word) 2

#define MR_trace_atom(atom, name, args)					    \
	do {								    \
		tag_incr_hp((atom), MR_mktag(0), 2);			    \
		MR_field(MR_mktag(0), (atom), (Integer) 0) = (Word) (name); \
		MR_field(MR_mktag(0), (atom), (Integer) 1) = (args);	    \
	} while(0)

#endif	/* MERCURY_TRACE_DECLARATIVE_H */
