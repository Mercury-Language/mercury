/*
** Copyright (C) 1998-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef MERCURY_TRACE_DECLARATIVE_H
#define MERCURY_TRACE_DECLARATIVE_H

#include "mercury_imp.h"
#include "mercury_trace.h"
#include "mercury_trace_internal.h"

#ifdef MR_USE_DECLARATIVE_DEBUGGER

/*
** When in declarative debugging mode, the internal debugger calls
** MR_trace_decl_debug for each event.  
*/

extern	MR_Code	*MR_trace_decl_debug(MR_Trace_Cmd_Info *cmd,
			MR_Event_Info *event_info);

/*
** The internal (interactive) debugger calls this function to enter
** declarative debugging mode.  It returns MR_TRUE if successful, and
** MR_FALSE if there was some problem that prevented this mode from
** being entered.
*/

extern	MR_bool	MR_trace_start_decl_debug(MR_Trace_Mode trace_mode,
			const char *out, MR_Trace_Cmd_Info *cmd,
			MR_Event_Info *event_info,
			MR_Event_Details *event_details, MR_Code **jumpaddr);

/*
** The following macros are provided to help C code manipulate the
** Mercury data structure.  The values here must match the corresponding
** values in the definitions in browser/declarative_execution.m.
*/

typedef MR_Word MR_Trace_Node;

#define MR_TRACE_STATUS_SUCCEEDED	(MR_Word) 0
#define MR_TRACE_STATUS_FAILED		(MR_Word) 1
#define MR_TRACE_STATUS_UNDECIDED	(MR_Word) 2

#endif  /* MR_USE_DECLARATIVE_DEBUGGER */
#endif	/* MERCURY_TRACE_DECLARATIVE_H */
