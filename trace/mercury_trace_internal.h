/*
** Copyright (C) 1998-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	MERCURY_TRACE_INTERNAL_H
#define	MERCURY_TRACE_INTERNAL_H

#include "mercury_types.h"
#include "mercury_trace.h"

#ifdef  MR_USE_DECLARATIVE_DEBUGGER

/*
** The following enum gives the possible modes that the declarative
** debugger can be in (see trace/mercury_trace_declarative.{c,h}).
** MR_TRACE_INTERACTIVE indicates the usual operation of the internal
** debugger.  The other modes refer to what type of analysis is
** being performed.
*/

typedef enum {
	MR_TRACE_INTERACTIVE,
	MR_TRACE_WRONG_ANSWER
} MR_Trace_Mode;

/*
** This variable is modified whenever we start or stop collecting
** an EDT for a particular type of analysis (see
** trace/mercury_trace_declarative.c).
*/

extern	MR_Trace_Mode	MR_trace_decl_mode;

#endif	/* MR_USE_DECLARATIVE_DEBUGGER */

extern	Code	*MR_trace_event_internal(MR_Trace_Cmd_Info *cmd,
			bool interactive,
			const MR_Stack_Layout_Label *layout,
			Word *saved_regs, MR_Trace_Port port,
			int seqno, int depth,
			const char *path, int *max_mr_num);


extern	Code	*MR_trace_event_internal_report(MR_Trace_Cmd_Info *cmd,
			const MR_Stack_Layout_Label *layout, Word *saved_regs,
			MR_Trace_Port port, int seqno, int depth,
			const char *path, int *max_mr_num);

/*
** Debugger I/O streams.
** Replacements for stdin/stdout/stderr respectively.
**
** The distinction between MR_mdb_out and MR_mdb_err is analagous to
** the distinction between stdout and stderr: ordinary output, including
** information messages about conditions which are not errors, should
** go to MR_mdb_out, but error messages should go to MR_mdb_err.
*/
extern FILE *MR_mdb_in;
extern FILE *MR_mdb_out;
extern FILE *MR_mdb_err;

#endif	/* MERCURY_TRACE_INTERNAL_H */
