/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	MERCURY_TRACE_INTERNAL_H
#define	MERCURY_TRACE_INTERNAL_H

#include "mercury_types.h"
#include "mercury_trace.h"

extern	Code	*MR_trace_event_internal(MR_Trace_Cmd_Info *cmd,
			bool interactive,
			const MR_Stack_Layout_Label *layout,
			Word *saved_regs, MR_Trace_Port port,
			int seqno, int depth,
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
