/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	MERCURY_TRACE_INTERNAL_H
#define	MERCURY_TRACE_INTERNAL_H

extern	Code	*MR_trace_event_internal(MR_Trace_Cmd_Info *cmd,
			bool interactive,
			const MR_Stack_Layout_Label *layout,
			Word *saved_regs, MR_Trace_Port port,
			int seqno, int depth,
			const char *path, int *max_mr_num);

#endif	/* MERCURY_TRACE_INTERNAL_H */
