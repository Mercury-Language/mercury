/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	MERCURY_TRACE_INTERNAL_H
#define	MERCURY_TRACE_INTERNAL_H

extern	void	MR_trace_event_internal(MR_trace_cmd_info *cmd,
			const MR_Stack_Layout_Label *layout,
			MR_trace_port port, int seqno, int depth,
			const char *path);

extern	void	MR_trace_event_internal_report(
			const MR_Stack_Layout_Label *layout,
			MR_trace_port port, int seqno, int depth,
			const char *path);

extern	bool	MR_event_matches_spy_point(const MR_Stack_Layout_Label *layout);

#endif	/* MERCURY_TRACE_INTERNAL_H */
