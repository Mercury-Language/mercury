/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	MERCURY_TRACE_EXTERNAL_H
#define	MERCURY_TRACE_EXTERNAL_H

#ifdef	MR_USE_EXTERNAL_DEBUGGER

extern	void	MR_trace_init_external(void);
extern	void	MR_trace_final_external(void);
extern	void	MR_trace_event_external(const MR_Stack_Layout_Label *layout,
			MR_trace_port port, Unsigned seqno, Unsigned depth,
			const char *path);

#endif	/* MR_USE_EXTERNAL_DEBUGGER */

#endif	/* MERCURY_TRACE_EXTERNAL_H */
