/*
** Copyright (C) 1998-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	MERCURY_TRACE_EXTERNAL_H
#define	MERCURY_TRACE_EXTERNAL_H

#ifdef	MR_USE_EXTERNAL_DEBUGGER

extern	void	MR_trace_init_external(void);
extern	void	MR_trace_final_external(void);
extern	Code   *MR_trace_event_external(MR_Trace_Cmd_Info *cmd,
			const MR_Stack_Layout_Label *layout,
			Word *saved_regs, MR_Trace_Port port, Unsigned seqno,
			Unsigned depth, const char *path, int *max_mr_num);


#endif	/* MR_USE_EXTERNAL_DEBUGGER */

#endif	/* MERCURY_TRACE_EXTERNAL_H */
