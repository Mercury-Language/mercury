/*
** Copyright (C) 1998-2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	MERCURY_TRACE_EXTERNAL_H
#define	MERCURY_TRACE_EXTERNAL_H

#include "mercury_conf.h"		/* for MR_USE_EXTERNAL_DEBUGGER */
#include "mercury_types.h"		/* for Code */
#include "mercury_library_types.h"	/* for MercuryFile */

#include "mercury_trace.h"		/* for MR_Trace_Cmd_Info, etc. */

#ifdef	MR_USE_EXTERNAL_DEBUGGER

extern	void	MR_trace_init_external(void);
extern	void	MR_trace_final_external(void);
extern	Code   *MR_trace_event_external(MR_Trace_Cmd_Info *cmd,
			MR_Event_Info *event_info);

/*
** External debugger socket streams.
*/

extern MercuryFile MR_debugger_socket_in;
extern MercuryFile MR_debugger_socket_out;

#endif	/* MR_USE_EXTERNAL_DEBUGGER */

#endif	/* MERCURY_TRACE_EXTERNAL_H */
