/*
** Copyright (C) 1997-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trace_base.c implements the interface between the main part
** of the runtime system (mainly mercury_wrapper.c) and the part of the
** tracing subsystem that has to be present even if no module in the program
** is compiled with execution tracing.
**
** Main author: Zoltan Somogyi.
*/

#include "mercury_imp.h"
#include "mercury_trace.h"
#include "mercury_trace_util.h"
#include "mercury_trace_internal.h"
#include "mercury_trace_external.h"
#include "mercury_engine.h"
#include "mercury_wrapper.h"
#include "mercury_misc.h"
#include <stdio.h>
#include <unistd.h>		/* for the write system call */

/*
** Do we want to use the debugger within this process, or do want to use
** the Opium-style trace analyzer debugger implemented by an external process.
** This variable is set in mercury_wrapper.c and never modified afterwards.
*/

MR_trace_type	MR_trace_handler = MR_TRACE_INTERNAL;

/*
** Compiler generated tracing code will check whether MR_trace_enabled is true,
** before calling MR_trace. For now, and until we implement interface tracing,
** MR_trace_enabled should keep the same value throughout the execution of
** the entire program after being set in mercury_wrapper.c. There is one
** exception to this: the Mercury routines called as part of the functionality
** of the tracer itself (e.g. the term browser) should always be executed
** with MR_trace_enabled set to FALSE.
*/

bool		MR_trace_enabled = FALSE;

/*
** MR_trace_call_seqno counts distinct calls. The prologue of every
** procedure assigns the current value of this counter as the sequence number
** of that invocation and increments the counter. This is the only way that
** MR_trace_call_seqno is modified.
**
** MR_trace_call_depth records the current depth of the call tree. The prologue
** of every procedure assigns the current value of this variable plus one
** as the depth of that invocation. Just before making a call, the caller
** will set MR_trace_call_depth to its own remembered depth value. 
** These are the only ways in which MR_trace_call_depth is modified.
**
** Although neither MR_trace_call_seqno nor MR_trace_call_depth are used
** directly in this module, the seqno and depth arguments of MR_trace
** always derive their values from the saved values of these two global
** variables.
*/

Unsigned	MR_trace_call_seqno = 0;
Unsigned	MR_trace_call_depth = 0;

/*
** MR_trace_event_number is a simple counter of events. This is used in
** two places: here, for display to the user and for skipping a given number
** of events, and when printing an abort message, so that the programmer
** can zero in on the source of the problem more quickly.
*/

Unsigned	MR_trace_event_number = 0;

/*
** MR_trace_from_full is a boolean that is set before every call;
** it states whether the caller is being fully traced, or only interface
** traced. If the called code is interface traced, it will generate
** call, exit and fail trace events only if MR_trace_from_full is true.
** (It will never generate internal events.) If the called code is fully
** traced, it will always generate all trace events, external and internal,
** regardless of the setting of this variable on entry.
**
** The initial value is set to TRUE to allow the programmer to gain
** control in the debugger when main/2 is called.
*/

Bool		MR_trace_from_full = 1;

void
MR_trace_init(void)
{
#ifdef MR_USE_EXTERNAL_DEBUGGER
	if (MR_trace_handler == MR_TRACE_EXTERNAL)
		MR_trace_init_external(); /* should be in this module */
#endif
}

void
MR_trace_final(void)
{
#ifdef MR_USE_EXTERNAL_DEBUGGER
	if (MR_trace_handler == MR_TRACE_EXTERNAL)
		MR_trace_final_external(); /* should be in this module */
#endif
}

void
MR_trace_start(bool enabled)
{
	MR_trace_event_number = 0;
	MR_trace_call_seqno = 0;
	MR_trace_call_depth = 0;
	MR_trace_from_full = TRUE;
	MR_trace_enabled = enabled;
}

void
MR_trace_end(void)
{
	MR_trace_enabled = FALSE;
}

void
MR_trace_report(FILE *fp)
{
	if (MR_trace_event_number > 0) {
		/*
		** This means that the executable was compiled with tracing,
		** which implies that the user wants trace info on abort.
		*/

		fprintf(fp, "Last trace event was event #%ld.\n",
			(long) MR_trace_event_number);
	}
}

void
MR_trace_report_raw(int fd)
{
	char	buf[80];	/* that ought to be more than long enough */

	if (MR_trace_event_number > 0) {
		/*
		** This means that the executable was compiled with tracing,
		** which implies that the user wants trace info on abort.
		*/

		sprintf(buf, "Last trace event was event #%ld.\n",
			(long) MR_trace_event_number);
		write(fd, buf, strlen(buf));
	}
}
