/*
** Copyright (C) 1997-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trace_base.h defines the interface between the main part
** of the runtime system (mainly mercury_wrapper.c) and the part of the
** tracing subsystem that has to be present even if no module in the program
** is compiled with execution tracing.
*/

#ifndef MERCURY_TRACE_BASE_H
#define MERCURY_TRACE_BASE_H

#include <stdio.h>

/*
** This enum should EXACTLY match the definition of the `trace_port_type' type
** in library/debugger_interface.
*/

typedef	enum {
	MR_PORT_CALL,
	MR_PORT_EXIT,
	MR_PORT_FAIL,
	MR_PORT_THEN,
	MR_PORT_ELSE,
	MR_PORT_DISJ,
	MR_PORT_SWITCH,
	MR_PORT_PRAGMA_FIRST,
	MR_PORT_PRAGMA_LATER
} MR_trace_port;

/*
** MR_trace is called from Mercury modules compiled with tracing.
** It performs an indirect call through MR_trace_func_ptr, which
** will point either to MR_trace_real, which is defined in the trace
** library, or to MR_trace_fake, defined here, which just prints an
** error message and aborts.
*/

extern	void	MR_trace(const MR_Stack_Layout_Label *, MR_trace_port,
			Word, Word, const char *, int, bool);

extern	void	MR_trace_fake(const MR_Stack_Layout_Label *, MR_trace_port,
			Word, Word, const char *, int);

/*
** MR_trace_init() is called from mercury_runtime_init()
** when the debuggee programs begins, to perform any initialization
** that must be done before any traced Mercury code is executed.
** This includes the initialization code written in Mercury as well as main.
**
** MR_trace_start(enabled) is called from mercury_runtime_init()
** after the initialization code written in Mercury is executed,
** when we are about to start executing main. The argument says
** whether tracing is enabled for main (it is never enabled for
** initialization and finalization routines).
**
** MR_trace_end() is called from mercury_runtime_terminate() just
** after main has terminated and just before we call the finalization
** code written in Mercury.
**
** MR_trace_final() is called from mercury_runtime_terminate()
** after all Mercury code, including finalization code, has terminated.
*/

extern	void	MR_trace_init(void);
extern	void	MR_trace_start(bool enabled);
extern	void	MR_trace_end(void);
extern	void	MR_trace_final(void);

#define	MR_trace_incr_seq()	(++MR_trace_call_seqno)
#define	MR_trace_incr_depth()	(++MR_trace_call_depth)
#define	MR_trace_reset_depth(d)	do { MR_trace_call_depth = (d); } while (0)

/*
** The globals that define the interface between the tracing subsystem
** and compiled code, and which must be initialized in the permanent part
** of the runtime.
*/

extern	Word		MR_trace_call_seqno;
extern	Word		MR_trace_call_depth;

typedef enum {
	MR_TRACE_INTERNAL,
	MR_TRACE_EXTERNAL
} MR_trace_type;

extern	MR_trace_type	MR_trace_handler;
extern	bool		MR_trace_enabled;

extern	Unsigned	MR_trace_event_number;
extern	Bool		MR_trace_from_full;

/*
** These functions will report the number of the last event,
** if there have been some events, and will do nothing otherwise.
*/

extern	void	MR_trace_report(FILE *fp);
extern	void	MR_trace_report_raw(int fd);

#endif /* MERCURY_TRACE_BASE_H */
