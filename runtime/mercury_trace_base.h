/*
** Copyright (C) 1997-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trace_base.h defines the interface between the main part
** of the runtime system (mainly mercury_wrapper.c) and the part of the
** tracing subsystem that has to be present even if tracing is not enabled.
** The part of the tracing system that required only when tracing is enabled
** is in the trace directory.
*/

#ifndef MERCURY_TRACE_BASE_H
#define MERCURY_TRACE_BASE_H

#include <stdio.h>
#include "mercury_stack_layout.h"
#include "mercury_std.h"

/*
** This enum should EXACTLY match the definition of the `trace_port_type'
** type in browser/debugger_interface, the definition of the predicates
** `llds_out__trace_port_to_{int,string}', and port names list in the C source
** file of this module.
*/

typedef	enum {
	MR_PORT_CALL,
	MR_PORT_EXIT,
	MR_PORT_REDO,
	MR_PORT_FAIL,
	MR_PORT_EXCEPTION,
	MR_PORT_COND,
	MR_PORT_THEN,
	MR_PORT_ELSE,
	MR_PORT_NEG_ENTER,
	MR_PORT_NEG_SUCCESS,	/* negated goal failed; negation succeeds */
	MR_PORT_NEG_FAILURE,	/* negated goal succeeded; negation fails */
	MR_PORT_DISJ,
	MR_PORT_SWITCH,
	MR_PORT_PRAGMA_FIRST,
	MR_PORT_PRAGMA_LATER
} MR_Trace_Port;

extern	const char 			*MR_port_names[];

#define MR_trace_incr_seq()		((Word) ++MR_trace_call_seqno)
#define MR_trace_incr_depth()		((Word) ++MR_trace_call_depth)
#define MR_trace_reset_depth(d)		(MR_trace_call_depth = (Unsigned) (d))

/*
** MR_trace is called from Mercury modules compiled with tracing.
** If the event is supposed to be traced, it performs an indirect call
** through MR_trace_func_ptr, which will point either to MR_trace_real,
** which is defined in the trace library, or to MR_trace_fake, defined here,
** which just prints an error message and aborts.
**
** The return value, if not NULL, says where execution should continue
** after the event. (NULL means it should continue as usual.)
*/

extern	Code	*MR_trace(const MR_Stack_Layout_Label *);
extern	Code	*MR_trace_fake(const MR_Stack_Layout_Label *);

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

/*
** The globals that define the interface between the tracing subsystem
** and compiled code, and which must be initialized in the permanent part
** of the runtime.
**
** XXX They should probably be in MercuryEngine.
*/

extern	Unsigned	MR_trace_call_seqno;
extern	Unsigned	MR_trace_call_depth;

typedef enum {
	MR_TRACE_INTERNAL,
	MR_TRACE_EXTERNAL
} MR_Trace_Type;

extern	MR_Trace_Type	MR_trace_handler;
extern	bool		MR_trace_enabled;

extern	Unsigned	MR_trace_event_number;
extern	Bool		MR_trace_from_full;

/*
** These functions will report the number of the last event,
** if there have been some events, and will do nothing otherwise.
*/

extern	void	MR_trace_report(FILE *fp);
extern	void	MR_trace_report_raw(int fd);

/*
** This function prints an error message and aborts.  It should be
** called in situations where tracing is required, but `--trace' was
** not passed to c2init.
*/

extern	void	MR_tracing_not_enabled(void);

/*
** If MR_TRACE_HISTOGRAM is defined, MR_trace maintains two arrays of integers,
** MR_trace_histogram_all and MR_trace_histogram_exp, in which the element
** with subscript d is incremented when a trace event occurs at depth d.
** The intention is that the MR_trace_histogram_all records all events
** and is never reset, which means that it records information about a whole
** execution of the program. MR_trace_histogram_exp on the other hand can be
** zeroed by a command from the debugger at e.g a call port, and examined at
** e.g. an exit port, which means that it can record information about the
** execution of a call.
**
** Both arrays are allocated via malloc, and resized on demand. They are
** always the same size, and this size is stored in MR_trace_histogram_max.
** MR_trace_histogram_hwm stores the high water mark, i.e. the biggest
** depth number that has been encountered so far in the execution of the
** program.
*/

#ifdef	MR_TRACE_HISTOGRAM

extern	int	*MR_trace_histogram_all;
extern	int	*MR_trace_histogram_exp;
extern	int	MR_trace_histogram_max;
extern	int	MR_trace_histogram_hwm;

extern	void	MR_trace_print_histogram(FILE *fp, const char *which,
			int *histogram, int max);

#endif	/* MR_TRACE_HISTOGRAM */

#endif /* MERCURY_TRACE_BASE_H */
