/*
** Copyright (C) 1997-2004 The University of Melbourne.
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
#include "mercury_tabling.h"	/* for MR_TableNode */
#include "mercury_goto.h"	/* for MR_declare_entry */

/*
** This enum should EXACTLY match the definition of the `trace_port_type'
** type in browser/util.m, the definition of the predicate
** `layout_out__trace_port_to_string' and the function
** `stack_layout__port_number', and the port names list
** in the C source file of this module (mercury_trace_base.c),
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
	MR_PORT_PRAGMA_LATER,
	MR_PORT_NONE
} MR_Trace_Port;

#define	MR_PORT_NUM_PORTS		((int) MR_PORT_NONE + 1)

extern	const char 			*MR_port_names[];

#define MR_trace_incr_seq()		((MR_Word) ++MR_trace_call_seqno)
#define MR_trace_incr_depth()		((MR_Word) ++MR_trace_call_depth)
#define MR_trace_reset_depth(d)		(MR_trace_call_depth = \
						(MR_Unsigned) (d))

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

extern	MR_Code	*MR_trace(const MR_Label_Layout *);
extern	MR_Code	*MR_trace_fake(const MR_Label_Layout *);

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
extern	void	MR_trace_start(MR_bool enabled);
extern	void	MR_trace_end(void);
extern	void	MR_trace_final(void);

/*
** Kill any windows created by mdb.
*/
extern	void	(*MR_trace_shutdown)(void);

/*
** The globals that define the interface between the tracing subsystem
** and compiled code, and which must be initialized in the permanent part
** of the runtime.
**
** XXX They should probably be in MercuryEngine.
*/

/*
** Compiler generated tracing code will check whether MR_trace_enabled is true,
** before calling MR_trace.
**
** MR_trace_enabled should keep the same value throughout the execution of
** the entire program after being set in mercury_wrapper.c, with two
** exceptions. First, the Mercury routines called as part of the functionality
** of the tracer itself (e.g. the term browser) should always be executed
** with MR_trace_enabled set to MR_FALSE. Second, when a procedure has
** the tabled_for_io_unitize annotation, which means that it can both do I/O
** and call Mercury code, then we turn the procedure and its descendants
** into a single unit by turning off tracing within the descendants.
** This is required to prevent the I/O tabling problems that could otherwise
** arise if we got retries from within the descendants.
*/

extern	MR_bool		MR_trace_enabled;

/*
** MR_trace_ever_enabled will keep the same value throughout the execution of
** the entire program after being set in mercury_wrapper.c to the same value
** as MR_trace_enabled. Unlike MR_trace_enabled, it is never reset, so one can
** use its value to test whether tracing was ever enabled.
*/

extern	MR_bool		MR_trace_ever_enabled;

/*
** MR_trace_call_seqno counts distinct calls. The prologue of every
** procedure assigns the current value of this counter as the sequence number
** of that invocation and increments the counter. This and retry are the only
** ways that MR_trace_call_seqno is modified.
**
** MR_trace_call_depth records the current depth of the call tree. The prologue
** of every procedure assigns the current value of this variable plus one
** as the depth of that invocation. Just before making a call, the caller
** will set MR_trace_call_depth to its own remembered depth value. 
** These and retry are the only ways in which MR_trace_call_depth is modified.
**
** Although neither MR_trace_call_seqno nor MR_trace_call_depth are used
** directly in this module, the seqno and depth arguments of MR_trace
** always derive their values from the saved values of these two global
** variables.
*/

extern	MR_Unsigned	MR_trace_call_seqno;
extern	MR_Unsigned	MR_trace_call_depth;

/*
** MR_trace_event_number is a simple counter of events. This is used in
** two places: in the debugger for display to the user and for skipping
** a given number of events, and when printing an abort message, so that
** the programmer can zero in on the source of the problem more quickly.
*/

extern	MR_Unsigned	MR_trace_event_number;

/*
** MR_trace_from_full is a boolean that is set before every call;
** it states whether the caller is being deep traced, or only shallow
** traced. If the called code is shallow traced, it will generate
** interface trace events only if MR_trace_from_full is true.
** (It will never generate internal events.) If the called code is deep
** traced, it will always generate all trace events, external and internal,
** regardless of the setting of this variable on entry.
**
** The initial value is set to MR_TRUE to allow the programmer to gain
** control in the debugger when main/2 is called.
*/

extern	MR_bool		MR_trace_from_full;

/*
** If set to true, MR_standardize_event_details modifies how functions that
** print event numbers and call sequence numbers operate, making them
** standardize these numbers. The Nth event number to be printed will be
** printed as E<N> and the Nth call sequence number will be printed as C<N>
** regardless of their actual values. This is intended to avoid hardcoding
** concrete event and call numbers in the expected outputs of the debugger
** test cases.
**
** The functions MR_standardize_event_num and MR_standardize_call_num implement
** the standardization itself.
*/

extern	MR_bool		MR_standardize_event_details;

extern	MR_Unsigned	MR_standardize_event_num(MR_Unsigned event_num);
extern	MR_Unsigned	MR_standardize_call_num(MR_Unsigned call_num);

/*
** Do we want to use the debugger within this process, or do want to use
** the Opium-style trace analyzer debugger implemented by an external process.
** This variable is set in mercury_wrapper.c and never modified afterwards.
*/

typedef enum {
	MR_TRACE_INTERNAL,
	MR_TRACE_EXTERNAL
} MR_Trace_Type;

extern	MR_Trace_Type	MR_trace_handler;

/*
** MR_trace_unhide_events is a boolean. Normally, it is set to false, which
** means that events that the compiler designates as hidden are really hidden
** from the procedural debugger, being visible only when building the annotated
** trace. When an mdb command intended for implementors only sets it to true,
** hidden events will be visible to the procedural debugger too, i.e. the
** hidden annotation on events will cease to be effective.
**
** The MR_trace_have_unhid_events is a boolean that is set to true whenever
** MR_trace_unhide_events is set to true, and it is never reset to false.
** MR_trace_have_unhid_events will therefore be true if the user has ever
** unhidden events. The declarative debugger checks this flag and refuses
** to perform if it is set, because if this flag has ever been set, then the
** numbering of events may not be the same after a retry, which makes it
** impossible to *reliably* find the event at which the "dd" command was issued
** while building the annotated trace.
*/

extern	MR_bool		MR_trace_unhide_events;
extern	MR_bool		MR_trace_have_unhid_events;

/*
** The details of I/O tabling are documented in library/table_builtin.m.
*/

typedef enum {
	/* from program start to first debugger event */
	MR_IO_TABLING_UNINIT,	

	/* from first debugger event to "table_io start" command */
	MR_IO_TABLING_BEFORE,

	/* from "table_io start" command to "table_io end" command */
	MR_IO_TABLING_DURING,

	/* from "table_io end" command to program exit */
	MR_IO_TABLING_AFTER
} MR_IoTablingPhase;

typedef	MR_Unsigned	MR_IoActionNum;

#define	MR_IO_ACTION_MAX	((MR_IoActionNum) -1)

extern	MR_IoTablingPhase	MR_io_tabling_phase;

/* True iff I/O tabling is enabled. */
extern	MR_bool		MR_io_tabling_enabled;

/* The root of the trie that we use for tabling I/O. */
extern	MR_TableNode	MR_io_tabling_pointer;

/* The I/O action number of the last I/O action. */
extern	MR_IoActionNum	MR_io_tabling_counter;

/* The highest I/O action number ever reached ("hwm" = "high water mark"). */
extern	MR_IoActionNum	MR_io_tabling_counter_hwm;

/* The highest I/O action number which is too early to be tabled. */
extern	MR_IoActionNum	MR_io_tabling_start;

/* The highest I/O action number which is to be tabled. */
extern	MR_IoActionNum	MR_io_tabling_end;

/* The event number at which I/O tabling was started; zero before start. */
extern	MR_Unsigned	MR_io_tabling_start_event_num;

/* The event number at which I/O tabling was stopped; zero before stop. */
extern	MR_Unsigned	MR_io_tabling_stop_event_num;

/* The flag that controls whether we should generate diagnostics. */
extern	MR_bool		MR_io_tabling_debug;

/* The flag that controls whether I/O tabling is allowed at all. */
extern	MR_bool		MR_io_tabling_allowed;

/*
** These functions will report the number of the last event,
** if there have been some events, and will do nothing otherwise.
*/

extern	void	MR_trace_report(FILE *fp);
extern	void	MR_trace_report_raw(int fd);

/*
** If MR_trace_report_msg is not NULL, it will be included in messages
** from MR_trace_report.
*/
extern	char	*MR_trace_report_msg;

/*
** This function prints an error message and aborts.  It should be
** called in situations where tracing is required, but `--trace' was
** not passed to c2init.
*/

extern	void	MR_tracing_not_enabled(void);

/*
** Return the details of I/O action <action_number> in three pieces:
** the name of the I/O action procedure in *proc_name_ptr, a boolean that is
** true iff procedure is a function in *is_func_ptr, and a Mercury
** representation of the argument list (minus the IO state arguments)
** in *arg_list_ptr.
** This function uses the heap pointer, so calls to it must be wrapped
** with MR_save_transient_hp() and MR_restore_transient_hp().
**
** This function is called from the Mercury code in the debugger, in the
** browser directory. It is here, not in the trace directory, because code
** in the browser directory cannot call functions in the trace directory.
*/

extern	const char
		*MR_trace_get_action(int action_number,
			MR_ConstString *proc_name_ptr, MR_Word *is_func_ptr,
			MR_Word *arg_list_ptr);

/*
** These functions allow library/exceptions.m to tell the debuggers
** which exception has been thrown.
*/

extern	void	MR_trace_set_exception_value(MR_Word exception);
extern	MR_Word	MR_trace_get_exception_value(void);

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

#ifndef	MR_HIGHLEVEL_CODE

MR_declare_entry(MR_do_trace_redo_fail_shallow);
MR_declare_entry(MR_do_trace_redo_fail_deep);

#endif	/* !MR_HIGHLEVEL_CODE */

#endif /* MERCURY_TRACE_BASE_H */
