/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trace_util.h defines macros for dealing with registers.
**
** These macros assume, and enforce, the conventions described
** in trace/README.
*/

#ifndef MERCURY_TRACE_UTIL_H
#define MERCURY_TRACE_UTIL_H

/*
** When using the heap pointer, we need to restore it, in case it is
** transient.
*/
#define MR_TRACE_USE_HP(STATEMENTS) do {				\
		restore_transient_registers();				\
		STATEMENTS;						\
		save_transient_registers();				\
	} while (0)

/*
** When calling Mercury code defined using `pragma export', we need
** to call save_registers() and restore_registers() around it.
** That in turn needs to be preceded/followed by
** restore/save_transient_registers() if it is in a C function.
*/
#define MR_TRACE_CALL_MERCURY(STATEMENTS) do {				\
		restore_transient_registers();				\
		save_registers();					\
		STATEMENTS;						\
		restore_registers();					\
		save_transient_registers();				\
	} while (0)

#endif /* MERCURY_TRACE_UTIL_H */
