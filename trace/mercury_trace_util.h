/*
** Copyright (C) 1998,2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module provides utility functions for the debugger.
**
** This header files defines macros for dealing with registers.
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

/*
** MR_trace_is_number checks whether the given word contains a natural number,
** i.e. a sequence of digits. If yes, it puts the value of the number in
** *value and returns TRUE, otherwise it returns FALSE.
*/

extern	bool	MR_trace_is_number(const char *word, int *value);

#endif /* MERCURY_TRACE_UTIL_H */
