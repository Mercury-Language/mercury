/*
** Copyright (C) 1998,2000-2002 The University of Melbourne.
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

#include "mercury_std.h"		/* for bool        */
#include "mercury_types.h"		/* for MR_Word etc */
#include "mercury_library_types.h"	/* for MercuryFile */
#include <stdio.h>			/* for FILE        */

/*
** When using the heap pointer, we need to restore it, in case it is
** transient.
*/
#define MR_TRACE_USE_HP(STATEMENTS) do {				\
		MR_restore_transient_registers();			\
		STATEMENTS;						\
		MR_save_transient_registers();				\
	} while (0)

/*
** When calling Mercury code defined using `pragma export', we need
** to call save_registers() and restore_registers() around it.
** That in turn needs to be preceded/followed by
** restore/save_transient_registers() if it is in a C function.
*/

#define MR_TRACE_CALL_MERCURY(STATEMENTS) do {				\
		MR_restore_transient_registers();			\
		MR_save_registers();					\
		STATEMENTS;						\
		MR_restore_registers();					\
		MR_save_transient_registers();				\
	} while (0)

/*
** MR_c_file_to_mercury_file is used to convert MR_mdb_in and MR_mdb_out
** into Mercury streams suitable for use by the browser.
*/
extern	void	MR_c_file_to_mercury_file(FILE *c_file,
			MercuryFile *mercury_file);

/*
** MR_trace_is_number checks whether the given word contains a natural number,
** i.e. a sequence of digits. If yes, it puts the value of the number in
** *value and returns TRUE, otherwise it returns FALSE.
*/

extern	bool	MR_trace_is_number(const char *word, int *value);

/*
** These functions print the values of sets of Mercury abstract machine
** registers. Their main use is low level debugging, including debugging
** the debugger itself.
*/

extern	void	MR_print_stack_regs(FILE *fp, MR_Word *saved_regs);
extern	void	MR_print_heap_regs(FILE *fp, MR_Word *saved_regs);
extern	void	MR_print_tabling_regs(FILE *fp, MR_Word *saved_regs);
extern	void	MR_print_succip_reg(FILE *fp, MR_Word *saved_regs);
extern	void	MR_print_r_regs(FILE *fp, MR_Word *saved_regs);

#endif /* MERCURY_TRACE_UTIL_H */
