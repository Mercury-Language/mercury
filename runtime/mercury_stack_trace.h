/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef MERCURY_STACK_TRACE_H
#define MERCURY_STACK_TRACE_H

#include "mercury_stack_layout.h"

/*
** mercury_stack_trace.h -
**	Definitions for use by the stack tracing.
*/

/*---------------------------------------------------------------------------*/

/*
** MR_dump_stack:
** 	Given the succip, det stack pointer and current frame, generate a 
** 	stack dump showing the name of each active procedure on the
** 	stack.
** 	NOTE: MR_dump_stack will assume that the succip is for the
** 	topmost stack frame.  If you call MR_dump_stack from some
** 	pragma c_code, that may not be the case.
** 	Due to some optimizations (or lack thereof) the MR_dump_stack call 
** 	may end up inside code that has a stack frame allocated, but
** 	that has a succip for the previous stack frame.
** 	Don't call MR_dump_stack from Mercury pragma c_code (calling
** 	from other C code in the runtime is probably ok, provided the
** 	succip corresponds to the topmost stack frame).
** 	(See library/require.m for a technique for calling MR_dump_stack
** 	from Mercury).
** 	If you need a more convenient way of calling from Mercury code,
** 	it would probably be best to make an impure predicate defined
** 	using `:- external'.
*/

extern	void		MR_dump_stack(Code *success_pointer,
				Word *det_stack_pointer,
				Word *current_frame);

/*
** MR_dump_stack_from_layout:
**	This function does the same job and makes the same assumptions
**	as MR_dump_stack, but instead of the succip, it takes the entry
**	layout of the current procedure as input. It also takes a parameter
**	that tells it where to put the stack dump. If the entire stack
**	was printed successfully, the return value is NULL; otherwise,
**	it is a string indicating why the dump was cut short.
*/

extern	const char	*MR_dump_stack_from_layout(FILE *fp,
				const MR_Stack_Layout_Entry *entry_layout,
				Word *det_stack_pointer, Word *current_frame);

/*
** MR_dump_nondet_stack_from_layout:
**	This function dumps the control control slots of the nondet stack.
**	The output format is not meant to be intelligible to non-implementors.
**	The value of maxfr should be in *base_maxfr.
*/

extern	void		MR_dump_nondet_stack_from_layout(FILE *fp,
				Word *base_maxfr);

/*
** MR_find_nth_ancestor:
**	Return the layout structure of the return label of the call
**	ancestor_level levels above the current call. Label_layout
**	tells us how to decipher the stack of the current call, while
**	*stack_trace_sp and *stack_trace_curfr tell us where it is.
**	On return, *stack_trace_sp and *stack_trace_curfr will be
**	set up to match the specified ancestor.
**
**	If the required stack walk is not possible (e.g. because some
**	stack frames have no layout information, or because the stack
**	does not have the required depth), the return value will be NULL,
**	and problem will point to an error message.
*/

extern	const MR_Stack_Layout_Label *MR_find_nth_ancestor(
				const MR_Stack_Layout_Label *label_layout,
				int ancestor_level, Word **stack_trace_sp,
				Word **stack_trace_curfr, const char **problem);

/*
** MR_stack_trace_bottom should be set to the address of global_success,
** the label main/2 goes to on success. Stack dumps terminate when they
** reach a stack frame whose saved succip slot contains this address.
*/

Code	*MR_stack_trace_bottom;

/*
** MR_nondet_stack_trace_bottom should be set to the address of the buffer
** nondet stack frame created before calling main. Nondet stack dumps terminate
** when they reach a stack frame whose redoip contains this address. Note that
** the redoip and redofr slots of this frame may be hijacked.
*/

Word	*MR_nondet_stack_trace_bottom;

#endif /* MERCURY_STACK_TRACE_H */
