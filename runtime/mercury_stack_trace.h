/*
** Copyright (C) 1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef MERCURY_STACK_TRACE_H
#define MERCURY_STACK_TRACE_H

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
** 	pragma c_code that may not be the case.
** 	Due to some optimizations (or lack thereof) the MR_dump_stack call 
** 	may end up inside code that has a stack frame allocated, but
** 	that has a succip for the previous stack frame.
** 	Don't call MR_dump_stack from Mercury pragma c_code (calling
** 	from other C code in the runtime is probably ok, provided the
** 	succip corresponds to the topmost stack frame).
** 	(See library/require.m for a technique for calling MR_dump_stack
** 	from Mercury).
** 	If you need a more conveinent way of calling from Mercury code,
** 	it would probably be best to make an impure predicate defined
** 	using `:- external'.
*/

extern void MR_dump_stack(Code *success_pointer, Word *det_stack_pointer,
		Word *current_frame);

#endif /* MERCURY_STACK_TRACE_H */
