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
** 	Given the succip and det stack pointer, generate a stack dump
** 	showing then name of each procedure on the stack.
** 	XXX 
** 	Currently only deterministic stack frames are handled, if a
** 	nondeterministic stack frame is found while tracing down the
** 	stack, the stack dump ends.
*/

extern void MR_dump_stack(Code *success_pointer, Word *det_stack_pointer);

#endif /* MERCURY_STACK_TRACE_H */
