/*
** Copyright (C) 2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
*/

#ifndef MB_MACHINE_DEF_H
#define MB_MACHINE_DEF_H

#include "mb_stack.h"

struct MB_Machine_State_Struct {

	MB_Module	*module;
	MB_Bytecode_Addr ip;		/* next instruction pointer */

	/*
	** The stack that the most ancestral bytecode function was using.
	** If a procedure finds that its stack frame is equal to this then
	** it knows it should return to native code rather than bytecode
	*/
	MB_Word		*initial_stack;

	/* The native code address to return to at finish */
	MB_Native_Addr	native_return;

	/* The following proc information is all set by MB_proc_type_check() */
	struct {
		/*
		** The determinism of the currently executing procedure
		** (set to a return value from MB_code_get_det)
		*/
		MB_Word is_det;

		/*
		** Pointer to vars for current procedure
		** Points to variable 0 on either the det or nondet stack
		** (depending on current procedure).
		*/
		MB_Word *var;
	} cur_proc;
};

#include "mercury_std.h"

#if 0

/* When you redo this, try offsetof() instead */

#if (MR_VARIABLE_SIZED > 0)
# define MB_CLOSURE_SIZE(x)	(sizeof(MB_Closure) \
				- sizeof(((MB_Closure *)(NULL))-> \
					closure_hidden_args \
				+ sizeof(MB_Word)*(x))
#else
# define MB_CLOSURE_SIZE(x)	(sizeof(MB_Closure) \
				+ sizeof(MB_Word)*(x))
#endif
typedef struct {
	MB_Word		code_addr;
	MB_Word		num_hidden_args;
	MB_Word		closure_hidden_args[MR_VARIABLE_SIZED];
} MB_Closure;
#endif

#endif /* MB_MACHINE_DEF_H */
