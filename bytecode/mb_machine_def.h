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

	MB_Bytecode_Addr ip;		/* next instruction pointer */

	/*
	** The stack that the most ancestral bytecode function was using.
	**
	** At entry, each procedure checks to see if initial_stack is
	** NULL. If it is, then it replaces it with the current stack
	** pointer (MB_sp or MB_maxfr depending on code model)
	**
	** At procedure exit (MB_BC_endof_proc), the procedure checks the
	** stack to see whether this invocation was the one that set initfr.
	** If it was the one that set initfr, it knows it should return to
	**  native code
	** If it wasn't, then it must have been called by a bytecode proc
	**  and returns directly to a bytecode address
	**
	*/
	MB_Word		*initial_stack;

	/*
	** The native code address to return to
	** When a procedure wants to call/return to native code, it sets
	** ip to MB_CODE_NATIVE_RETURN and native_return to the actual
	** native code address to return to
	*/
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

#endif /* MB_MACHINE_DEF_H */
