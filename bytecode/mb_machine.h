/*
** Copyright (C) 1997,2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** Abstract mercury machine
**
*/

#ifndef MB_MACHINE_H
#define MB_MACHINE_H

#include <stdio.h>

#include "mb_bytecode.h"
/*#include "mb_module.h"*/
#include "mb_util.h"
#include "mb_stack.h"

struct MB_Machine_State_Struct;
typedef struct MB_Machine_State_Struct MB_Machine_State;

/*
** Returns an instruction describing a pointer to the next instruction
** Executes some 'special' IPs (eg: redo, fail) & returns their resultant ip
*/
void		MB_ip_set(MB_Machine_State *ms, MB_Bytecode_Addr new_ip);
MB_Bytecode_Addr MB_ip_get(MB_Machine_State *ms);
void		MB_native_return_set(MB_Machine_State *ms, MB_Native_Addr return_addr);
MB_Native_Addr	MB_native_return_get(MB_Machine_State *ms);

/*
** Check which procedure we are in & set variable stack pointer appropriately
*/
void		MB_proc_var_init(MB_Machine_State *ms);

/* Get/set a variable on the det stack */
MB_Word		MB_var_get(MB_Machine_State *ms, MB_Word idx);
void		MB_var_set(MB_Machine_State *ms, MB_Word idx, MB_Word value);

/* Get/set the initial stack frame (see machine_def.h for use) */
MB_Word		*MB_initialstackframe_get(MB_Machine_State *ms);
void		MB_initialstackframe_set(MB_Machine_State *ms, MB_Word *stack);

/* Get/set an entry on the nondet stack, relative to curfr */
/* index zero is the topmost element */
MB_Word		MB_frame_max_get(MB_Machine_State *ms, MB_Word idx);
void		MB_frame_max_set(MB_Machine_State *ms, MB_Word idx,MB_Word val);

/* Get an entry on the nondet stack */
MB_Word		MB_frame_get(MB_Machine_State *ms, MB_Word idx);
void		MB_frame_set(MB_Machine_State *ms, MB_Word idx, MB_Word val);

/* Add nondet stack frame */
MB_Word		MB_frame_temp_det_push(MB_Machine_State *ms, MB_Word redoip);
MB_Word		MB_frame_temp_push(MB_Machine_State *ms, MB_Word redoip);
MB_Word		MB_frame_push(MB_Machine_State *ms, MB_Word redoip,
			MB_Word succip, MB_Word vars, MB_Word temps);

/* Get/set a variable in the current stack frame variable list */
MB_Word		MB_frame_var_get(MB_Machine_State *ms, MB_Word idx);
void		MB_frame_var_set(MB_Machine_State *ms, MB_Word idx,MB_Word val);

/* Display the current state of the machine */
void		MB_show_state(MB_Machine_State *ms, FILE *fp);

/* Display the call stack of the machine */
void		MB_show_call(MB_Machine_State *ms, FILE *fp);

/* Single step execute */
void		MB_step(MB_Machine_State *ms);

/* Single step execute over predicates */
void		MB_step_over(MB_Machine_State *ms);

/* Run until exception */
void		MB_run(MB_Machine_State *ms);

/*
** Create a bytecode interpreter machine with an initial bytecode ip of new_ip
*/
void		MB_machine_create(MB_Machine_State *ms, MB_Bytecode_Addr new_ip,
				MB_Word *initial_stack);

/*
** Execute a bytecode machine until native code invocation required.
** Return address of native code to return to
*/
MB_Native_Addr	MB_machine_exec(MB_Machine_State *ms);

#endif	/* MB_MACHINE_H */

