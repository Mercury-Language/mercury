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
#include "mb_util.h"

struct MB_Machine_State_Struct;
typedef struct MB_Machine_State_Struct MB_Machine_State;

/*
** Set/get the next execution point
*/
void		MB_ip_set(MB_Machine_State *ms, MB_Bytecode_Addr new_ip);
MB_Bytecode_Addr MB_ip_get(MB_Machine_State *ms);
void		MB_native_return_set(MB_Machine_State *ms,
				MB_Native_Addr return_addr);
MB_Native_Addr	MB_native_return_get(MB_Machine_State *ms);

/*
** Check which procedure we are in & set variable stack pointer appropriately
** (Required before MB_var_get and MB_var_set are used)
*/
void		MB_proc_var_init(MB_Machine_State *ms);

/* Is the currently execting proc det/semidet */
MB_Bool		MB_proc_is_det(MB_Machine_State* ms);

/* Get/set a variable in the current proc variable list */
MB_Word		MB_var_get(MB_Machine_State *ms, MB_Word idx);
void		MB_var_set(MB_Machine_State *ms, MB_Word idx, MB_Word value);

/* Get/set the initial stack frame (see machine_def.h for use) */
MB_Word		*MB_initialstackframe_get(MB_Machine_State *ms);
void		MB_initialstackframe_set(MB_Machine_State *ms, MB_Word *stack);

/*
** Add a temporary nondet stack frame
** Will push a temp nondet or temp det frame automatically
** (assuming proc_var_init was called)
*/
void		MB_frame_temp_push_do_fail(MB_Machine_State *ms);
void		MB_frame_temp_push(MB_Machine_State *ms,
					MB_Bytecode_Addr MB_redoip);

#endif	/* MB_MACHINE_H */

