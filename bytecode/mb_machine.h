/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: mb_machine.h,v 1.1 2001-01-24 07:42:25 lpcam Exp $
**
** Abstract mercury machine
**
*/


#ifndef MB_MACHINE_H
#define	MB_MACHINE_H

#include <stdio.h>

#include "mb_bytecode.h"
#include "mb_util.h"
#include "mb_stack.h"

#define MB_MACHINEREGS	21

/* Don't directly access data from a machine state; go through the
** wrappers below which provide some measure of error checking
** (C++, C++, oh where art thou C++?)
*/
typedef struct MB_Machine_State_Tag {
	MB_Word ip;			/* next instruction pointer*/
	
	/* det stack */
	struct {
		MB_Word succip;		/* sucess return address */
		MB_Stack stack;		/* stack data */
	} det;

	/* nondet stack */
	struct {
		MB_Word	curfr;		/* stack frame of current procedure */
		MB_Word	maxfr;		/* highest frame on nondet stack */
		MB_Stack stack;		/* stack data */
	} nondet;

	/* heap */
	struct {
		/* XXX */
	} heap;

	/* MB_Bytecode is 44 bytes long - this is obviously very inefficient
	** for most instructions.
	**
	** All code accesses should go through MB_get_code so the following
	** is abstracted away:
	**
	** code_id is an array of bytecode types
	** code_index is the index into code_data for the bytecode arguments
	** code_data is the bytecode arguments
	**
	** This way each instruction takes 5 bytes + argument size
	** rather than 1 byte + size of largest possible argument
	*/
	struct {
		MB_Word	 count;		/* number of instructions */
		MB_Byte* id;		/* instruction types */
		MB_Stack data_index;	/* index into code data for aguments */
		MB_Stack data;		/* argument data stack */
	} code;

	#define CODE_DATA_NONE	0	/* If a bytecode's data_index is this
					** then we will assume it has no data
					*/

	MB_Word reg[MB_MACHINEREGS];	/* machine regs */

	
	/* For the simulation only (not part of the abstract machine) */

	/* Call stack: each stack frame consists of:
	** stack[sp-1]: index into code.data[] containing info on current proc
	** stack[sp-2]: index into code.data[] containing info on previous proc
	** etc.
	*/
	struct {
		MB_Stack	stack;
	} call;

	/* high-water marked */
	struct {
		MB_Stack stack;
	} label;
	
} MB_Machine_State;

#define MB_CODE_INVALID_ADR	((MB_Word)-1)

typedef struct MB_Stack_Frame_Tag {
	MB_Word prevfr;
	MB_Word succfr;
	MB_Word redoip;
	MB_Word succip;
} MB_Stack_Frame;

/* Get the value of a register */
MB_Word		MB_reg_get(MB_Machine_State* ms, MB_Word idx);

/* Set the value of a register */
void		MB_reg_set(MB_Machine_State* ms, MB_Word idx, MB_Word value);

/* Get/set the next instruction pointer */
MB_Word		MB_ip_get(MB_Machine_State* ms);
void		MB_ip_set(MB_Machine_State* ms, MB_Word);

/* Get/set the success instruction pointer */
MB_Word		MB_succip_get(MB_Machine_State* ms);
void		MB_succip_set(MB_Machine_State* ms, MB_Word);

/* Read the bytecode at a given address */
MB_Bytecode	MB_code_get(MB_Machine_State* ms, MB_Word adr);

/* Get the bytecode type at a given address */
MB_Byte		MB_code_get_id(MB_Machine_State* ms, MB_Word adr);

/* Get the bytecode argument at a given address */
MB_Bytecode_Arg*MB_code_get_arg(MB_Machine_State* ms, MB_Word adr);

/* Get the predicate in which the following address resides */
MB_Bytecode	MB_code_get_pred(MB_Machine_State* ms, MB_Word adr);
MB_Word		MB_code_get_pred_adr(MB_Machine_State* ms, MB_Word adr);

/* Get the procedure in which the following address resides */
MB_Bytecode	MB_code_get_proc(MB_Machine_State* ms, MB_Word adr);

/* Return how many bytecodes there are */
MB_Word		MB_code_size(MB_Machine_State* ms);

/* Get/set a variable on the det stack */
MB_Word		MB_var_get(MB_Machine_State* ms, MB_Word idx);
void		MB_var_set(MB_Machine_State* ms, MB_Word idx, MB_Word value);


/* The positions of frame variables*/
#define MB_FRAME_PREVFR	0
#define MB_FRAME_REDOIP	1
#define MB_FRAME_REDOFR	2
#define MB_FRAME_SUCCIP	3
#define MB_FRAME_SUCCFR	4

#define MB_FRAME_SIZE		5
#define MB_FRAME_TEMP_SIZE	3

/* Get/set an entry on the nondet stack, relative to curfr */
/* index zero is the topmost element */
MB_Word		MB_frame_get(MB_Machine_State* ms, MB_Word idx);
void		MB_frame_set(MB_Machine_State* ms, MB_Word idx, MB_Word val);

/* get/set a value inside a temporary frame */
MB_Word		MB_frame_temp_get(MB_Machine_State* ms,
			MB_Word frame_num, MB_Word idx);
void		MB_frame_temp_set(MB_Machine_State* ms,
			MB_Word frame_num, MB_Word idx, MB_Word val);

/* Get/set a variable in the current stack frame variable list */
MB_Word		MB_frame_var_get(MB_Machine_State* ms, MB_Word idx);
void		MB_frame_var_set(MB_Machine_State* ms, MB_Word idx, MB_Word val);

/* add/remove a number of temporary stack frames to the nondet stack */
void		MB_frame_temp_add(MB_Machine_State* ms, MB_Word count);
void		MB_frame_temp_remove(MB_Machine_State* ms, MB_Word count);

/* add/remove an ordinary nondet stack frame */
void		MB_frame_add(MB_Machine_State* ms, MB_Word var_count);
void		MB_frame_remove(MB_Machine_State* ms, MB_Word var_count);

/* Load a program from a file */
/* Returns false for failure */
MB_Machine_State*MB_load_program(FILE* fp);
MB_Machine_State*MB_load_program_name(MB_CString filename);
MB_Bool		MB_reset_program(MB_Machine_State* ms);
void		MB_unload_program(MB_Machine_State* ms);

/* Display the current state of the machine */
void		MB_show_state(MB_Machine_State* ms, FILE* fp);

/* Display the call stack of the machine */
void		MB_show_call(MB_Machine_State* ms, FILE* fp);

/* Single step execute */
void		MB_step(MB_Machine_State* ms);

/* Single step execute over predicates */
void		MB_step_over(MB_Machine_State* ms);

/* Run until exception */
void		MB_run(MB_Machine_State* ms);

#endif	/* MB_MACHINE_H */


