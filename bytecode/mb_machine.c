
/*
** Copyright (C) 2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: mb_machine.c,v 1.1 2001-01-24 07:42:25 lpcam Exp $
*/

/* XXX: make this variable */
#define MAX_CODE_SIZE		10000
#define INIT_CODE_SIZE		10000
#define INIT_CODE_DATA		100000
#define INIT_DET_SIZE		10000
#define INIT_NONDET_SIZE	10000
#define INIT_CALLSTACK_SIZE	500
#define INIT_LABELSTACK_SIZE	1000

/* Imports */
#include	<assert.h>
#include	<stdio.h>
#include	<string.h>

#include	"mb_bytecode.h"
#include	"mb_disasm.h"
#include	"mb_machine.h"
#include	"mb_mem.h"
#include	"mb_stack.h"

/* Exported definitions */


/* Get the value of a register */
MB_Word		MB_reg_get(MB_Machine_State* ms, MB_Word idx);
void		MB_reg_set(MB_Machine_State* ms, MB_Word idx, MB_Word value);
MB_Word		MB_ip_get(MB_Machine_State* ms);
void		MB_ip_set(MB_Machine_State* ms, MB_Word new_ip);
MB_Word		MB_succip_get(MB_Machine_State* ms);
void		MB_succip_set(MB_Machine_State* ms, MB_Word);
MB_Bytecode	MB_code_get(MB_Machine_State* ms, MB_Word adr);
MB_Byte		MB_code_get_id(MB_Machine_State* ms, MB_Word adr);
MB_Bytecode_Arg*MB_code_get_arg(MB_Machine_State* ms, MB_Word adr);
MB_Bytecode	MB_code_get_pred(MB_Machine_State* ms, MB_Word adr);
MB_Word		MB_code_get_pred_adr(MB_Machine_State* ms, MB_Word adr);
MB_Bytecode	MB_code_get_proc(MB_Machine_State* ms, MB_Word adr);
MB_Word		MB_code_size(MB_Machine_State* ms);
MB_Word		MB_var_get(MB_Machine_State* ms, MB_Word idx);
void		MB_var_set(MB_Machine_State* ms, MB_Word idx, MB_Word value);
MB_Word		MB_frame_get(MB_Machine_State* ms, MB_Word idx);
void		MB_frame_set(MB_Machine_State* ms, MB_Word idx, MB_Word val);
MB_Word		MB_frame_temp_get(MB_Machine_State* ms,
			MB_Word frame_num, MB_Word idx);
void		MB_frame_temp_set(MB_Machine_State* ms,
			MB_Word frame_num, MB_Word idx, MB_Word val);
MB_Word		MB_frame_var_get(MB_Machine_State* ms, MB_Word idx);
void		MB_frame_var_set(MB_Machine_State* ms, MB_Word idx, MB_Word val);
void		MB_frame_temp_add(MB_Machine_State* ms, MB_Word count);
void		MB_frame_temp_remove(MB_Machine_State* ms, MB_Word count);
void		MB_frame_add(MB_Machine_State* ms, MB_Word var_count);
void		MB_frame_remove(MB_Machine_State* ms, MB_Word var_count);

MB_Machine_State* MB_load_program(FILE* fp);
MB_Machine_State* MB_load_program_name(MB_CString filename);
MB_Bool		MB_reset_program(MB_Machine_State* ms);
void		MB_unload_program(MB_Machine_State* ms);

void		MB_step(MB_Machine_State* ms);
void		MB_step_over(MB_Machine_State* ms);
void		MB_run(MB_Machine_State* ms);

#define FILEVERSION	9

/* Local declarations */

static char
rcs_id[]	= "$Id: mb_machine.c,v 1.1 2001-01-24 07:42:25 lpcam Exp $";

static MB_Word	find_entry_point(MB_Machine_State* ms);
static MB_Bool	translate_calls(MB_Machine_State* ms);

/* Implementation */

/* Finds the main/2 entry point */
/* returns (MB_Word)-1 if it can't find it */
static MB_Word
find_entry_point(MB_Machine_State* ms)
{
	/* First find the main procedure */
	MB_Word code_size = MB_code_size(ms);
	MB_Bytecode bc;
	MB_Integer i;
	MB_Byte bcid;
	for (i = 0; i < code_size; i++) {
		/* Search for the main predicate */
		bcid = MB_code_get_id(ms, i);
		if (bcid == MB_BC_enter_pred) {
			bc = MB_code_get(ms, i);
			if (bc.opt.enter_pred.pred_arity == 2 &&
				!bc.opt.enter_pred.is_func &&
				MB_strcmp(bc.opt.enter_pred.pred_name, "main") == 0)
			{
				/* XXX: is proc 0 always the correct entry point? */
				/* Find proc 0 */
				for (i++; i < code_size; i++) {
					bc = MB_code_get(ms, i);
					if (bc.id == MB_BC_endof_pred) break;
					if (bc.id == MB_BC_enter_proc &&
						((bc.opt.enter_proc.det == MB_DET_DET) ||
						(bc.opt.enter_proc.det == MB_DET_CC_MULTIDET)) &&
							bc.opt.enter_proc.proc_id == 0) {
	
						MB_stack_push(&ms->call.stack, i);
						return i;
					}
				
				}
			}
		}
	}

	MB_util_error("Unable to find main/2 entry point");
	return (MB_Word)-1;
} /* find_entry_point */

/* Get the value of a register */
MB_Word
MB_reg_get(MB_Machine_State* ms, MB_Word idx)
{
	assert(idx >= 0);
	assert(idx < MB_MACHINEREGS);
	return ms->reg[idx];
}

/* Set the value of a register */
void
MB_reg_set(MB_Machine_State* ms, MB_Word idx, MB_Word value)
{
	assert(idx >= 0);
	assert(idx < MB_MACHINEREGS);
	ms->reg[idx] = value;
}

/* Get the next instruction pointer */
MB_Word
MB_ip_get(MB_Machine_State* ms)
{
	return ms->ip;
}


void
MB_ip_set(MB_Machine_State* ms, MB_Word new_ip)
{
	assert(new_ip >= 0);
	assert(new_ip < ms->code.count);
	ms->ip = new_ip;
}

 /* Get the success instruction pointer */
MB_Word
MB_succip_get(MB_Machine_State* ms)
{
	return ms->det.succip;
}


void
MB_succip_set(MB_Machine_State* ms, MB_Word new_ip)
{
	ms->det.succip = new_ip;
}  

/* Get the actual size of a program, in bytecodes */
MB_Word
MB_code_size(MB_Machine_State* ms)
{
	return ms->code.count;
}

#define ARGSIZE(name)	(sizeof(((MB_Bytecode*)NULL)->opt.##name) + \
				sizeof(MB_Word)-1) \
				/ sizeof(MB_Word)
/* the size of the arguments in a MB_Bytecode struct, in number of MB_Words*/
static const MB_Word argument_size[] = {
	ARGSIZE(enter_pred),
	ARGSIZE(endof_pred),
	ARGSIZE(enter_proc),
	ARGSIZE(endof_proc),
	ARGSIZE(label),
	ARGSIZE(enter_disjunction),
	ARGSIZE(endof_disjunction),
	ARGSIZE(enter_disjunct),
	ARGSIZE(endof_disjunct),
	ARGSIZE(enter_switch),
	ARGSIZE(endof_switch),
	ARGSIZE(enter_switch_arm),
	ARGSIZE(endof_switch_arm),
	ARGSIZE(enter_if),
	ARGSIZE(enter_then),
	ARGSIZE(endof_then),
	ARGSIZE(endof_if),
	ARGSIZE(enter_negation),
	ARGSIZE(endof_negation),
	ARGSIZE(enter_commit),
	ARGSIZE(endof_commit),
	ARGSIZE(assign),
	ARGSIZE(test),
	ARGSIZE(construct),
	ARGSIZE(deconstruct),
	ARGSIZE(complex_construct),
	ARGSIZE(complex_deconstruct),
	ARGSIZE(place_arg),
	ARGSIZE(pickup_arg),
	ARGSIZE(call),
	ARGSIZE(higher_order_call),
	ARGSIZE(builtin_binop),
	ARGSIZE(builtin_unop),
	ARGSIZE(builtin_bintest),
	ARGSIZE(builtin_untest),
	ARGSIZE(semidet_succeed),
	ARGSIZE(semidet_success_check),
	ARGSIZE(fail),
	ARGSIZE(context),
	ARGSIZE(not_supported)
};

/* Get the bytecode at a given address; performs a range check */
MB_Bytecode
MB_code_get(MB_Machine_State* ms, MB_Word adr)
{
	MB_Bytecode bc;

	assert(adr >= 0 && adr < ms->code.count);

	bc.id = MB_code_get_id(ms, adr);
	
	assert(bc.id < sizeof(argument_size)/sizeof(argument_size[0]));

	if (argument_size[bc.id] > 0) {
	
		memcpy(&(bc.opt),
			MB_code_get_arg(ms, adr),
			argument_size[bc.id]*sizeof(MB_Word));
	}
	return bc;
}

/* Get the bytecode type at a given address */
MB_Byte
MB_code_get_id(MB_Machine_State* ms, MB_Word adr)
{
	if (adr < 0 || adr >= ms->code.count)
		return MB_BC_debug_invalid;
	return ms->code.id[adr];
}

/* Get the bytecode argument at a given address */
MB_Bytecode_Arg*
MB_code_get_arg(MB_Machine_State* ms, MB_Word adr)
{
	MB_Word data_index;

	if (adr < 0 || adr >= ms->code.count) return NULL;

	data_index = MB_stack_peek(&ms->code.data_index, adr);
	if (data_index == 0) {
		return NULL;
	} else {
		return (void*)MB_stack_peek_p(&ms->code.data, data_index);
	}
}

/* Get the predicate owning the code at adr */
MB_Bytecode
MB_code_get_pred(MB_Machine_State* ms, MB_Word adr)
{
	MB_Word pred_adr = MB_code_get_pred_adr(ms, adr);
	if (pred_adr == MB_CODE_INVALID_ADR) {
		MB_Bytecode bc;
		bc.id = MB_BC_enter_pred;
		bc.opt.enter_pred.pred_name = MB_NULL_STR;
		bc.opt.enter_pred.pred_arity = 0;
		bc.opt.enter_pred.is_func = 0;
		bc.opt.enter_pred.proc_count = 0;
		return bc;
	}
	

	return MB_code_get(ms, pred_adr);
}

MB_Word
MB_code_get_pred_adr(MB_Machine_State* ms, MB_Word adr) {

	while (MB_code_get_id(ms, adr) != MB_BC_enter_pred) {

		adr--;
		if (adr < 0 || adr >= ms->code.count) {
			return MB_CODE_INVALID_ADR;
		}
	}

	return adr;
}

/* Get the procedure owning the code at adr */
MB_Bytecode
MB_code_get_proc(MB_Machine_State* ms, MB_Word adr)
{
	MB_Byte bc_id;
	adr++;
	do {
		adr--;
		assert(adr >= 0 && adr < ms->code.count);
		bc_id = MB_code_get_id(ms, adr);
		assert(bc_id != MB_BC_enter_pred);
		assert(bc_id != MB_BC_endof_pred);
	}
	while (bc_id != MB_BC_enter_proc);

	return MB_code_get(ms, adr);
}

/* Translates calls from a predicate name / procedure to an actual code address */
static MB_Bool
translate_calls(MB_Machine_State* ms)
{
	/* first run through and save all the predicate names table
	** XXX: should use a hash table for this: mercury_hash_table
	** has one but it doesn't use the same memory allocation as
	** in mb_mem.h - Is this a problem?
	*/
	MB_Stack pred_stack;
	MB_Word i;
	pred_stack = MB_stack_new(100);	/* guess 100 preds (grows as needed) */
	for (i = 0; i < MB_code_size(ms); i++) {
		if (MB_code_get_id(ms, i) == MB_BC_enter_pred) {
			MB_stack_push(&pred_stack, i);
		}
	}

	/* XXX: should also temporarily table the procs, instead of re-searching
	** each time, but since there is usually only one proc per predicate, don't
	** bother for now
	*/

	for (i = 0; i < MB_code_size(ms); i++) {

		/* If we have found a call, find its destination address */
		if (MB_code_get_id(ms, i) == MB_BC_call) {

			MB_Bytecode_Arg* call_arg = MB_code_get_arg(ms, i);
			MB_Byte		bc_id;
			MB_Word		adr;
			MB_Word		j = MB_CODE_INVALID_ADR;
			
			adr = MB_CODE_INVALID_ADR;

			/* Search for the right procedure*/
			for (j = 0; j < MB_stack_size(&pred_stack); j++) {
				
				MB_Bytecode_Arg* pred_arg;
				adr = MB_stack_peek(&pred_stack, j);

				pred_arg = MB_code_get_arg(ms, adr);

				

				/* XXX: we can't distinguish between predicates
				** and functions in the same module, of the same
				** arity! (bug in bytecode generator)
				*/

				/* XXX: We ignore the module*/
				if ((pred_arg->enter_pred.pred_arity == call_arg->call.arity) &&
					MB_strcmp(pred_arg->enter_pred.pred_name,
						call_arg->call.pred_id) == 0)
				{
					break;
				}
			}

			if (j == MB_stack_size(&pred_stack)) {
				MB_util_error("Call from %08x to unknown predicate %s/%d",
						(int)i, call_arg->call.pred_id,
						(int)call_arg->call.arity);
				call_arg->call.adr = MB_CODE_INVALID_ADR;
				continue;
			}

			/* Now find the right proc */
			do {
				adr++;

				assert(adr < MB_code_size(ms) && adr >= 0);

				bc_id = MB_code_get_id(ms, adr);
				if (bc_id == MB_BC_enter_proc) {
					if (MB_code_get_arg(ms, adr)->enter_proc.proc_id ==
						call_arg->call.proc_id)
					{
						call_arg->call.adr = adr;
						break;
					}
				} else if ((bc_id == MB_BC_endof_pred) ||
						(bc_id == MB_BC_enter_pred))
				{
					MB_util_error(
						"Call from %08x to unknown predicate"
							"procedure %s/%d (%d)",
						i, call_arg->call.pred_id,
						(int)call_arg->call.arity,
						(int)call_arg->call.proc_id);
					/* XXX: This should eventually be fatal */
					MB_fatal("Error generating call addresses\n");
					break;
				}
			} while (1);

			if (adr >= MB_code_size(ms)) {
				MB_stack_delete(&pred_stack);
				return FALSE;
			}
		}
	}

	MB_stack_delete(&pred_stack);

	return TRUE;
} /* translate_calls */

/* Create a new machine given a pointer to a file containing the bytecodes */
MB_Machine_State*
MB_load_program(FILE* fp)
{

	int			indent_level = 0;
	MB_Short		version;
	MB_Bytecode		bc;
	MB_Bytecode_Arg*	cur_proc_arg = NULL;
	MB_Word			cur_adr = 0;

	MB_Machine_State*	ms = MB_new(MB_Machine_State);
	if (ms == NULL) return NULL;
	
	ms->code.data	= MB_stack_new(INIT_CODE_DATA);
	ms->det.stack	= MB_stack_new(INIT_DET_SIZE);
	ms->nondet.stack= MB_stack_new(INIT_NONDET_SIZE);
	ms->call.stack	= MB_stack_new(INIT_CALLSTACK_SIZE);
	ms->label.stack	= MB_stack_new(INIT_LABELSTACK_SIZE);
	ms->code.data_index= MB_stack_new(INIT_CODE_SIZE);
	/* XXX don't use fixed limits */
	ms->code.id	= MB_new_array(MB_Byte, MAX_CODE_SIZE);

	MB_stack_push(&ms->code.data, 0);	/* reserve 0 for indicating no data */

	if (!ms->code.id) {
		MB_unload_program(ms);
		return NULL;
	}

	/* Check the file version is ok */
	if (!MB_read_bytecode_version_number(fp, &version)) {
		MB_util_error("Unable to read version number\n");
		return FALSE;
	}
	if (version != FILEVERSION) {
		MB_util_error("Unknown file format version\n");
		return FALSE;
	}

	/* read in each bytecode */
	while (MB_read_bytecode(fp, &bc)) {
		if (cur_adr+1 >= MAX_CODE_SIZE) {
			MB_util_error("Not enough code space."
					" Increase MAX_CODE_SIZE.\n");
			MB_unload_program(ms);
			return NULL;
		}

		ms->code.id[cur_adr] = bc.id;


		if (bc.id == MB_BC_label) {
			/* XXX: we don't actually need to save the labels
			** in the code (but it makes printing the
			** debugging output easier)
			*/
			if (cur_proc_arg == NULL) {
				MB_fatal("Label encountered outside of a proc\n");
			}

			/* Add the label to the current proc's list of labels */
			MB_stack_push(&ms->label.stack, cur_adr);
		}
		/* copy the bytecode arguments into the code.data
		** structure, save the index & increment code.data
		** counters
		*/
		if (bc.id < sizeof(argument_size)/sizeof(argument_size[0]))
		{
			if (argument_size[bc.id] == 0) {
				/* If bytecode has no arguments, skip allocation */
				MB_stack_push(&ms->code.data_index, CODE_DATA_NONE);
			} else {
				/* Allocate the space for the bytecode's arguments */
				MB_Word cur_arg_index =
					MB_stack_alloc(
						&ms->code.data,
						argument_size[bc.id]);

				/* If we just read a procedure */
				if (bc.id == MB_BC_enter_proc) {
					/* Save the new current proc (so
					** labels know where they are)
					*/
					cur_proc_arg =
						(MB_Bytecode_Arg*)MB_stack_peek_p(
							&ms->code.data,
							cur_arg_index);

					/* and mark where the label indexes will begin */
					cur_proc_arg->enter_proc.label_index =
						MB_stack_alloc(&ms->label.stack, 0);
				}
				
				MB_stack_push(&ms->code.data_index, cur_arg_index);

				/* Copy the arguments into the argument data stack */
				memcpy(MB_stack_peek_p(&ms->code.data,
						cur_arg_index),
					&(bc.opt),
					argument_size[bc.id]*
						sizeof(MB_Word));
			}
		} else {
			MB_util_error("Unknown op code");
			MB_unload_program(ms);
			return NULL;
		}
		cur_adr++;
	}
	ms->code.count = cur_adr;
	ms->nondet.curfr = MB_stack_alloc(&ms->nondet.stack, MB_FRAME_SIZE);
	
	if (feof(fp) &&
		(ms->code.count > 0) &&
		(translate_calls(ms)) &&
		(MB_reset_program(ms)))
	{
		return ms;
	}
	
	MB_unload_program(ms);

	return NULL;
} /* MB_load_program */

/* add/remove an ordinary nondet stack frame */
void
MB_frame_add(MB_Machine_State* ms, MB_Word var_count)
{
	/* XXX */
	MB_fatal("frame stuff not done");
}

void
MB_frame_remove(MB_Machine_State* ms, MB_Word var_count)
{
	/* XXX */
	MB_fatal("frame stuff not done");
}

/* Load a program given a file name */
MB_Machine_State*
MB_load_program_name(MB_CString filename)
{
	FILE* fp = fopen(filename, "rb");
	if (fp != NULL) {
		return MB_load_program(fp);
	}
	return FALSE;
}

/* reset a program back to an unrun state*/
MB_Bool
MB_reset_program(MB_Machine_State* ms)
{
	ms->call.stack.sp = 0;

	MB_succip_set(ms, MB_CODE_INVALID_ADR);

	MB_ip_set(ms, find_entry_point(ms));
	
	if (MB_ip_get(ms) == (MB_Word)-1) {
		return FALSE;
	}

	return TRUE;
}

/* free all memory associated with a machine state */
void
MB_unload_program(MB_Machine_State* ms)
{
	if (ms != NULL) {
		/* the stacks will always be allocated since it will
		** have aborted if their allocation failed
		*/
		MB_stack_delete(&ms->label.stack);
		MB_stack_delete(&ms->call.stack);
		MB_stack_delete(&ms->nondet.stack);
		MB_stack_delete(&ms->det.stack);
		MB_stack_delete(&ms->code.data);
		MB_stack_delete(&ms->code.data_index);
		if (ms->code.id) MB_free(ms->code.id);
		MB_free(ms);
	}
}

/* Get a variable on the det stack */
MB_Word
MB_var_get(MB_Machine_State* ms, MB_Word idx)
{
	assert(idx >= 0);
	assert(idx <
		MB_code_get_arg(ms, MB_stack_peek_rel(&ms->call.stack, 1))
			->enter_proc.list_length);
	return MB_stack_peek_rel(&ms->det.stack, idx+1);
}

/* Set a variable on the det stack */
void
MB_var_set(MB_Machine_State* ms, MB_Word idx, MB_Word value)
{
	assert(idx >= 0);
	assert(idx <
		MB_code_get_arg(ms, MB_stack_peek_rel(&ms->call.stack, 1))
			->enter_proc.list_length);
	MB_stack_poke_rel(&ms->det.stack, idx+1, value);
}

/* Get/set an entry on the nondet stack, relative to the current frame */
MB_Word
MB_frame_get(MB_Machine_State* ms, MB_Word idx)
{
	return MB_stack_peek(&ms->nondet.stack,
			ms->nondet.curfr + idx);
}

void
MB_frame_set(MB_Machine_State* ms, MB_Word idx, MB_Word val)
{
	MB_stack_poke(&ms->nondet.stack, ms->nondet.curfr + idx, val);
}

MB_Word
MB_frame_temp_get(MB_Machine_State* ms, MB_Word frame_num, MB_Word idx)
{
	return MB_frame_get(ms,
			MB_FRAME_SIZE + frame_num*MB_FRAME_TEMP_SIZE + idx);
}

void
MB_frame_temp_set(MB_Machine_State* ms,
		MB_Word frame_num, MB_Word idx, MB_Word val)
{
	MB_frame_set(
		ms,
		MB_FRAME_SIZE + frame_num*MB_FRAME_TEMP_SIZE + idx,
		val);

}

/* Get/set a variable in the current stack frame variable list */
MB_Word
MB_frame_var_get(MB_Machine_State* ms, MB_Word idx)
{
	return MB_stack_peek(&ms->nondet.stack, MB_FRAME_SIZE + idx);

}

void
MB_frame_var_set(MB_Machine_State* ms, MB_Word idx, MB_Word val)
{
	MB_stack_poke(&ms->nondet.stack, MB_FRAME_SIZE + idx, val);
}

/* add/remove a number of temporary stack frames to the nondet stack */
void
MB_frame_temp_add(MB_Machine_State* ms, MB_Word count)
{
	MB_stack_alloc(&ms->nondet.stack, count * MB_FRAME_TEMP_SIZE);
}

void
MB_frame_temp_remove(MB_Machine_State* ms, MB_Word count)
{
	MB_stack_free(&ms->nondet.stack, count * MB_FRAME_TEMP_SIZE);
}

/* -------------------------------------------------------------------------- */
static void instr_invalid	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static void instr_enter_proc	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static void instr_endof_proc	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static void instr_enter_if	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static void instr_endof_if	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static void instr_construct	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static void instr_place		(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static void instr_pickup	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static void instr_call		(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static void instr_builtin_binop	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);

static void instr_noop		(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static void instr_notdone	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);


/* XXX: relies on the order of the definitions */
static void (*instruction_table[])(MB_Machine_State*, const MB_Bytecode_Arg*) = {
	instr_invalid,	/*enter_pred*/
	instr_invalid,	/*endof_pred*/
	instr_enter_proc,
	instr_endof_proc,
	instr_noop,	/* label */
	instr_notdone,	/* disjunction */
	instr_notdone,
	instr_notdone,	/* disjunct */
	instr_notdone,
	instr_notdone,	/* switch */
	instr_notdone,
	instr_notdone,
	instr_notdone,
	instr_enter_if,
	instr_notdone,
	instr_notdone,
	instr_endof_if,
	instr_notdone,	/* neg */
	instr_notdone,
	instr_notdone,	/* commit */
	instr_notdone,
	instr_notdone,	/* assign */
	instr_notdone,	/* test */
	instr_construct,/* construct */
	instr_notdone,
	instr_notdone,
	instr_notdone,
	instr_place,	/* place */
	instr_pickup,	/* pickup */
	instr_call,	/* call */
	instr_notdone,
	instr_builtin_binop,
	instr_notdone,
	instr_notdone,
	instr_notdone,
	instr_notdone,	/* semidet */
	instr_notdone,
	instr_notdone,	/* fail */
	instr_noop,	/* context */
	instr_notdone	/* not supported */
};

static void
instr_invalid(MB_Machine_State* ms, const MB_Bytecode_Arg* bca)
{
	assert(FALSE);
}


/* Just something to set new stack vars to to help find bugs */
#define UNSETSTACK	0xbadbad00

/* Enter/exit procedure */
static void
instr_enter_proc(MB_Machine_State* ms, const MB_Bytecode_Arg* bca)
{
	/* save the current address (for debugging) */
	MB_stack_push(&ms->call.stack, MB_ip_get(ms));

	switch (bca->enter_proc.det) {
		case MB_DET_DET: {
			MB_Word i;

			/* Allocate nondet stack frames */
			MB_frame_temp_add(ms, bca->enter_proc.temp_count);

			/* save our succip */
			MB_stack_push(&ms->det.stack, MB_succip_get(ms));

			/* allocate new stack variables */
			for (i = 0; i < bca->enter_proc.list_length; i++) {
				MB_stack_push(&ms->det.stack, UNSETSTACK+i);
			}

			MB_ip_set(ms, MB_ip_get(ms)+1);
			break;
		}

		default:
			instr_notdone(ms, bca);
	}
}

static void
instr_endof_proc(MB_Machine_State* ms, const MB_Bytecode_Arg* endof_bca)
{
	/* get the current proc off the top of the call stack */
	MB_Bytecode_Arg* bca = MB_code_get_arg(ms,
					MB_stack_pop(&ms->call.stack));
	
	switch (bca->enter_proc.det) {
		case MB_DET_DET: {
			/* deallocate stack variables */	
			MB_stack_free(&ms->det.stack, bca->enter_proc.list_length);
			MB_succip_set(ms, MB_stack_pop(&ms->det.stack));

			/* dellocate nondet stack frames */
			MB_frame_temp_remove(ms, bca->enter_proc.temp_count);

			MB_ip_set(ms, MB_succip_get(ms));
			break;
		}

		default:
			instr_notdone(ms, bca);
	}
}

static void
instr_enter_if (MB_Machine_State* ms, const MB_Bytecode_Arg* bca)
{
//	MB_Word* temp_frame = ms-> bca
	instr_notdone(ms, bca);
}

static void
instr_endof_if (MB_Machine_State* ms, const MB_Bytecode_Arg* bca)
{
	instr_notdone(ms, bca);
}

static void
instr_construct(MB_Machine_State* ms, const MB_Bytecode_Arg* bca)
{
	assert(bca->construct.list_length == 0);
	switch (bca->construct.consid.id) {
		case MB_CONSID_INT_CONST:
			MB_var_set(ms, bca->construct.to_var,
					bca->construct.consid.opt.int_const);

			instr_noop(ms, bca);
			break;
		case MB_CONSID_STRING_CONST:
			MB_var_set(ms, bca->construct.to_var,
					(MB_Word)bca->construct.consid.opt.string_const);
			
			instr_noop(ms, bca);
			break;
		default:
			instr_notdone(ms, bca);
	}
}

static void
instr_place(MB_Machine_State* ms, const MB_Bytecode_Arg* bca)
{
	/* copy value from var slot to reg */
	MB_reg_set(ms, bca->place_arg.to_reg,
		MB_var_get(ms, bca->place_arg.from_var));

	/* XXX for debugging only */
	MB_var_set(ms, bca->place_arg.from_var, UNSETSTACK);

	/* go to the next instruction */
	instr_noop(ms, bca);
}

static void
instr_pickup(MB_Machine_State* ms, const MB_Bytecode_Arg* bca)
{
	/* copy value from reg to var slot*/
	MB_var_set(ms, bca->pickup_arg.to_var,
		MB_reg_get(ms, bca->pickup_arg.from_reg));

	/* XXX for debugging only */
	MB_reg_set(ms, bca->pickup_arg.from_reg, UNSETSTACK);

	/* go to the next instruction */
	instr_noop(ms, bca);
}

static void
instr_call(MB_Machine_State* ms, const MB_Bytecode_Arg* bca)
{
	MB_Word new_adr;
	new_adr = bca->call.adr;

	if (new_adr == MB_CODE_INVALID_ADR) {
		MB_util_error("Attempt to call unknown predicate %s/%d (%d)",
				bca->call.pred_id,
				(int)bca->call.arity,
				(int)bca->call.proc_id);
		instr_noop(ms, bca);
	} else {
		/* set the new execution point*/
		MB_succip_set(ms, MB_ip_get(ms)+1);

		/* set the return address to the next instruction */
		MB_ip_set(ms, new_adr);
	}
}

/* --------------------------------------------------------------------- */

static MB_Word binop_add	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static MB_Word binop_sub	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static MB_Word binop_mul	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static MB_Word binop_div	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static MB_Word binop_mod	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static MB_Word binop_lshift	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static MB_Word binop_rshift	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static MB_Word binop_and	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static MB_Word binop_or		(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static MB_Word binop_xor	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static MB_Word binop_logand	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static MB_Word binop_logor	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static MB_Word binop_eq		(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static MB_Word binop_ne		(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static MB_Word binop_lt		(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static MB_Word binop_gt		(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static MB_Word binop_le		(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static MB_Word binop_ge		(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);
static MB_Word binop_bad	(MB_Machine_State* ms, const MB_Bytecode_Arg* bca);

/*
**	XXX: Currently we depend on the order of elements in the table.
*/
static MB_Word (*binop_table[])(MB_Machine_State* ms, const MB_Bytecode_Arg* bca) = {
	binop_add,
	binop_sub,
	binop_mul,
	binop_div,
	binop_mod,
	binop_lshift,
	binop_rshift,	/* XXX signed */
	binop_and,
	binop_or,
	binop_xor,
	binop_logand,
	binop_logor,
	binop_eq,
	binop_ne,
	binop_bad,	/* array_index */
	binop_bad,	/* str_eq */
	binop_bad,	/* str_ne */
	binop_bad,	/* str_lt */
	binop_bad,	/* str_gt */
	binop_bad,	/* str_le */
	binop_bad,	/* str_ge */
	binop_lt,
	binop_gt,
	binop_le,
	binop_ge,
	binop_bad,	/* float_plus */
	binop_bad,	/* float_minus */
	binop_bad,	/* float_times */
	binop_bad,	/* float_divide */
	binop_bad,	/* float_eq */
	binop_bad,	/* float_ne */
	binop_bad,	/* float_lt */
	binop_bad,	/* float_gt */
	binop_bad,	/* float_le */
	binop_bad,	/* float_ge */
	binop_bad	/* body */
};

#define SIMPLEBINOP(name, op)	\
	static MB_Word \
	binop_##name(MB_Machine_State* ms, const MB_Bytecode_Arg* bca) \
	{ \
		assert(bca->builtin_binop.arg1.id == MB_ARG_VAR); \
		assert(bca->builtin_binop.arg2.id == MB_ARG_VAR); \
		return (MB_Integer)(MB_var_get(ms, bca->builtin_binop.arg1.opt.var)) \
			op (MB_Integer)(MB_var_get(ms, bca->builtin_binop.arg2.opt.var)); \
	}

SIMPLEBINOP(add,	+)
SIMPLEBINOP(sub,	-)
SIMPLEBINOP(mul,	*)
SIMPLEBINOP(div,	/)
SIMPLEBINOP(mod,	%)
SIMPLEBINOP(lshift,	<<)
SIMPLEBINOP(rshift,	>>)
SIMPLEBINOP(and,	&)
SIMPLEBINOP(or,		|)
SIMPLEBINOP(xor,	^)
SIMPLEBINOP(logand,	&&)
SIMPLEBINOP(logor,	||)
SIMPLEBINOP(eq,		==)
SIMPLEBINOP(ne,		!=)
SIMPLEBINOP(lt,		<)
SIMPLEBINOP(gt,		>)
SIMPLEBINOP(le,		<=)
SIMPLEBINOP(ge,		>=)


static MB_Word
binop_bad(MB_Machine_State* ms, const MB_Bytecode_Arg* bca)
{
	MB_fatal("Unsupported binop\n");
	return 0;
}

/* --------------------------------------------------------------------- */

static void
instr_builtin_binop(MB_Machine_State* ms, const MB_Bytecode_Arg* bca)
{
	MB_Byte binop = bca->builtin_binop.binop;
	if (binop < (sizeof(binop_table)/sizeof(binop_table[0]))) {
		MB_var_set(ms,
			bca->builtin_binop.to_var,
			binop_table[bca->builtin_binop.binop](ms, bca));
	} else {
		MB_fatal("Invlid binop");
	}

	/* move to the next instruction*/
	instr_noop(ms, bca);
}
static void
instr_noop(MB_Machine_State* ms, const MB_Bytecode_Arg* bca)
{
	/* increment instruction pointer */
	MB_ip_set(ms, MB_ip_get(ms)+1);
}

static void
instr_notdone(MB_Machine_State* ms, const MB_Bytecode_Arg* bca)
{
	/* invalid instruction */
	MB_fatal("That instruction is not implemened yet\n");
	instr_noop(ms, bca);
}


/* Single step execute */
void
MB_step(MB_Machine_State* ms)
{
	MB_Word ip = MB_ip_get(ms);

	MB_Byte bc_id = MB_code_get_id(ms, ip);
	if (bc_id >= sizeof(instruction_table) / sizeof(instruction_table[0])) {
		instr_noop(ms, NULL);
	} else {
		instruction_table[bc_id](ms, MB_code_get_arg(ms, ip));
	}
}

void
MB_step_over(MB_Machine_State* ms)
{
	MB_Word ip = MB_ip_get(ms);
	MB_Byte bc_id = MB_code_get_id(ms, ip);
	
	if (ip == MB_CODE_INVALID_ADR) {
		MB_util_error("Attempt to execute invalid code address\n");
	}

	switch (bc_id) {
		case MB_BC_call: {
			
			/* If we are about to step into a predicate */
			/* then replace the following bytecode with */
			/* a MB_BC_debug_trap and run until it traps */
			/* then put things back to what they were */
			MB_Byte old_id;
			assert(ip+1 < MB_code_size(ms));
			old_id = ms->code.id[ip+1];

			ms->code.id[ip+1] = MB_BC_debug_trap;
			MB_run(ms);

			ms->code.id[ip+1] = old_id;
			break;
		}
		default:
			MB_step(ms);
	}
}

/* Run until invalid instruction or debug_trap bytecode encountered */
void
MB_run(MB_Machine_State* ms)
{
	do {
		MB_Word ip = MB_ip_get(ms);

		MB_Byte bc_id = MB_code_get_id(ms, ip);
		if (bc_id >= sizeof(instruction_table) / sizeof(instruction_table[0])) {
			switch (bc_id) {
				case MB_BC_debug_trap:
					return;
			}
			MB_util_error("Attempt to execute invalid instruction\n");
			instr_noop(ms, NULL);
			return;
		} else {
			instruction_table[bc_id](ms, MB_code_get_arg(ms, ip));
		}
	} while (1);
}


