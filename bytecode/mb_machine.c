/*
** Copyright (C) 2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
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
#include	"mercury_imp.h"

#include	<assert.h>
#include	<stdio.h>
#include	<string.h>

#include	"mb_bytecode.h"
#include	"mb_disasm.h"
#include	"mb_interface.h"
#include	"mb_machine.h"
#include	"mb_machine_def.h"

#include	"mb_mem.h"
#include	"mb_stack.h"

/* Exported definitions */

/* Set new stack vars to this help find bugs */
#define CLOBBERED	0xbadbad00

#define CLOBBERPICKUPS	0	/* clobber reg after pickup */
#define CLOBBERPLACES	0	/* clobber slot after place */
#define CLOBBERSTACK	1	/* reset new stack vars */

#define FILEVERSION	9

/* Local declarations */

static MB_Bool	dispatch(MB_Byte bc_id, MB_Machine_State *ms);

static void instr_do_redo	(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static void instr_do_fail	(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);

/* Implementation */

/* Get the next instruction pointer */
MB_Bytecode_Addr
MB_ip_get(MB_Machine_State *ms)
{
	return ms->ip;
}

/* set the native code return address */
void
MB_native_return_set(MB_Machine_State *ms, MB_Native_Addr native_return)
{
	ms->native_return = native_return;
}

MB_Native_Addr
MB_native_return_get(MB_Machine_State * ms)
{
	return ms->native_return;
}

void
MB_ip_set(MB_Machine_State *ms, MB_Bytecode_Addr new_ip)
{
	if (MB_ip_special(new_ip)) {
		switch ((MB_Word) new_ip) {
			case (MB_Word) MB_CODE_DO_FAIL:
				instr_do_fail(ms, NULL);
				break;

			case (MB_Word) MB_CODE_DO_REDO:
				instr_do_redo(ms, NULL);
				break;
				
			case (MB_Word) MB_CODE_NATIVE_RETURN:
				ms->ip = new_ip;
				break;

			default:
				assert(FALSE);
		}
	} else {
		ms->ip = new_ip;
	}
}

/*
** Check which procedure we are in & set variable stack pointer appropriately
**
** If you don't call this after the ip switches to a new function
** then MB_var_get and MB_var_set will give incorrect results
**
** If det/semidet, set the machine state variable slot pointer to the det stack
** If nondet, set the machine state variable slot pointer to the nondet stack
*/
void
MB_proc_var_init(MB_Machine_State *ms)
{
	MB_Bytecode_Addr ip = MB_ip_get(ms);

	MB_SAY("Hello from proc_var_init");

	if (!MB_ip_normal(ip)) return;
	
	/* Check that we are actually in a procedure and not just entering one*/
	if (MB_code_get_id(ip) != MB_BC_enter_proc) {

		/* If we are, check the determinism & set vars as appropriate */
		ms->cur_proc.is_det = MB_code_get_det(ip);

		ms->cur_proc.var = (ms->cur_proc.is_det == MB_ISDET_YES)
			? &(MB_stackitem(MB_DETFRAME_SIZE+1))
			: &(MB_frameitem(MB_FRAME_SIZE));

	} else {
		MB_SAY(" not getting proc det of unentered procedure");
	}
}

/*
** Get a variable from the appropriate mercury stack
**
** It knows which stack to use because you have of course already
** called MB_proc_var_init to set the current procedure's
** variable pointer to point to variable 0.
*/
MB_Word
MB_var_get(MB_Machine_State *ms, MB_Word idx)
{
	/*
	** idx is negative because variable 0 is the topmost and
	** higher number variables are below it on the stack
	*/
	return ms->cur_proc.var[-idx];
}

/* Set a variable on the mercury stack */
void
MB_var_set(MB_Machine_State *ms, MB_Word idx, MB_Word value)
{
	ms->cur_proc.var[-idx] = value;
}

/* Get/set the initial stack frame (see machine_def.h for use) */
MB_Word *
MB_initialstackframe_get(MB_Machine_State *ms)
{
	return ms->initial_stack;
}

void
MB_initialstackframe_set(MB_Machine_State *ms, MB_Word *frame)
{
	MB_SAY("'spector gadget %08x\n", frame);
	assert(ms->initial_stack == NULL);
	ms->initial_stack = frame;
}

/* Add a temporary det stack frame */
MB_Word
MB_frame_temp_det_push(MB_Machine_State *ms, MB_Word redoip)
{
	MB_fatal("MB_frame_temp_det_push not implemented yet");
	return 0;
#if 0
	MB_Word maxfr = MB_maxfr_get(ms);
	MB_Word prevfr = maxfr;

	maxfr += MB_FRAME_TEMP_DET_SIZE;
	MB_maxfr_set(ms, maxfr);

	MB_frame_max_set(ms, MB_FRAME_PREVFR, prevfr);
	MB_frame_max_set(ms, MB_FRAME_REDOIP, redoip);
	MB_frame_max_set(ms, MB_FRAME_REDOFR, MB_curfr_get(ms));
	MB_frame_max_set(ms, MB_FRAME_DETFR, MB_stack_size(&ms->det.stack));

	return maxfr;
#endif
}

/* Add a temporary stack frame */
MB_Word
MB_frame_temp_push(MB_Machine_State *ms, MB_Word redoip)
{
	MB_fatal("MB_frame_temp_push not implemented yet");
	return 0;
#if 0
	if (ms->cur_proc.detism == MB_CUR_DET) {
		return MB_frame_temp_det_push(ms, redoip);	
	} else {
		MB_Word maxfr = MB_maxfr_get(ms);
		MB_Word prevfr = maxfr;

		maxfr += MB_FRAME_TEMP_SIZE;
		MB_maxfr_set(ms, maxfr);

		MB_frame_max_set(ms, MB_FRAME_PREVFR, prevfr);
		MB_frame_max_set(ms, MB_FRAME_REDOIP, redoip);
		MB_frame_max_set(ms, MB_FRAME_REDOFR, MB_curfr_get(ms));

		return maxfr;
	}
#endif
}

/* Add a stack frame */
MB_Word
MB_frame_push(MB_Machine_State *ms, MB_Word redoip,
		MB_Word succip, MB_Word vars, MB_Word temps)
{
	MB_fatal("MB_frame_temp_push not implemented yet");
	return 0;
#if 0
	MB_Word maxfr = MB_maxfr_get(ms);
	MB_Word prevfr = maxfr;
	MB_Word succfr = MB_curfr_get(ms);

	maxfr += MB_FRAME_SIZE + vars + temps;

	MB_maxfr_set(ms, maxfr);
	MB_curfr_set(ms, maxfr);

	MB_frame_cur_set(ms, MB_FRAME_NUMVARS, vars);
	MB_frame_cur_set(ms, MB_FRAME_REDOIP, redoip);
	MB_frame_cur_set(ms, MB_FRAME_PREVFR, prevfr);
	MB_frame_cur_set(ms, MB_FRAME_SUCCIP, succip);
	MB_frame_cur_set(ms, MB_FRAME_SUCCFR, succfr);
	MB_frame_cur_set(ms, MB_FRAME_REDOFR, MB_curfr_get(ms));

	return maxfr;
#endif
}

/* Get/set a variable in the current stack frame variable list */
void
MB_frame_var_set(MB_Machine_State *ms, MB_Word idx, MB_Word val)
{
	MB_fatal("MB_frame_var_set not implemented yet");
#if 0
	MB_stack_poke(&ms->nondet.stack,
		MB_curfr_get(ms) - MB_FRAME_SIZE - idx, val);
#endif
}
/* Get/set a variable in the current stack frame variable list */
MB_Word
MB_frame_var_get(MB_Machine_State *ms, MB_Word idx)
{
	MB_fatal("MB_frame_var_get not implemented yet");
	return 0;
#if 0
	return MB_stack_peek(&ms->nondet.stack,
		MB_curfr_get(ms) - MB_FRAME_SIZE - idx);
#endif
}
/* -------------------------------------------------------------------------- */
static void instr_invalid		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_enter_proc		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_endof_proc		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_enter_disjunction	(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_endof_disjunction	(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_enter_disjunct	(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_endof_disjunct	(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_enter_switch		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_enter_switch_arm	(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_endof_switch_arm	(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_endof_switch		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_enter_if		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_enter_then		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_endof_then		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
/* instr_enter_else is identical to enter_then */
static void instr_endof_if		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_enter_negation	(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_endof_negation_goal	(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_endof_negation	(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_enter_commit		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_endof_commit		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_assign		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_test			(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_construct		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_deconstruct		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_place			(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_pickup		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_call			(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_higher_order_call	(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_builtin_binop		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_builtin_bintest	(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_builtin_unop		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_builtin_untest	(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_semidet_success	(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_semidet_success_check	(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_do_redo		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_do_fail		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_noop			(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);
static void instr_notdone		(MB_Machine_State *ms,
						const MB_Bytecode_Arg *bca);

/* return true if a construction succeeds */
static MB_Word do_construct_cons(MB_Machine_State *ms, const MB_Cons_id *cid,
		MB_Word list_length, MB_Short *var_list);

/* return true if a deconstruction succeeds */
static MB_Bool do_deconstruct(MB_Machine_State *ms, const MB_Cons_id *cid,
		MB_Word var, MB_Word list_length, MB_Short *var_list);
static MB_Bool do_deconstruct_cons(MB_Machine_State *ms, const MB_Cons_id *cid,
		MB_Word val, MB_Word list_length, MB_Short *var_list);

/* XXX det and nondet conditions (ite / disjunct / commit) all use the same
** [would require modifying bytecode ids?]
*/

/*typedef void (MB_Instruction_Handler)(MB_Machine_State *,
						const MB_Bytecode_Arg *);
*/

typedef void (*MB_Instruction_Handler)	(MB_Machine_State *,
						const MB_Bytecode_Arg *);

/* XXX ORDER: relies on the order of the definitions */
static MB_Instruction_Handler instruction_table[] = {
	instr_invalid,		/* enter_pred */
	instr_invalid,		/* endof_pred */
	instr_enter_proc,
	instr_endof_proc,
	instr_noop,		/* label */
	instr_enter_disjunction,
	instr_endof_disjunction,
	instr_enter_disjunct,
	instr_endof_disjunct,
	instr_enter_switch,
	instr_endof_switch,
	instr_enter_switch_arm,
	instr_endof_switch_arm,
	instr_enter_if,
	instr_enter_then,
	instr_endof_then,
	instr_endof_if,
	instr_enter_negation,
	instr_endof_negation,
	instr_enter_commit,
	instr_endof_commit,
	instr_assign,
	instr_test,
	instr_construct,
	instr_deconstruct,
	instr_notdone,		/* XXX complex construct */
	instr_notdone,		/* XXX complex deconstruct */
	instr_place,
	instr_pickup,
	instr_call,
	instr_higher_order_call,
	instr_builtin_binop,
	instr_builtin_unop,	/* XXX unop */
	instr_builtin_bintest,
	instr_builtin_untest,	/* XXX unop test */
	instr_semidet_success,
	instr_semidet_success_check,
	instr_do_redo,		/* fail */
	instr_noop,		/* context */
	instr_notdone,		/* not supported */
	instr_enter_then,	/* enter_else (identical to enter_then) */
	instr_endof_negation_goal
};

static void
instr_invalid(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("Invalid instruction encountered");
}


/* Enter/exit procedure */
static void
instr_enter_proc(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	switch (bca->enter_proc.det) {
		case MB_DET_SEMIDET:
			MB_fatal("semidet");

		case MB_DET_DET: {
			MB_Word detframe_size = 
					bca->enter_proc.temp_count +
					bca->enter_proc.list_length +
					MB_DETFRAME_SIZE;

			/*
			** Save the initial stack frame if this function is
			** going to be the one that returns to native code
			*/
			MB_SAY( "MB_sp:             %08x\n"
				"initialstackframe: %08x\n",
				MB_sp,
				ms->initial_stack);

			if (MB_initialstackframe_get(ms) == NULL) {
				MB_initialstackframe_set(ms, MB_sp);
			}

			MB_incr_sp(detframe_size);

			/* save succip */
			MB_stackitem(MB_DETFRAME_SUCCIP) = (MB_Word) MB_succip;

			MB_ip_set(ms, MB_ip_get(ms) + 1);

			break;
		}
		case MB_DET_MULTIDET:
		case MB_DET_NONDET: {

			MB_fatal("enter_proc/multidet,nondet");

			#if 0
			MB_frame_push(ms,
				MB_CODE_DO_FAIL,
				MB_succip_get(ms),
				bca->enter_proc.list_length,
				bca->enter_proc.temp_count);

			MB_ip_set(ms, MB_ip_get(ms) + 1);
			#endif
			
			break;
		}
		/* XXX Other options */
		default:
			instr_notdone(ms, NULL);
	}

	/* set procedure detism info & variable stack pointer */
	MB_proc_var_init(ms);

	#if CLOBBERSTACK
	{
		MB_Word i;
		MB_Word count = bca->enter_proc.list_length +
					bca->enter_proc.temp_count;
		for (i = 0; i < count; i++) {
			MB_var_set(ms, i, CLOBBERED + i);
		}
	}
	#endif
#if 0
	if (MB_model_semi(bca->enter_proc.det)) {
		/*
		** If a semidet procedure then mark our success slot as failure
		** until we know otherwise.
		*/
		MB_temp_set(ms, MB_SEMIDET_SUCCESS_SLOT, MB_SEMIDET_FAILURE);

		/*
		** Also push a failure context in case fail is encountered
		*/
		MB_frame_temp_push(ms, bca->enter_proc.end_label.addr);
	}
#endif
}

static void
instr_endof_proc(MB_Machine_State *ms, const MB_Bytecode_Arg *endof_bca)
{
	/* get the current proc */
	MB_Bytecode_Arg *bca =
		MB_code_get_arg(endof_bca->endof_proc.proc_start);
	
	switch (bca->enter_proc.det) {
		case MB_DET_SEMIDET:
			MB_fatal("endof semidet");
#if 0
			/* put the success indicator into a register */
			MB_reg_set(ms, MB_SEMIDET_SUCCESS_REG,
				MB_temp_get(ms, MB_SEMIDET_SUCCESS_SLOT));

			/* remove the failure context */
			MB_maxfr_pop(ms);
#endif

		case MB_DET_DET: {
			MB_Word detframe_size = 
					bca->enter_proc.temp_count +
					bca->enter_proc.list_length +
					MB_DETFRAME_SIZE;

			MB_succip = MB_stackitem(MB_DETFRAME_SUCCIP);

			/* deallocate stack variables */
			MB_decr_sp(detframe_size);

			/* Check whether we should return to native code */
			if (MB_sp == MB_initialstackframe_get(ms)) {
				MB_SAY("trying to go native again");
				MB_native_return_set(ms, MB_succip);
				MB_ip_set(ms, MB_CODE_NATIVE_RETURN);
			} else {
				MB_SAY("just doing an easy bc ret");
				MB_SAY( "MB_sp:             %08x\n"
					"initialstackframe: %08x\n",
					MB_sp,
					ms->initial_stack);
				MB_ip_set(ms, MB_succip);
			}
			return;
		}
#if 0
		case MB_DET_MULTIDET:
		case MB_DET_NONDET: {
			MB_ip_set(ms, MB_frame_cur_get(ms, MB_FRAME_SUCCIP));
			MB_curfr_set(ms, MB_frame_cur_get(ms, MB_FRAME_SUCCFR));
			break;
		}
#endif
		/* XXX other options */
		default:
			instr_notdone(ms, NULL);
	}
	
	MB_proc_var_init(ms);
}

static void
instr_enter_disjunction(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("enter_disjunction");
#if 0
	/* push a new temp frame */
	MB_frame_temp_push(ms, MB_CODE_INVALID_ADR);
	instr_noop(ms, NULL);
#endif
}

static void
instr_enter_disjunct(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("enter_disjunct");
#if 0
	/*
	** set the redo point of the topmost frame (pushed in
	** enter_disjunction) to the disjunct after the current one
	**
	** if this is the last disjunct, then remove the top frame instead
	*/
	if (bca->enter_disjunct.next_label.addr == MB_CODE_INVALID_ADR) {
		/* remove the top frame */
		MB_maxfr_set(ms, MB_frame_max_get(ms, MB_FRAME_REDOFR));
	} else {
		/* set a new redoip */
		MB_frame_max_set(ms, MB_FRAME_REDOIP,
				bca->enter_disjunct.next_label.addr);
	}
	instr_noop(ms, NULL);
#endif
}

static void
instr_endof_disjunct(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("endof_disjunct");
#if 0
	/*
	** a simple jump to the end of the disjunction
	** if we are coming from a nonlast disjunct then we will
	** be leaving one or more nondet stack frames so we can backtrack
	** into the disjunction if we fail later on
	*/
	MB_ip_set(ms, bca->endof_disjunct.end_label.addr);
#endif
}

static void
instr_endof_disjunction(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("endof_disjunction");
#if 0
	/*
	** do nothing
	*/
	instr_noop(ms, NULL);
#endif
}

static void
instr_enter_switch(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	instr_noop(ms, NULL);
}

static void
instr_enter_switch_arm(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	/* Check if this deconstruct is going to succeed */
	if (do_deconstruct(ms, &bca->enter_switch_arm.cons_id,
			bca->enter_switch_arm.var, 0, 0))
	{
		/*
		** If it does succeed, then step into the switch
		*/
		instr_noop(ms, NULL);

	} else {
		/*
		** If it fails, go to the next switch arm
		*/
		MB_ip_set(ms, bca->enter_switch_arm.next_label.addr);
	}
}

static void
instr_endof_switch_arm(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	/* This switch arm has succeeded, now go to the end of the switch */
	MB_ip_set(ms, bca->endof_switch_arm.end_label.addr);
}

static void
instr_endof_switch(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	/*
	** If we get here, no switch arm matched, so trigger a redo
	*/
	instr_do_redo(ms, NULL);
}

static void
instr_enter_if(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("enter_if");
#if 0
	/*
	** push a failure context and save the frame address in a
	** temp stack slot
	*/
	MB_temp_set(ms, bca->enter_if.frame_ptr_tmp,
		MB_frame_temp_push(ms, bca->enter_if.else_label.addr)
		);

	instr_noop(ms, NULL);
#endif
}

/* enter_else is identical to enter_then */
/*
instr_enter_else()
*/
static void
instr_enter_then(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("enter_then");
#if 0
	MB_Word tempfr = MB_temp_get(ms, bca->enter_then.frame_ptr_tmp);

	/* If the frame is on top, can we pop it */
	if (MB_maxfr_get(ms) == tempfr) {
		MB_maxfr_pop(ms);
	} else {
		/* otherwise replace redoip with do_fail, effectively
		 * discarding it when the stack gets unwound */
		MB_frame_set(ms,
			tempfr + MB_FRAME_REDOIP,
			MB_CODE_DO_FAIL
			);
	}
	
	instr_noop(ms, NULL);
#endif
}

static void
instr_endof_then(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("endof_then");
#if 0
	/* jump to the end of the construct */
	MB_ip_set(ms, bca->endof_then.follow_label.addr);
#endif
}

static void
instr_endof_if(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("endof_if");
#if 0
	/* do nothing */
	instr_noop(ms, NULL);
#endif
}

static void
instr_enter_negation(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("enter_negation");
#if 0
	/* push a fail context: if the negation fails we want it
	** to drop through to the end of the negation and succeed
	*/
	MB_temp_set(ms, bca->enter_negation.frame_ptr_tmp,
		MB_frame_temp_push(ms, bca->enter_negation.end_label.addr));

	instr_noop(ms, NULL);
#endif
}

static void
instr_endof_negation_goal(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("endof_negation_goal");
#if 0
	/*
	** the negation has succeeded. Now we want to indicate
	** failure.
	** Rewind the stack back to before the negation and do a redo
	*/

	MB_maxfr_set(ms,
		MB_frame_get(ms, MB_FRAME_PREVFR + 
			MB_temp_get(ms, bca->endof_negation_goal.frame_ptr_tmp))
		);

	instr_do_redo(ms, NULL);
#endif	
}

static void
instr_endof_negation(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("endof_negation");
#if 0
	/*
	** the negation failed. remove the temp frame which will
	** be at the top and continue
	*/
	MB_maxfr_pop(ms);
	
	instr_noop(ms, NULL);
#endif
}

static void
instr_enter_commit(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("enter_commit");
#if 0
	/*
	** push a new stack frame & save its location in a temp
	** stack slot
	*/
	MB_temp_set(ms, bca->enter_commit.frame_ptr_tmp,
		MB_frame_temp_push(ms, MB_CODE_DO_FAIL));

	instr_noop(ms, NULL);	
#endif
}

static void
instr_endof_commit(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("endof_commit");
#if 0	
	/*
	** Unwind the stack back to where it was before the commit
	*/
	MB_maxfr_set(ms,
		MB_frame_get(ms, MB_FRAME_PREVFR +
			MB_temp_get(ms, bca->endof_commit.frame_ptr_tmp))
		);

	instr_noop(ms, NULL);	
#endif
}

static void
instr_assign(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("assign");
#if 0	
	/* copy variable from one slot to another */
	MB_var_set(ms, bca->assign.to_var, MB_var_get(ms,bca->assign.from_var));

	instr_noop(ms, NULL);	
#endif
}

static void
instr_test(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("test");
#if 0
	/* test the equality of two variable slots */
	if (MB_var_get(ms, bca->test.var1) == MB_var_get(ms, bca->test.var2)) {
		instr_noop(ms, NULL);
	} else {
		instr_do_redo(ms, NULL);
	}
#endif
}

static MB_Word
do_construct_cons(MB_Machine_State *ms, const MB_Cons_id *cid,
		MB_Word list_length, MB_Short *var_list)
{
	MB_fatal("do_construct_cons");
#if 0
	const MB_Tag	*cons_tag = &cid->opt.cons.tag;
	MB_Word		*val = MB_mkword(
				MB_mktag(cons_tag->opt.pair.primary),
				MB_mkbody((MB_Word) NULL));

	/* the final value we will put in the reg */

	assert(cid->id == MB_CONSID_CONS);
			
	assert(cid->opt.cons.arity == list_length);

	switch (cons_tag->id) {
		case MB_TAG_SIMPLE: /* only need a primary tag */
		case MB_TAG_COMPLICATED: /* need primary + remote 2ndary tag */
		{
			/*
			** The code for these two is virtually identical except
			** that if it is complicated we need one extra heap
			** slot for the remote secondary tag
			*/
			MB_Word	extra = (cons_tag->id == MB_TAG_COMPLICATED)
					? 1 : 0;
			MB_Word *heap_data;
			MB_Word	i;
			

			if (list_length + extra) {
				MB_Short *var_list;

				/* allocate heap memory */
				heap_data = (MB_Word *) MB_GC_new_array(
						MB_Word, list_length + extra);

				/* ensure tag bits aren't used */
				assert(MB_tag((MB_Word) heap_data) == 0);

				/* get variable list */
				var_list = (MB_Short *)MB_stack_peek_p(
						&ms->code.data,
						var_list_index);

				/* copy variables to allocate heap block */
				for (i = 0; i < list_length; i++) {
					heap_data[i + extra] =
						MB_var_get(ms, var_list[i]);
				}
			} else {
				heap_data = NULL;
			}

			/*
			** copy the secondary tag if we need to
			** and combine the pointer & tag
			*/
			if (cons_tag->id == MB_TAG_COMPLICATED_CONSTANT) {
				heap_data[0] = cons_tag->opt.pair.secondary;
				val = MB_mkword(
					MB_mktag(cons_tag->opt.pair.primary),
					MB_body((MB_Word) heap_data,
							MB_mktag(0)));
			} else {
				val = MB_mkword(
					MB_mktag(cons_tag->opt.primary),
					MB_body((MB_Word) heap_data,
							MB_mktag(0)));
			}
			
			break;
		}
			
		case MB_TAG_COMPLICATED_CONSTANT:
			/* primary + local secondary tag */
			assert(list_length == 0);
			val = MB_mkword(
				MB_mktag(cons_tag->opt.pair.primary),
				MB_mkbody(cons_tag->opt.pair.secondary));

			break;
			
		case MB_TAG_ENUM:
			assert(list_length == 0);
			val = MB_mkword(MB_mktag(cons_tag->opt.enum_tag),
					MB_mkbody(0));
			break;

		case MB_TAG_NONE:
		default:
			instr_notdone(ms, NULL);
	}
	return (MB_Word) val;
#endif
	return 0;
}

static void
instr_construct(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_Word val;
	/* construct a variable into a slot */
	switch (bca->construct.consid.id) {
		case MB_CONSID_INT_CONST:
			assert(bca->construct.list_length == 0);
			val = bca->construct.consid.opt.int_const;
			break;

		case MB_CONSID_STRING_CONST:
			assert(bca->construct.list_length == 0);
			val = (MB_Word) bca->construct.consid.opt.string_const;
			break;

		case MB_CONSID_CONS:
			val = do_construct_cons(ms,
				&bca->construct.consid,
				bca->construct.list_length,
				bca->construct.var_list);
			break;

		case MB_CONSID_FLOAT_CONST:
			instr_notdone(ms, NULL);
			
		case MB_CONSID_PRED_CONST: {
			MB_fatal("Construct closure not done");
		#if 0
			int i;

			MB_Closure *closure = MB_GC_malloc(
				MB_CLOSURE_SIZE(bca->construct.list_length),
				MB_GC_NOT_ATOMIC);
			MB_Short *var_list = (MB_Short *)MB_stack_peek_p(
					&ms->code.data,
					bca->construct.var_list_index);

			closure->code_addr = bca->construct
						.consid.opt.pred_const.addr;
			closure->num_hidden_args = bca->construct.list_length;

			for (i = 0; i < closure->num_hidden_args; i++) {
				closure->closure_hidden_args[i] =
					MB_var_get(ms, var_list[i]);
			}

			val = (MB_Word) closure;

			break;
		#endif
		}
			
		case MB_CONSID_CODE_ADDR_CONST:
		case MB_CONSID_BASE_TYPE_INFO_CONST:
			instr_notdone(ms, NULL);

		case MB_CONSID_CHAR_CONST:
			val = (MB_Word) bca->construct.consid.opt.char_const.ch;
			break;

		default:
			instr_notdone(ms, NULL);
	}
	MB_var_set(ms, bca->construct.to_var, val);
	instr_noop(ms, NULL);
}

/*
** returns true if the deconstruction succeeds
** if a int/string/char const, checks for equality and triggers a redo if it
** fails.
** if a functor then deconstructs arguments into variable slots
*/
static MB_Bool
do_deconstruct(MB_Machine_State *ms, const MB_Cons_id *cid, MB_Word var, 
		MB_Word list_length, MB_Short *var_list)
{
	MB_Word var_val = MB_var_get(ms, var);
	
	/* XXX not all deconstructions done */
	switch (cid->id) {
		case MB_CONSID_INT_CONST:
			return (var_val == cid->opt.int_const);

		case MB_CONSID_STRING_CONST:
			return (!MB_str_cmp((char *)var_val,
				cid->opt.string_const));

		case MB_CONSID_CONS: {
			return do_deconstruct_cons(ms, cid, var_val,
					list_length, var_list);
		}

		case MB_CONSID_CHAR_CONST:
			return (var_val == (MB_Word) cid->opt.char_const.ch);

		default:
			instr_notdone(ms, NULL);
	}

	assert(FALSE);
	return FALSE;
}

/* returns true if val is equivalent to a construction given by cid */
static MB_Bool
do_deconstruct_cons(MB_Machine_State *ms, const MB_Cons_id *cid,
		MB_Word val, MB_Word list_length, MB_Short *var_list)
{
	const MB_Tag *cons_tag = &cid->opt.cons.tag;

	assert(cid->id == MB_CONSID_CONS);

	/*
	** We should either check all variables (eg: deconstruct instruction)
	** or none of them (eg: switch_arm instruction)
	*/
	assert((cid->opt.cons.arity == list_length) || (list_length == 0));

	switch (cons_tag->id) {
		case MB_TAG_SIMPLE: /* only need a primary tag */
		case MB_TAG_COMPLICATED: /* need primary + remote 2ndary tag */
		{
			/*
			** The code for these two is virtually identical except
			** that if it is complicated we need one extra heap
			** slot for the remote secondary tag
			*/
			MB_Word	extra = (cons_tag->id == MB_TAG_COMPLICATED)
				? 1 : 0;
			MB_Word *heap_data = (MB_Word *)MB_strip_tag(val);
			MB_Word	i;

			/* check that tags are identical */
			if (cons_tag->id == MB_TAG_COMPLICATED) {
				if ((MB_tag(val) != cons_tag->opt.pair.primary)
					|| (heap_data[0] !=
						cons_tag->opt.pair.secondary))
				{
					return FALSE;
				}
			} else {
				if (MB_tag(val) != cons_tag->opt.primary) {
					return FALSE;
				}
			}
			

			/* Deconstruct variables */
			if (list_length) {
				/* ensure variables are the same */
				for (i = 0; i < list_length; i++)
				{
					MB_var_set(ms, var_list[i],
							heap_data[i + extra]);
				}
			}

			break;
		}
			
		case MB_TAG_COMPLICATED_CONSTANT:
			/* primary + local secondary tag */
			assert(list_length == 0);
			if (val != (MB_Word) MB_mkword(
				MB_mktag(cons_tag->opt.pair.primary),
				MB_mkbody(cons_tag->opt.pair.secondary)))
			{
				return FALSE;
			}

			break;
			
		case MB_TAG_ENUM:
			assert(list_length == 0);
			if (val != (MB_Word)
				MB_mkword(MB_mktag(cons_tag->opt.enum_tag),
				MB_mkbody(0)))
			{
				return FALSE;
			}
			break;

		case MB_TAG_NONE:
		default:
			instr_notdone(ms, NULL);
	}

	return TRUE;
}


static void
instr_deconstruct(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("deconstruct");
	/* test the equality of a variable in a slot with a given value */
	if (!do_deconstruct(ms, &bca->deconstruct.consid,
				bca->deconstruct.from_var,
				bca->deconstruct.list_length,
				bca->deconstruct.var_list))
	{
		instr_do_redo(ms, NULL);
	} else {
		instr_noop(ms, NULL);
	}
}

static void
instr_place(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	/* copy value from var slot to reg */
	MB_reg(bca->place_arg.to_reg) =
		MB_var_get(ms, bca->place_arg.from_var);

	#if CLOBBERPLACES
		/* XXX for debugging only */
		MB_var_set(ms, bca->place_arg.from_var, CLOBBERED);
	#endif

	/* go to the next instruction */
	instr_noop(ms, NULL);
}

static void
instr_pickup(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	/* copy value from reg to var slot */
	MB_var_set(ms, bca->pickup_arg.to_var,
		MB_reg(bca->pickup_arg.from_reg));

	#if CLOBBERPICKUPS
		/* XXX for debugging only */
		MB_reg_set(ms, bca->pickup_arg.from_reg, CLOBBERED);
	#endif

	/* go to the next instruction */
	instr_noop(ms, NULL);
}

static void
instr_call(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	/* Call another procedure */
	
	MB_Bytecode_Addr next_ip = MB_ip_get(ms) + 1;

	/* Call bytecode */
	if (!bca->call.addr.is_native) {
		MB_Bytecode_Addr new_addr = bca->call.addr.addr.bc;
		if (new_addr == MB_CODE_INVALID_ADR) {
			MB_util_error("Attempt to call unknown bytecode"
					" %s %s__%s/%d mode %d",
					bca->call.is_func ? "func" : "pred",
					bca->call.module_name,
					bca->call.pred_name,
					(int) bca->call.arity,
					(int) bca->call.mode_num);
			MB_fatal("");
		} else {
			if (MB_ip_normal(new_addr)) {
				/* set the return address to the next instr */
				MB_succip = next_ip;
				/* set the new execution point */
				MB_ip_set(ms, new_addr);
			} else {
				MB_fatal("Unexpected call address"
					" (special address not implemented?)");
			}
		}

	/* Call native code */
	} else {
		MB_Native_Addr new_addr = bca->call.addr.addr.native;
		MB_SAY("Attempting to call native code from bytecode");
		
		/* Make sure the address has been looked up */
		if (new_addr == NULL) {
			new_addr = MB_code_find_proc_native(
					bca->call.module_name,
					bca->call.pred_name,
					bca->call.arity,
					bca->call.mode_num,
					bca->call.is_func);
			if (new_addr == NULL) {
				MB_util_error(
					"Warning: proc ref in bytecode"
					" to unknown %s %s__%s/%d mode %d",
					bca->call.is_func ? "func" : "pred",
					bca->call.module_name,
					bca->call.pred_name,
					(int) bca->call.arity,
					(int) bca->call.mode_num);
				MB_fatal("Are you sure the module"
					" was compiled with trace"
					" information enabled?");
			}

			/* XXX: Write to constant data */
			bca->call.addr.addr.native = new_addr;
		}

		if (ms->cur_proc.is_det == MB_ISDET_YES) {
			/* Push a interface det stack frame */
			MB_incr_sp(MB_DETFRAME_INTERFACE_SIZE);

			/* Set success ip to reentry stub */
			MB_succip = MB_native_get_return_det();

			/* Save initial stack frame pointer */
			MB_stackitem(MB_DETFRAME_INTERFACE_INITIAL_FRAME)
				= (MB_Word) MB_initialstackframe_get(ms);

			/* Save bytecode reentry point */
			MB_stackitem(MB_DETFRAME_INTERFACE_BC_SUCCIP)
				= (MB_Word) next_ip;

			/* return to native code at address new_addr */
			MB_ip_set(ms, MB_CODE_NATIVE_RETURN);
			MB_native_return_set(ms, new_addr);

			MB_SAY("New ip:        %08x\n", ms->ip);
			MB_SAY("native_return: %08x\n", ms->native_return);
		} else {
			MB_fatal("Native calls from nondet code not done");
		}
	}
}

/*
**
** XXX:
** 
** Why does the call need to know the number of output arguments ???
**
** If semidet, do I need to make space for the extra argument or has
** the mercury compiler already done that ???
**
*/
static void
instr_higher_order_call(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("higher_order_call");
#if 0
	MB_Closure *closure = (MB_Closure *)MB_var_get(ms,
				bca->higher_order_call.pred_var);
	/*
	** shift the input arguments to the right so we can insert the
	** arguments inside the closure
	*/
	if (bca->higher_order_call.in_var_count != 0) {
		signed int i = closure->num_hidden_args;
		signed int j = i + bca->higher_order_call.in_var_count;
		for (; i >= 1; i--, j--) {
			MB_reg_set(ms, j, MB_reg_get(ms, i));
		}
	}
	/*
	** Now insert the hidden arguments
	*/
	if (closure->num_hidden_args) {
		signed int i;
		MB_Word num_hidden_args = closure->num_hidden_args;
		for (i = 1; i <= num_hidden_args; i++) {
			MB_reg_set(ms, i, closure->closure_hidden_args[i-1]);
		}
	}
	
	/*
	** Do the actual call
	*/
	
	/* set the return address to the next instruction */
	MB_succip_set(ms, MB_ip_get(ms) + 1);

	/* set the new execution point */
	MB_ip_set(ms, closure->code_addr);
#endif
}
/* -------------------------------------------------------------------------- */

static MB_Word binop_add	(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word binop_sub	(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word binop_mul	(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word binop_div	(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word binop_mod	(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word binop_lshift	(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word binop_rshift	(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word binop_and	(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word binop_or		(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word binop_xor	(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word binop_logand	(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word binop_logor	(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word binop_eq		(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word binop_ne		(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word binop_lt		(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word binop_gt		(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word binop_le		(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word binop_ge		(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word binop_bad	(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);


typedef MB_Word (*MB_Instruction_Binop)	(MB_Machine_State *,
						const MB_Bytecode_Arg *);
/*
** XXX ORDER Currently we depend on the order of elements in the table.
*/
static MB_Instruction_Binop binop_table[] = {
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
	binop_##name(MB_Machine_State *ms, const MB_Bytecode_Arg *bca) \
	{ \
		assert(bca->builtin_binop.arg1.id == MB_ARG_VAR); \
		assert(bca->builtin_binop.arg2.id == MB_ARG_VAR); \
		return (MB_Integer)(MB_var_get(ms, \
					bca->builtin_binop.arg1.opt.var)) \
			op (MB_Integer)(MB_var_get(ms, \
					bca->builtin_binop.arg2.opt.var)); \
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
binop_bad(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("Unsupported binop\n");
	return 0;
}



static void
instr_builtin_binop(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_Byte binop = bca->builtin_binop.binop;
	if (binop < (sizeof(binop_table)/sizeof(binop_table[0]))) {
		MB_var_set(ms,
			bca->builtin_binop.to_var,
			binop_table[bca->builtin_binop.binop](ms, bca));

		instr_noop(ms, NULL);
	} else {
		MB_fatal("Invalid binop");
	}
}

static void
instr_builtin_bintest(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("builtin_bintest");
#if 0
	MB_Byte binop = bca->builtin_binop.binop;
	if (binop < (sizeof(binop_table)/sizeof(binop_table[0]))) {
		if (binop_table[bca->builtin_binop.binop](ms, bca)) {
			/* If successful, just go to the next instr */
			instr_noop(ms, NULL);
		} else {
			/* otherwise follow the failure context */
			/*instr_do_fail(ms, NULL);*/
			instr_do_redo(ms, NULL);
		}
	} else {
		MB_fatal("Invalid bintest");
	}
#endif
}
/* -------------------------------------------------------------------------- */
static MB_Word unop_bad		(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word unop_bitwise_complement
				(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
static MB_Word unop_not		(MB_Machine_State*ms,const MB_Bytecode_Arg*bca);
/*
** XXX ORDER Currently we depend on the order of elements in the table
*/
static MB_Word (*unop_table[])(MB_Machine_State *ms,
				const MB_Bytecode_Arg *bca) =
{
	unop_bad,		/* mktag */
	unop_bad,		/* tag */
	unop_bad, 		/* unmktag */
	unop_bad, 		/* unmkbody */
	unop_bad,		/* cast_to_unsigned */
	unop_bad,		/* hash_string */
	unop_bitwise_complement,
	unop_not
};

#define SIMPLEUNOP(name, op)	\
	static MB_Word \
	unop_##name(MB_Machine_State *ms, const MB_Bytecode_Arg *bca) \
	{ \
		assert(bca->builtin_unop.arg.id == MB_ARG_VAR); \
		return op (MB_Integer) \
			(MB_var_get(ms, bca->builtin_unop.arg.opt.var)); \
	}

SIMPLEUNOP(bitwise_complement,		~)
SIMPLEUNOP(not,				!)

static MB_Word
unop_bad(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("Unsupported unop\n");
	return 0;
}

static void
instr_builtin_unop(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("builtin_unop");
#if 0
	MB_Byte unop = bca->builtin_unop.unop;
	
	/* XXX until I learn properly what unary operations are */
	instr_notdone(ms, NULL);
	
	if (unop < (sizeof(unop_table)/sizeof(unop_table[0]))) {
		MB_var_set(ms,
			bca->builtin_unop.to_var,
			unop_table[bca->builtin_unop.unop](ms, bca));

		instr_noop(ms, NULL);
	} else {
		MB_fatal("Invalid unop");
	}
#endif
}


static void
instr_builtin_untest(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("builtin_untest");
#if 0
	instr_notdone(ms, NULL);
#endif
}

/* --------------------------------------------------------------------- */
static void
instr_semidet_success(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("semidet_success");
#if 0
	MB_temp_set(ms, MB_SEMIDET_SUCCESS_SLOT, MB_SEMIDET_SUCCESS);

	instr_noop(ms, NULL);
#endif
}

static void
instr_semidet_success_check(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("semidet_success_check");
#if 0
	if (MB_reg_get(ms, MB_SEMIDET_SUCCESS_REG) != MB_SEMIDET_SUCCESS) {
		instr_do_redo(ms, NULL);
	} else {
		instr_noop(ms, NULL);
	}
#endif
}

static void
instr_do_redo(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("do_redo");
#if 0
	MB_SAY("setting redo -> %d", MB_frame_max_get(ms, MB_FRAME_REDOIP));
	MB_ip_set(ms, MB_frame_max_get(ms, MB_FRAME_REDOIP));
	MB_curfr_set(ms, MB_frame_max_get(ms, MB_FRAME_REDOFR));

	MB_SAY("checking proc", MB_frame_max_get(ms, MB_FRAME_REDOIP));
	MB_proc_var_init(ms);
	MB_SAY("checked proc", MB_frame_max_get(ms, MB_FRAME_REDOIP));
#endif
}

static void
instr_do_fail(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("do_fail");
#if 0
	MB_maxfr_pop(ms);
	instr_do_redo(ms, bca);
#endif
}

static void
instr_noop(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	/* increment instruction pointer */
	MB_ip_set(ms, MB_ip_get(ms) + 1);
}

static void
instr_notdone(MB_Machine_State *ms, const MB_Bytecode_Arg *bca)
{
	MB_fatal("notdone");
#if 0
	/* invalid instruction */
	MB_fatal("That instruction is not implemented yet\n");
	instr_noop(ms, NULL);
#endif
}

/*
** Execute the current instruction. Returns false if instruction could
** not be executed
*/
static MB_Bool
dispatch(MB_Byte bc_id, MB_Machine_State *ms)
{
	MB_Bytecode_Addr ip = MB_ip_get(ms);
	
	if (bc_id < sizeof(instruction_table) / sizeof(instruction_table[0])) {
		instruction_table[bc_id](ms, MB_code_get_arg(ip));
		return TRUE;
	}
	
	return FALSE;
}

/* Single step execute */
void
MB_step(MB_Machine_State *ms)
{
	MB_fatal("Untested step");
#if 0
	MB_Word ip = MB_ip_get(ms);

	MB_Byte bc_id = MB_code_get_id(ms, ip);
	if (!dispatch(bc_id, ms)) {
		MB_fatal("Invalid instruction encountered\n");
		instr_noop(ms, NULL);
	}
#endif
}

void
MB_step_over(MB_Machine_State *ms)
{
	MB_fatal("Untested step_over");
#if 0
	MB_Word ip = MB_ip_get(ms);
	MB_Byte bc_id = MB_code_get_id(ms, ip);
	
	if (ip == MB_CODE_INVALID_ADR) {
		MB_util_error("Attempt to execute invalid code address\n");
	}

	switch (bc_id) {
		case MB_BC_call: {
			
			/* If we are about to step into a predicate */
			/* then replace the following bytecode with */
			/* an MB_BC_debug_trap and run until it halts */
			/* then put things back to what they were */
			MB_Byte old_id;
			assert(ip + 1 < MB_code_size(ms));
			old_id = ms->code.id[ip + 1];

			ms->code.id[ip + 1] = MB_BC_debug_trap;
			MB_run(ms);

			ms->code.id[ip + 1] = old_id;
			break;
		}
		default:
			MB_step(ms);
	}
#endif
}

/* Run until invalid instruction or debug_trap bytecode encountered */
void
MB_run(MB_Machine_State *ms)
{
	MB_fatal("Untested run");
#if 0
	do {
		MB_Word ip = MB_ip_get(ms);

		MB_Byte bc_id = MB_code_get_id(ms, ip);
		if (!dispatch(bc_id, ms)) {
			switch (bc_id) {
				case MB_BC_debug_trap:
					return;
			}
			MB_util_error(
				"Attempt to execute invalid instruction\n");
			instr_noop(ms, NULL);
			return;
		}
	} while (1);
#endif
}

/* --------------------------------------------------------------------- */
void
MB_machine_create(MB_Machine_State *ms, MB_Bytecode_Addr new_ip, 
		MB_Word *initial_stack)
{
	
	ms->ip = new_ip;
	ms->initial_stack = initial_stack;
	MB_proc_var_init(ms);
}

MB_Native_Addr
MB_machine_exec(MB_Machine_State *ms)
{
	char buffer[4];
	MB_Word count = 0;
	MB_SAY("Hello from machine_exec");

	do {
		MB_Bytecode_Addr ip = MB_ip_get(ms);

		if (MB_ip_normal(ip)) {

			MB_Byte bc_id = MB_code_get_id(ip);

			#if 1
				MB_show_state(ms, stdout);
				MB_SAY("count: %d, execing %p", count++, ip);
				MB_SAY("press enter to continue");
				fgets(buffer, sizeof(buffer), stdin);
			#endif

			if (!dispatch(bc_id, ms)) {
				switch (bc_id) {
					case MB_BC_debug_trap:
						return 0;
				}
				MB_util_error("Attempt to execute"
					" invalid instruction\n");
				instr_noop(ms, NULL);
				return 0;
			}
		} else {
			switch ((MB_Word) ip) {
				case (MB_Word) MB_CODE_NATIVE_RETURN:
					MB_SAY("Returning to a native state");
					return MB_native_return_get(ms);
				default:
					MB_SAY("Address %08x", ip);
					MB_fatal("Attempt to execute invalid"
						" address\n");
			}
		}
	} while (1);

	return 0;
}


