/*
** Copyright (C) 2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
*/

/* Imports */
#include	"mercury_imp.h"
#include	"mercury_ho_call.h"

#include	"mb_exec.h"

#include	<assert.h>
#include	<stdio.h>
#include	<string.h>
#include	"mb_interface.h"
#include	"mb_mem.h"
#include	"mb_machine_show.h"
#include	"mb_module.h"

/* Exported definitions */
MB_Native_Addr	MB_machine_exec(MB_Bytecode_Addr new_ip,
				MB_Word *initial_stack);

/* Local declarations */

/* Set new stack vars to this help find bugs */
#define CLOBBERED	0xbadbad00

#define CLOBBERPICKUPS	0	/* clobber reg after pickup */
#define CLOBBERPLACES	0	/* clobber slot after place */
#define CLOBBERSTACK	1	/* reset new stack vars */

static MB_Bool	dispatch(MB_Byte bc_id, MB_Machine_State *ms);

static void instr_do_redo	(MB_Machine_State*ms, MB_Bytecode_Arg*bca);
static void instr_do_fail	(MB_Machine_State*ms, MB_Bytecode_Arg*bca);

static void instr_invalid		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_enter_proc		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_endof_proc		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_enter_disjunction	(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_endof_disjunction	(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_enter_disjunct	(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_endof_disjunct	(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_enter_switch		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_enter_switch_arm	(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_endof_switch_arm	(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_endof_switch		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_enter_if		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_enter_then		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_endof_then		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
/* instr_enter_else is identical to enter_then */
static void instr_endof_if		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_enter_negation	(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_endof_negation_goal	(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_endof_negation	(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_enter_commit		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_endof_commit		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_assign		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_test			(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_construct		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_deconstruct		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_place			(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_pickup		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_call			(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_higher_order_call	(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_builtin_binop		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_builtin_bintest	(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_builtin_unop		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_builtin_untest	(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_semidet_success	(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_semidet_success_check	(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_do_redo		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_do_fail		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_noop			(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);
static void instr_notdone		(MB_Machine_State *ms,
						MB_Bytecode_Arg *bca);

/* return true if a construction succeeds */
static MB_Word do_construct_cons(MB_Machine_State *ms, const MB_Cons_id *cid,
		MB_Word list_length, MB_Short *var_list);

/* return true if a deconstruction succeeds */
static MB_Bool do_deconstruct(MB_Machine_State *ms, const MB_Cons_id *cid,
		MB_Word var, MB_Word list_length, MB_Short *var_list);
static MB_Bool do_deconstruct_cons(MB_Machine_State *ms, const MB_Cons_id *cid,
		MB_Word val, MB_Word list_length, MB_Short *var_list);

/* Calls a native code procedure and sets up reentry variables */
static void	call_native_proc(MB_Machine_State *ms,
	       			MB_Native_Addr native_addr,
			       	MB_Bytecode_Addr return_ip);

typedef void (*MB_Instruction_Handler)	(MB_Machine_State *, MB_Bytecode_Arg *);

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
instr_invalid(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	MB_fatal("Invalid instruction encountered");
}


/* Enter/exit procedure */
static void
instr_enter_proc(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	switch (bca->enter_proc.det) {
		case MB_DET_FAILURE:
		case MB_DET_SEMIDET:
		case MB_DET_CC_NONDET:
		case MB_DET_DET:
		case MB_DET_CC_MULTIDET: {
			MB_Word detframe_size = 
					bca->enter_proc.temp_count +
					bca->enter_proc.list_length +
					MB_DETFRAME_SIZE;

			/*
			** Save the initial stack frame if this function is
			** going to be the one that returns to native code
			*/
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

			MB_Word *prevfr = MB_maxfr;
			MB_Word *succfr = MB_curfr;

			if (MB_initialstackframe_get(ms) == NULL) {
				MB_initialstackframe_set(ms, MB_maxfr);
			}

			MB_maxfr += MB_FRAME_NORMAL_SIZE
					+ bca->enter_proc.list_length
					+ bca->enter_proc.temp_count;

			MB_curfr = MB_maxfr;

			MB_fr_prevfr(MB_curfr) = prevfr;
			MB_fr_redoip(MB_curfr) = (MB_Word)
							MB_native_get_do_fail();
			MB_fr_redofr(MB_curfr) = (MB_Word) MB_maxfr;
			MB_fr_succip(MB_curfr) = (MB_Word) MB_succip;
			MB_fr_succfr(MB_curfr) = succfr;
			/*
			** bcretip is set just before a procedure call so that
			** bytecode_return_nondet knows where in the bytecode to
			** jump to. Set to NULL for now to catch errors.
			*/
			MB_fr_bcretip(MB_curfr) = (MB_Word) NULL;
			MB_fr_bcinitfr(MB_curfr) = (MB_Word)
						MB_initialstackframe_get(ms);

			MB_ip_set(ms, MB_ip_get(ms) + 1);
			
			break;
		}
		/* XXX Other options */
		default:
			MB_fatal("enter_proc det type not implemented");
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

	if (MB_model_semi(bca->enter_proc.det)) {
		/*
		** If a semidet procedure then mark our success slot as failure
		** until we know otherwise.
		*/
		MB_stackitem(MB_DETFRAME_SEMIDET_SUCCESS) = MB_SEMIDET_FAILURE;

		/* Also push a failure context in case fail is encountered */
		MB_frame_temp_push(ms, bca->enter_proc.end_label.addr);
	}
}

static void
instr_endof_proc(MB_Machine_State *ms, MB_Bytecode_Arg *endof_bca)
{
	/* get the current proc */
	MB_Bytecode_Arg *bca =
		MB_code_get_arg(endof_bca->endof_proc.proc_start);
	
	switch (bca->enter_proc.det) {
		case MB_DET_FAILURE:
		case MB_DET_CC_NONDET:
		case MB_DET_SEMIDET:
			/* put the success indicator into a register */
			MB_reg(MB_SEMIDET_SUCCESS_REG) =
				MB_stackitem(MB_DETFRAME_SEMIDET_SUCCESS);

			/* remove the failure context */
			MB_maxfr = MB_fr_prevfr(MB_maxfr);
		case MB_DET_CC_MULTIDET:
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
				MB_native_return_set(ms, MB_succip);
			} else {
				MB_ip_set(ms, MB_succip);
			}
			break;
		}

		case MB_DET_MULTIDET:
		case MB_DET_NONDET: {
			/* We don't deallocate the stack */
		
			MB_Word	*old_curfr = MB_curfr;
			/* Restore succip */
			MB_succip = MB_fr_succip(MB_curfr);

			/* Restore curfr */
			MB_curfr = MB_fr_succfr(MB_curfr);

			/* Check whether we should return to native code */
			if (MB_fr_prevfr(old_curfr) ==
					MB_initialstackframe_get(ms))
			{
				MB_native_return_set(ms, MB_succip);
			} else {
				MB_ip_set(ms, MB_succip);
			}

			break;
		}
		/* XXX other options */
		default:
			MB_fatal("endof_proc det type not implemented");
	}
	
	MB_proc_var_init(ms);
}

static void
instr_enter_disjunction(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	/* push a new temp frame */
	MB_frame_temp_push(ms, MB_CODE_INVALID_ADR);
	instr_noop(ms, NULL);
}

static void
instr_enter_disjunct(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	/*
	** set the redo point of the topmost frame (pushed in
	** enter_disjunction) to the disjunct after the current one
	**
	** if this is the last disjunct, then remove the top frame instead
	*/
	if (bca->enter_disjunct.next_label.addr == MB_CODE_INVALID_ADR) {
		/* remove the top frame */
		/* XXX TESTING */
		MB_maxfr = MB_fr_prevfr(MB_maxfr);
	} else {
		/* set a new redoip */

		/*
		** We know it is a frame from bytecode, but was it a temp nondet
		** frame from a det or nondet proc?
		*/

		assert(MB_FRAME_TEMP_DET_SIZE != MB_FRAME_TEMP_NONDET_SIZE);

		if (MB_frame_size(MB_maxfr) == MB_FRAME_TEMP_DET_SIZE) {
			MB_fr_temp_det_bcredoip(MB_maxfr) 
				= (MB_Word) bca->enter_disjunct.next_label.addr;
		} else {
			MB_fr_temp_nondet_bcredoip(MB_maxfr) 
				= (MB_Word) bca->enter_disjunct.next_label.addr;
		}
	}
	instr_noop(ms, NULL);
}

static void
instr_endof_disjunct(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	/*
	** a simple jump to the end of the disjunction
	** if we are coming from a nonlast disjunct then we will
	** be leaving one or more nondet stack frames so we can backtrack
	** into the disjunction if we fail later on
	*/
	MB_ip_set(ms, bca->endof_disjunct.end_label.addr);
}

static void
instr_endof_disjunction(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	/*
	** do nothing
	*/
	instr_noop(ms, NULL);
}

static void
instr_enter_switch(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	instr_noop(ms, NULL);
}

static void
instr_enter_switch_arm(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
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
instr_endof_switch_arm(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	/* This switch arm has succeeded, now go to the end of the switch */
	MB_ip_set(ms, bca->endof_switch_arm.end_label.addr);
}

static void
instr_endof_switch(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	/*
	** If we get here, no switch arm matched, so trigger a redo
	*/
	instr_do_redo(ms, NULL);
}

static void
instr_enter_if(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	/*
	** push a failure context and save the frame address in a
	** temp stack slot
	*/
	MB_frame_temp_push(ms, bca->enter_if.else_label.addr);
	MB_var_set(ms, bca->enter_if.frame_ptr_tmp, (MB_Word) MB_maxfr);

	instr_noop(ms, NULL);
}

/* enter_else is identical to enter_then */
/*
instr_enter_else()
*/
static void
instr_enter_then(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	MB_Word *tempfr = (MB_Word *)
				MB_var_get(ms, bca->enter_then.frame_ptr_tmp);

	/* If the frame is on top, can we pop it */
	if (MB_maxfr == tempfr) {
		MB_maxfr = MB_fr_prevfr(MB_maxfr);
	} else {
		/* otherwise replace redoip with do_fail, effectively
		 * discarding it when the stack gets unwound */
		MB_fr_redoip(tempfr) = (MB_Word) MB_native_get_do_fail();
	}
	
	instr_noop(ms, NULL);
}

static void
instr_endof_then(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	/* Jump to the end of the construct */
	MB_ip_set(ms, bca->endof_then.follow_label.addr);
}

static void
instr_endof_if(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	/* Do nothing */
	instr_noop(ms, NULL);
}

static void
instr_enter_negation(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	/* 
	** Push a fail context: If the negation fails we want it
	** to drop through to the end of the negation and succeed
	*/
	MB_var_set(ms, bca->enter_negation.frame_ptr_tmp, (MB_Word) MB_maxfr);
	MB_frame_temp_push(ms, bca->enter_negation.end_label.addr);

	instr_noop(ms, NULL);
}

static void
instr_endof_negation_goal(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	/*
	** The negation has succeeded. Now we want to indicate failure.
	** Rewind the stack back to before the negation and issue a redo
	*/

	MB_maxfr = MB_var_get(ms, bca->endof_negation_goal.frame_ptr_tmp);

	instr_do_redo(ms, NULL);
}

static void
instr_endof_negation(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	/*
	** The negation failed.
	** Remove the temp frame which will be at the top and continue
	*/
	MB_maxfr = MB_fr_prevfr(MB_maxfr);
	
	instr_noop(ms, NULL);
}

static void
instr_enter_commit(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	/* push a new stack frame & save its location in a temp stack slot */
	MB_frame_temp_push_do_fail(ms);
	MB_var_set(ms, bca->enter_commit.frame_ptr_tmp, (MB_Word) MB_maxfr);

	instr_noop(ms, NULL);	
}

static void
instr_endof_commit(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	/* Unwind the stack back to where it was before the commit */
	MB_maxfr = MB_var_get(ms, bca->endof_commit.frame_ptr_tmp);

	instr_noop(ms, NULL);	
}

static void
instr_assign(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	/* copy variable from one slot to another */
	MB_var_set(ms, bca->assign.to_var,
			MB_var_get(ms, bca->assign.from_var));

	instr_noop(ms, NULL);	
}

static void
instr_test(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	int result;

	/* test the equality of two variable slots */
	switch (bca->test.id) {
		case MB_TESTID_INT:
		case MB_TESTID_CHAR:
		case MB_TESTID_ENUM:
			result =
			       	MB_var_get(ms, bca->test.var1)
			       	== MB_var_get(ms, bca->test.var2);
			break;
		case MB_TESTID_STRING:
			result = !strcmp(MB_var_get(ms, bca->test.var1),
					MB_var_get(ms, bca->test.var2));
			break;
		case MB_TESTID_FLOAT:
			MB_fatal("Float testing not supported");
		default:
			MB_fatal("Unexpected test type");
	}

	if (result) {		
		instr_noop(ms, NULL);
	} else {
		instr_do_redo(ms, NULL);
	}
}

static MB_Word
do_construct_cons(MB_Machine_State *ms, const MB_Cons_id *cid,
		MB_Word list_length, MB_Short *var_list)
{
	const MB_Tag	*cons_tag = &cid->opt.cons.tag;
	MB_Word		*val = MB_mkword(
				MB_mktag(cons_tag->opt.pair.primary),
				MB_mkbody((MB_Word) NULL));

	/* the final value we will put in the reg */

	assert(cid->id == MB_CONSID_CONS);
	
	/* 
	** XXX: If list_length can be anything, then what is the use of
	** the arity field for functors??
	*/
	/* assert(list_length != 0); */

	switch (cons_tag->id) {
		case MB_TAG_SIMPLE: 	/* only need a primary tag */
		case MB_TAG_COMPLICATED:/* need primary + remote 2ndary tag */
		{
			/*
			** The code for these two is virtually identical except
			** that if it is tag_complicated we need one extra heap
			** slot for the remote secondary tag
			*/
			MB_Word	extra = (cons_tag->id == MB_TAG_COMPLICATED)
					? 1 : 0;
			MB_Word *heap_data;

			if (list_length + extra != 0) {
				MB_Unsigned	i;
			
				/* allocate heap memory */
				heap_data = (MB_Word *) MB_GC_NEW_ARRAY(
						MB_Word, list_length + extra);

				/* ensure tag bits aren't used */
				assert(MB_tag((MB_Word) heap_data) == 0);

				/* copy variables to allocated heap block */
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
			/* Simple tag with no body */
			assert(list_length == 0);
			val = MB_mkword(MB_mktag(cons_tag->opt.enum_tag),
					MB_mkbody(0));
			break;

		case MB_TAG_NONE:
			assert(list_length == 1);
			val = (MB_Word *) MB_var_get(ms, var_list[0]);
			break;
			MB_fatal("tag_none not done");
		default:
			MB_fatal("Unknown tag type in construct");
	}
	return (MB_Word) val;
}

static void
instr_construct(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
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
			MB_fatal("Construct float not implemented");
			
		case MB_CONSID_PRED_CONST: {
			/* XXX Closure layouts not done */

			int i;
			MB_Word		num_hidden_args;
			MR_Closure	*closure;
			MB_Short	*var_list;
			MB_Cons_id	*consid = &(bca->construct.consid);

			/*MB_util_error("Closure layouts not implemented");*/

			if (consid->opt.pred_const.native_addr == NULL) {
				consid->opt.pred_const.native_addr =
					MB_code_find_proc_native(
						consid->opt.pred_const.
								module_name,
						consid->opt.pred_const.
								pred_name,
						consid->opt.pred_const.mode_num,
						consid->opt.pred_const.arity,
						consid->opt.pred_const.is_func
					);

				if (consid->opt.pred_const.native_addr == NULL){
					MB_util_error("%s %s__%s/%d (%d)",
						consid->opt.pred_const.is_func
							? "func" : "pred",
						consid->opt.pred_const.
								module_name,
						consid->opt.pred_const.
								pred_name,
						consid->opt.pred_const.arity,
						consid->opt.pred_const.mode_num
						);
					MB_fatal("Unable to find closure code");
				}
			}

			/* Create a closure */
			num_hidden_args = bca->construct.list_length;
			var_list = bca->construct.var_list;

			/* Fill in the closure */

			closure = (MR_Closure *) MB_GC_malloc(
				offsetof(MR_Closure, MR_closure_hidden_args_0)
					+ sizeof(MR_Word) * num_hidden_args);
			closure->MR_closure_layout = NULL;
			closure->MR_closure_code =
				consid->opt.pred_const.native_addr;
			closure->MR_closure_num_hidden_args = num_hidden_args;

			/* Copy the hidden arguments */
			for (i = 0; i < num_hidden_args; i++) {
				closure->MR_closure_hidden_args(i+1) =
					MB_var_get(ms, var_list[i]);
			}

			val = (MB_Word) closure;
			break;
		}
			
		case MB_CONSID_CODE_ADDR_CONST:
			MB_fatal("Construct code_addr not implemented");

		case MB_CONSID_BASE_TYPE_INFO_CONST: {

			MB_Cons_id	*consid = &(bca->construct.consid);

			if (consid->opt.base_type_info_const.type_info == NULL) {
				consid->opt.base_type_info_const.type_info =
					MB_type_find_ctor_info_guaranteed(
						consid->opt.base_type_info_const
							.module_name,
						consid->opt.base_type_info_const
							.type_name,
						consid->opt.base_type_info_const
							.type_arity);
			}

			val = (MB_Word) consid->opt.base_type_info_const
								.type_info;
			break;
		}

		case MB_CONSID_CHAR_CONST:
			val = (MB_Word) bca->construct.consid.opt.char_const.ch;
			break;

		default:
			MB_fatal("Unknown constructor id");
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
			MB_fatal("Deconstruct type not implemented");
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
		case MB_TAG_SIMPLE: 	/* only need a primary tag */
		case MB_TAG_COMPLICATED:/* need primary + remote 2ndary tag */
		{
			/*
			** The code for these two is virtually identical except
			** that if it is complicated we need one extra heap
			** slot for the remote secondary tag
			*/
			MB_Word	extra = (cons_tag->id == MB_TAG_COMPLICATED)
				? 1 : 0;
			MB_Word *heap_data = (MB_Word *) MB_strip_tag(val);
			MB_Word i;

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
			

			/* Deconstruct into variable slots */
			for (i = 0; i < list_length; i++) {
				MB_var_set(ms, var_list[i],
						heap_data[i + extra]);
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
			MB_fatal("tag_none not done");

		default:
			MB_fatal("Unknown deconstruct tag");
	}

	return TRUE;
}


static void
instr_deconstruct(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
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
instr_place(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
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
instr_pickup(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
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

/* Calls a native code procedure and sets up reentry variables */
static void
call_native_proc(MB_Machine_State *ms, MB_Native_Addr native_addr,
	       				MB_Bytecode_Addr return_ip)
{
	if (MB_proc_is_det(ms)) {
		/*
		** Call native code from det function
		*/

		/* Push a interface det stack frame */
		MB_incr_sp(MB_DETFRAME_INTERFACE_SIZE);

		/* Set success ip to reentry stub */
		MB_succip = MB_native_get_return_det();

		/* Save initial stack frame pointer */
		MB_stackitem(MB_DETFRAME_INTERFACE_BCINITFR)
			= (MB_Word) MB_initialstackframe_get(ms);

		/* Save bytecode reentry point */
		MB_stackitem(MB_DETFRAME_INTERFACE_BCRETIP)
			= (MB_Word) return_ip;

	} else {
		/* 
		** Call native code from nondet function
		*/

		/* Set success ip to reentry point */
		MB_succip = MB_native_get_return_nondet();

		/* Save bytecode reentry point */
		MB_fr_bcretip(MB_curfr) = (MB_Word) return_ip;
	}

	/* return to native code at address new_addr */
	MB_native_return_set(ms, native_addr);
}

static void
instr_call(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
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
		
		/* Make sure the address has been looked up */
		if (new_addr == NULL) {
			new_addr = MB_code_find_proc_native(
					bca->call.module_name,
					bca->call.pred_name,
					bca->call.mode_num,
					bca->call.arity,
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

		call_native_proc(ms, new_addr, next_ip);
	}
}

/*
** Why does the call need to know the number of output arguments ???
**
** XXX: If semidet, do I need to make space for the extra argument or has
** the mercury compiler already done that ???
*/
static void
instr_higher_order_call(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	/* We are doing the call */
	MR_Closure *closure = (MR_Closure *)
		MB_var_get(ms, bca->higher_order_call.pred_var);

	/*
	** Shift the input arguments to the right so we can insert the
	** arguments inside the closure
	*/
	if (bca->higher_order_call.in_var_count != 0) {
		signed int i = closure->MR_closure_num_hidden_args;
		signed int j = i + bca->higher_order_call.in_var_count;
		for (; i >= 1; i--, j--) {
			MB_reg(j) = MB_reg(i);
		}
	}

	/*
	** Now insert the hidden arguments
	*/
	if (closure->MR_closure_num_hidden_args) {
		signed int i;
		MB_Word num_hidden_args =
			closure->MR_closure_num_hidden_args;

		for (i = 1; i <= num_hidden_args; i++) {
			MB_reg(i) = closure->MR_closure_hidden_args(i);
		}
	}
	
	/*
	** Do the actual call
	*/

	call_native_proc(ms, closure->MR_closure_code, MB_ip_get(ms) + 1);

}
/*----------------------------------------------------------------------------*/

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
instr_builtin_binop(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
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
instr_builtin_bintest(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
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
}
/*----------------------------------------------------------------------------*/
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
	unop_bad, 		/* mkbody */
	unop_bad, 		/* unmkbody */
	unop_bad,		/* strip_tag  */
	unop_bad,		/* hash_string */
	unop_bitwise_complement,
	unop_not
};

#define SIMPLEUNOP(name, op)	\
	static MB_Word \
	unop_##name(MB_Machine_State *ms, const MB_Bytecode_Arg *bca) \
	{ \
		return op MB_var_get(ms, bca->builtin_unop.arg.opt.var); \
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
instr_builtin_unop(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	MB_Byte unop = bca->builtin_unop.unop;
	
	if (unop < (sizeof(unop_table)/sizeof(unop_table[0]))) {

		MB_var_set(ms, bca->builtin_unop.to_var,
		       unop_table[bca->builtin_unop.unop](ms, bca));

		instr_noop(ms, NULL);
	} else {
		MB_fatal("Invalid unop");
	}
}


static void
instr_builtin_untest(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	MB_fatal("builtin_untest not done");
}

/*----------------------------------------------------------------------------*/
static void
instr_semidet_success(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	MB_stackitem(MB_DETFRAME_SEMIDET_SUCCESS) = MB_SEMIDET_SUCCESS;

	instr_noop(ms, NULL);
}

static void
instr_semidet_success_check(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	if (MB_reg(MB_SEMIDET_SUCCESS_REG) != MB_SEMIDET_SUCCESS) {
		instr_do_redo(ms, NULL);
	} else {
		instr_noop(ms, NULL);
	}
}

static void
instr_do_redo(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	/*
	** XXX: redo to bytecode could be sped up by going directly to the
	** right location instead of jumping back into native code first
	*/

	/* return to native code at address new_addr */
	MB_native_return_set(ms, MB_native_get_do_redo());
}

static void
instr_do_fail(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	/*
	** XXX: fail to bytecode could be sped up by going directly to the
	** right location instead of jumping back into native code first
	*/

	/* return to native code at address new_addr */
	MB_native_return_set(ms, MB_native_get_do_fail());
}

static void
instr_noop(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	/* increment instruction pointer */
	MB_ip_set(ms, MB_ip_get(ms) + 1);
}

static void
instr_notdone(MB_Machine_State *ms, MB_Bytecode_Arg *bca)
{
	MB_fatal("Instruction type not (yet) completed");

	/* invalid instruction */
	instr_noop(ms, NULL);
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

/*----------------------------------------------------------------------------*/
/*
** XXX: This is debugging code *only*. It won't work in threads, and
** is rather fragile. Don't push it too hard, and it will be kind to you.
*/

#define DODEBUG		1

#include "mb_disasm.h"

static void
MB_machine_debug(MB_Machine_State* ms) {


	static MB_Bytecode_Addr stopat =
	#if DODEBUG
		NULL;
	#else
		(MB_Bytecode_Addr) MB_CODE_INVALID_ADR;
	#endif
	static int line_len = 80;

	static MB_Word count = 0;
	static MB_Word stopcount = 0;
	char buffer[60];
	MB_Bytecode_Addr cur_ip = MB_ip_get(ms);

	count++;

	if (count == stopcount) {
		stopat = cur_ip;
	}

	if (stopat != NULL && stopat != cur_ip) return;

	if (cur_ip == MB_CODE_NATIVE_RETURN) {
		stopat = NULL;
		return;
	}

	MB_show_state(ms, stderr);

	MB_SAY("exec count %d", count);

Reread:
	fgets(buffer, sizeof(buffer), stdin);

	switch (buffer[0]) {
		case 0:
		case '\n':
		case 's': /* step */
			stopat = NULL;
			break;
		case 'c': /* instruction count */
			if (sscanf(buffer, "c %d", &stopcount)
				== 1)
			{
				stopat = MB_CODE_INVALID_ADR;
			}
			break;
		case 'd': /* code dump */
			MB_listing(ms, stderr,
				(MB_Bytecode_Addr) NULL,
				(MB_Bytecode_Addr) ((MB_Word)
						    (((char *) NULL - 1)) / 3),
				line_len);
			goto Reread;
		case '-':
			line_len *= 3;
			line_len /= 4;
			goto Reread;
		case '+':
			line_len *= 4;
			line_len /= 3;
			goto Reread;
		case 'l': { /* code listing */
			MB_Bytecode_Addr start;	
			MB_Bytecode_Addr end;	
			switch (sscanf(buffer, "l %p %p", &start, &end)) {
				case EOF:
				case 0:
					start = MB_ip_get(ms);
				case 1:
					start -= 12;
					end = start + 48;
					break;
				case 2:
					break;
			}
			MB_listing(ms, stderr, start, end,
					line_len);
			goto Reread;
		}
		case 'S': /* machine state */
			MB_show_state(ms, stderr);
			goto Reread;
		case 'n': /* next */
			stopat = cur_ip + 1;
			break;
		case 'r': /* run */
			stopat = MB_CODE_INVALID_ADR;
			break;
		case 'e': /* run to reentry */
			stopat = MB_CODE_NATIVE_RETURN;
			break;
		case 'R': /* to return */
			do {
				cur_ip++;
			} while (MB_code_get_id(cur_ip) != MB_BC_endof_proc);
			stopat = cur_ip;
			break;
		case 'x':
			exit(0);
		case '?':
			MB_SAY(
			"s    - step\n"
			"n    - next (step over)\n"
			"r    - run to end\n"
			"c $1 - run to Count $1\n"
			"e    - run to reEntry\n"
			"R    - run to immediate return\n"
			"x    - exit"
			);
			goto Reread;
		case 'v': /* view [data] */
			switch (buffer[1]) {
			case 's': {
				char *strbuf = NULL;
				sscanf(buffer, "vs %p", &strbuf);
				MB_SAY("String at %p: %s", strbuf, strbuf);
				break;
			}
			case 'l': {
				MB_Word list_ptr;
				MB_Word tag;
				sscanf(buffer, "vl %p", &list_ptr);
				MB_SAY("List at %p: [", list_ptr);

				tag = MB_tag(list_ptr);
				while (tag != 0) {
					MB_SAY("\t%08x",
						MB_field(tag, list_ptr, 0));
					list_ptr = MB_field(tag, list_ptr, 1);

					tag = MB_tag(list_ptr);
				}
				MB_SAY("]\n");
				break;
			}
			default:
				MB_SAY("Unknown data type");
			}
			goto Reread;
		default:
			MB_SAY("Unknown command");
			goto Reread;
	}
	MB_SAY("Will stop at %p", stopat);
}
/*----------------------------------------------------------------------------*/
#include "mb_machine_def.h"	/* reqd to instantiate MB_Machine_State */
MB_Native_Addr
MB_machine_exec(MB_Bytecode_Addr new_ip, MB_Word *initial_stack)
{
	/* Create Machine State */
	MB_Machine_State ms;
	ms.ip = new_ip;
	ms.initial_stack = initial_stack;
	MB_proc_var_init(&ms);

	do {
		MB_Bytecode_Addr ip; 

		MB_machine_debug(&ms);

		ip = MB_ip_get(&ms);
		if (MB_ip_normal(ip)) {

			MB_Byte bc_id = MB_code_get_id(ip);


			if (!dispatch(bc_id, &ms)) {
				switch (bc_id) {
					case MB_BC_debug_trap:
						return 0;
				}
				MB_util_error("Attempt to execute"
					" invalid instruction\n");
				instr_noop(&ms, NULL);
				return 0;
			}
		} else {
			switch ((MB_Word) ip) {
				case (MB_Word) MB_CODE_DO_FAIL:
					instr_do_fail(&ms, NULL);
					break;

				case (MB_Word) MB_CODE_DO_REDO:
					instr_do_redo(&ms, NULL);
					break;

				case (MB_Word) MB_CODE_NATIVE_RETURN:
					return MB_native_return_get(&ms);
				default:
					MB_util_error("At address %p:", ip);
					MB_fatal("Attempt to execute invalid"
						" address\n");
			}
		}
	} while (1);

	assert(FALSE);
	return NULL;
}


