/*
** Copyright (C) 1997-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_trace.c - implements the top levels of the tracing subsystem,
** which interface to the automatically generated code in user programs.
** The trace events can be processed either by our internal debugger,
** or by an external trace-analysis style debugger. These are in
** mercury_trace_internal.c and mercury_trace_external.c respectively.
**
** After MR_trace has determined that the event needs to be processed,
** it saves all the Mercury virtual machine registers that are live at the
** call site in both the MR_fake_regs array and a separate saved_regs array.
**
** Any debugger code that wants to inspect the contents of the registers
** at the time of the call must look at the saved_regs array, which is
** passed around to all the functions that need it.
**
** Any Mercury code invoked from the debugger may update the real registers
** and the MR_fake_reg array. This includes both the general purpose registers
** and the control registers. Just before we return control to the Mercury
** program being debugged, we copy the contents of the saved_regs array back
** into the machine registers, thus overwriting the changes made by all the
** Mercury predicates we have called. There are two exceptions to this. One,
** we keep any changes to the MR_global_hp register. This is necessary to
** ensure that data structures that we want to keep permanently (e.g. help
** system nodes added by debugger commands) are not overwritten by later code.
** Two, when the debugger executes a retry command, it restores the state
** of the machine registers to a state that is appropriate for the call site.
**
** Main author: Zoltan Somogyi.
*/

#include "mercury_imp.h"
#include "mercury_trace.h"
#include "mercury_trace_internal.h"
#include "mercury_trace_external.h"
#include "mercury_trace_spy.h"
#include "mercury_layout_util.h"
#include "mercury_memory.h"
#include "mercury_engine.h"
#include "mercury_wrapper.h"
#include "mercury_misc.h"
#include "mercury_array_macros.h"
#include <stdio.h>

static	MR_Trace_Cmd_Info	MR_trace_ctrl = {
	MR_CMD_GOTO,
	0,	/* stop depth */
	0,	/* stop event */
	MR_PRINT_LEVEL_SOME,
	FALSE,	/* not strict */
	TRUE	/* must check */
};

Code 		*MR_trace_real(const MR_Stack_Layout_Label *layout);
static	Code	*MR_trace_event(MR_Trace_Cmd_Info *cmd, bool interactive,
			const MR_Stack_Layout_Label *layout,
			MR_Trace_Port port, Unsigned seqno, Unsigned depth);
static	Word	MR_trace_find_input_arg(const MR_Stack_Layout_Label *label, 
			Word *saved_regs, MR_uint_least16_t var_num,
			bool *succeeded);

/*
** Reserve room for event counts for this many depths initially.
*/

#define	INIT_HISTOGRAM	128

/*
** MR_trace_real() is called via a function pointer from MR_trace()
** in runtime/mercury_trace_base.c, which in turn is called from
** compiled code whenever an event to be traced occurs.
*/

Code *
MR_trace_real(const MR_Stack_Layout_Label *layout)
{
	Integer		maybe_from_full;
	Unsigned	seqno;
	Unsigned	depth;
	MR_Spy_Action	action;
	bool		match;
	MR_Trace_Port	port;

	/* in case MR_sp or MR_curfr is transient */
	restore_transient_registers();

	maybe_from_full = layout->MR_sll_entry->MR_sle_maybe_from_full;
	if (MR_DETISM_DET_STACK(layout->MR_sll_entry->MR_sle_detism)) {
		if (maybe_from_full > 0 && ! MR_stackvar(maybe_from_full)) {
			return NULL;
		}

		seqno = (Unsigned) MR_call_num_stackvar(MR_sp);
		depth = (Unsigned) MR_call_depth_stackvar(MR_sp);
	} else {
		if (maybe_from_full > 0 && ! MR_framevar(maybe_from_full)) {
			return NULL;
		}

		seqno = (Unsigned) MR_call_num_framevar(MR_curfr);
		depth = (Unsigned) MR_call_depth_framevar(MR_curfr);
	}

	MR_trace_event_number++;

#ifdef	MR_TRACE_HISTOGRAM

	/*
	** The depths do not necessarily increase one-by-one, since
	** a procedure from a fully traced module can be called from
	** deep within interface traced code.
	*/

	MR_ensure_big_enough2(depth, MR_trace_histogram, _all, _exp,
		int, INIT_HISTOGRAM);

	if (depth > MR_trace_histogram_hwm) {
		MR_trace_histogram_hwm = depth;
	}

	MR_trace_histogram_all[depth]++;
	MR_trace_histogram_exp[depth]++;

#endif	/* MR_TRACE_HISTOGRAM */

	switch (MR_trace_ctrl.MR_trace_cmd) {
		case MR_CMD_FINISH:
			if (MR_trace_ctrl.MR_trace_stop_depth != depth) {
				goto check_stop_print;
			} else {
				port = (MR_Trace_Port) layout->MR_sll_port;

				if (! MR_port_is_final(port)) {
					goto check_stop_print;
				} else {
					return MR_trace_event(&MR_trace_ctrl,
						TRUE, layout, port,
						seqno, depth);
				}
			}

		case MR_CMD_GOTO:
			if (MR_trace_event_number >=
					MR_trace_ctrl.MR_trace_stop_event)
			{
				port = (MR_Trace_Port) layout->MR_sll_port;
				return MR_trace_event(&MR_trace_ctrl, TRUE,
						layout, port, seqno, depth);
			} else {
				goto check_stop_print;
			}

		case MR_CMD_RESUME_FORWARD:
			port = (MR_Trace_Port) layout->MR_sll_port;
			if (port != MR_PORT_REDO &&
			    port != MR_PORT_FAIL &&
			    port != MR_PORT_EXCEPTION)
			{
				return MR_trace_event(&MR_trace_ctrl, TRUE,
						layout, port, seqno, depth);
			} else {
				goto check_stop_print;
			}

		case MR_CMD_RETURN:
			port = (MR_Trace_Port) layout->MR_sll_port;
			if (port != MR_PORT_EXIT) {
				return MR_trace_event(&MR_trace_ctrl, TRUE,
						layout, port, seqno, depth);
			} else {
				goto check_stop_print;
			}

		case MR_CMD_MIN_DEPTH:
			if (MR_trace_ctrl.MR_trace_stop_depth <= depth) {
				port = (MR_Trace_Port) layout->MR_sll_port;
				return MR_trace_event(&MR_trace_ctrl, TRUE,
						layout, port, seqno, depth);
			} else {
				goto check_stop_print;
			}

		case MR_CMD_MAX_DEPTH:
			if (MR_trace_ctrl.MR_trace_stop_depth >= depth) {
				port = (MR_Trace_Port) layout->MR_sll_port;
				return MR_trace_event(&MR_trace_ctrl, TRUE,
						layout, port, seqno, depth);
			} else {
				goto check_stop_print;
			}

		case MR_CMD_TO_END:
			goto check_stop_print;

		default:
			fatal_error("invalid command in MR_trace");
	}

check_stop_print:

	if (MR_trace_ctrl.MR_trace_must_check) {
		/*
		** The value of MR_trace_ctrl.MR_trace_must_check was
		** precomputed when the command was set up; it was set to TRUE
		** iff either MR_trace_ctrl.MR_trace_strict is FALSE (allowing
		** us to stop at breakpoints whose action is MR_SPY_STOP) or
		** MR_trace_ctrl.MR_trace_print_level is something other than
		** MR_PRINT_LEVEL_NONE (allowing us to print at least some
		** events). The precomputation avoids several jumps in the
		** very frequent case that MR_trace_must_check is false.
		*/

		port = (MR_Trace_Port) layout->MR_sll_port;
		match = MR_event_matches_spy_point(layout, port, &action);
		if (! match) {
			if (MR_trace_ctrl.MR_trace_print_level ==
					MR_PRINT_LEVEL_ALL)
			{
				return MR_trace_event(&MR_trace_ctrl, FALSE,
						layout, port, seqno, depth);
			}

			return NULL;
		}

		if ((! MR_trace_ctrl.MR_trace_strict) && action == MR_SPY_STOP)
		{
			return MR_trace_event(&MR_trace_ctrl, TRUE,
					layout, port, seqno, depth);
		}

		if (MR_trace_ctrl.MR_trace_print_level != MR_PRINT_LEVEL_NONE)
		{
			/*
			** It doesn't matter whether action is MR_SPY_STOP or
			** MR_SPY_PRINT; even if it is MR_SPY_STOP, we want
			** to print it if printing is allowed and we did not
			** stop.
			*/

			return MR_trace_event(&MR_trace_ctrl, FALSE,
					layout, port, seqno, depth);
		}
	}

	return NULL;
}

static Code *
MR_trace_event(MR_Trace_Cmd_Info *cmd, bool interactive,
	const MR_Stack_Layout_Label *layout, MR_Trace_Port port,
	Unsigned seqno, Unsigned depth)
{
	Code		*jumpaddr;
	MR_Event_Info	event_info;
	Word		*saved_regs = event_info.MR_saved_regs;
	int		max_r_num;

	event_info.MR_event_number = MR_trace_event_number;
	event_info.MR_call_seqno = seqno;
	event_info.MR_call_depth = depth;
	event_info.MR_trace_port = port;
	event_info.MR_event_sll = layout;
	event_info.MR_event_path = layout->MR_sll_entry->MR_sle_module_layout
			->MR_ml_string_table + layout->MR_sll_goal_path;

	max_r_num = layout->MR_sll_entry->MR_sle_max_r_num;
	if (max_r_num + MR_NUM_SPECIAL_REG > MR_MAX_SPECIAL_REG_MR) {
		event_info.MR_max_mr_num = max_r_num + MR_NUM_SPECIAL_REG;
	} else {
		event_info.MR_max_mr_num = MR_MAX_SPECIAL_REG_MR;
	}

	/* This also saves the regs in MR_fake_regs. */
	MR_copy_regs_to_saved_regs(event_info.MR_max_mr_num, saved_regs);

#ifdef MR_USE_EXTERNAL_DEBUGGER
	if (MR_trace_handler == MR_TRACE_EXTERNAL) {
		if (!interactive) {
			fatal_error("reporting event for external debugger");
		}

		jumpaddr = MR_trace_event_external(cmd, &event_info);
	} else {
		jumpaddr = MR_trace_event_internal(cmd, interactive,
				&event_info);
	}
#else
	/*
	** We should get here only if MR_trace_handler == MR_TRACE_INTERNAL.
	** This is enforced by mercury_wrapper.c.
	*/

	jumpaddr = MR_trace_event_internal(cmd, interactive, &event_info);
#endif

	/*
	** Whenever the debugger changes the flow of control, e.g. by
	** executing a retry command, it also sets up the saved registers
	** to the state that is appropriate for the label at which execution
	** will resume. There may be more registers live at that point than
	** at the point at which MR_trace was called. Therefore max_mr_num
	** should also be set appropriately when executing a retry command.
	**
	** For the treatment of MR_global_hp, see the top of this file.
	*/

	restore_transient_registers(); /* in case MR_global_hp is transient */
	MR_saved_global_hp(saved_regs) = MR_global_hp;
	MR_copy_saved_regs_to_regs(event_info.MR_max_mr_num, saved_regs);
	return jumpaddr;
}

const char *
MR_trace_retry(MR_Event_Info *event_info, MR_Event_Details *event_details,
	Code **jumpaddr)
{
	const MR_Stack_Layout_Entry	*entry;
	const MR_Stack_Layout_Label	*call_label;
	const MR_Stack_Layout_Vars	*input_args;
	Word				*args;
	int				arg_max;
	int				arg_num;
	Word				arg_value;
	int				i;
	bool				succeeded;
	const char			*message;
	Word 				*saved_regs;

	saved_regs = event_info->MR_saved_regs;
	entry = event_info->MR_event_sll->MR_sll_entry;
	if (!MR_ENTRY_LAYOUT_HAS_EXEC_TRACE(entry)) {
		message = "Cannot perform retry, because this procedure "
			"was not compiled with\nexecution tracing enabled.";
		return message;
	}

	call_label = entry->MR_sle_call_label;
	input_args = &call_label->MR_sll_var_info;
	if (input_args->MR_slvs_var_count < 0) {
		message = "Cannot perform retry because information about "
		          "the input arguments is not available.";
		return message;
	}

	/*
	** With the Boehm collector, args need not be considered a root, 
	** since its contents are just copies of values from elsewhere,
	** With the native collector, it need not be considered a root
	** because its lifetime spans only this function, in which
	** no native garbage collection can be triggered.
	*/

	args = NULL;
	arg_max = 0;

	for (i = 0; i < MR_all_desc_var_count(input_args); i++) {
		arg_value = MR_trace_find_input_arg(event_info->MR_event_sll,
				saved_regs,
				input_args->MR_slvs_names[i].MR_var_num,
				&succeeded);

		if (! succeeded) {
			message = "Cannot perform retry because the values of "
				  "some input arguments are missing.";
			return message;
		}

		if (i < MR_long_desc_var_count(input_args)) {
			arg_num = MR_get_register_number_long(
				MR_long_desc_var_locn(input_args, i));
		} else {
			arg_num = MR_get_register_number_short(
				MR_short_desc_var_locn(input_args, i));
		}

		if (arg_num > 0) {
			MR_ensure_big_enough(arg_num, arg, Word,
				MR_INIT_ARG_COUNT);
			args[arg_num] = arg_value;
		} else {
			fatal_error("illegal location for input argument");
		}
	}

	MR_trace_call_seqno = event_info->MR_call_seqno - 1;
	MR_trace_call_depth = event_info->MR_call_depth - 1;

	if (MR_DETISM_DET_STACK(entry->MR_sle_detism)) {
		MR_Long_Lval	location;
		Word		*this_frame;

		/*
		** We are at a final port, so both curfr and maxfr
		** must already have been reset to their original values.
		** We only need to set up the succip register for the "call",
		** and then remove this frame from the det stack.
		*/

		location = entry->MR_sle_succip_locn;
		if (MR_LONG_LVAL_TYPE(location) != MR_LONG_LVAL_TYPE_STACKVAR)
		{
			fatal_error("illegal location for stored succip");
		}

		this_frame = MR_saved_sp(saved_regs);
		MR_saved_succip(saved_regs) = (Word *)
				MR_based_stackvar(this_frame,
				MR_LONG_LVAL_NUMBER(location));
		MR_saved_sp(saved_regs) -= entry->MR_sle_stack_slots;
		MR_trace_event_number = MR_event_num_stackvar(this_frame);

#ifdef	MR_USE_TRAIL
		if (entry->MR_sle_maybe_trail >= 0) {
			Word	ticket_counter;
			Word	trail_ptr;

			trail_ptr = MR_based_stackvar(this_frame,
					entry->MR_sle_maybe_trail);
			ticket_counter = MR_based_stackvar(this_frame,
					entry->MR_sle_maybe_trail+1);
			MR_reset_ticket(trail_ptr, MR_retry);
			MR_discard_tickets_to(ticket_counter);
		} else {
			fatal_error("retry cannot restore the trail");
		}
#endif
	} else {
		Word	*this_frame;

		/*
		** We are at a final port, so sp must already have been reset
		** to its original value. We only need to set up the succip
		** and curfr registers for the "call", and remove this frame,
		** and any other frames above it, from the nondet stack.
		*/

		this_frame = MR_saved_curfr(saved_regs);

		MR_saved_succip(saved_regs) = MR_succip_slot(this_frame);
		MR_saved_curfr(saved_regs) = MR_succfr_slot(this_frame);
		MR_saved_maxfr(saved_regs) = MR_prevfr_slot(this_frame);
		MR_trace_event_number = MR_event_num_framevar(this_frame);

#ifdef	MR_USE_TRAIL
		if (entry->MR_sle_maybe_trail >= 0) {
			Word	ticket_counter;
			Word	trail_ptr;

			trail_ptr = MR_based_framevar(this_frame,
					entry->MR_sle_maybe_trail);
			ticket_counter = MR_based_framevar(this_frame,
					entry->MR_sle_maybe_trail+1);
			MR_reset_ticket(trail_ptr, MR_retry);
			MR_discard_tickets_to(ticket_counter);
		} else {
			fatal_error("retry cannot restore the trail");
		}
#endif
	}

	for (i = 1; i < arg_max; i++) {
		saved_reg(saved_regs, i) = args[i];
	}

	if (args != NULL) {
		free(args);
	}

	event_info->MR_max_mr_num = max(event_info->MR_max_mr_num, arg_max);
	*jumpaddr = entry->MR_sle_code_addr;

	/*
	** Overriding MR_trace_call_seqno etc is not enough, because
	** we will restore the values of those variables later. We must
	** also override the saved copies.
	*/

	event_details->MR_call_seqno = MR_trace_call_seqno;
	event_details->MR_call_depth = MR_trace_call_depth;
	event_details->MR_event_number = MR_trace_event_number;

	return NULL;
}


static Word
MR_trace_find_input_arg(const MR_Stack_Layout_Label *label, Word *saved_regs,
	MR_uint_least16_t var_num, bool *succeeded)
{
	const MR_Stack_Layout_Vars	*vars;
	int				i;

	vars = &label->MR_sll_var_info;
	if (vars->MR_slvs_names == NULL) {
		*succeeded = FALSE;
		return 0;
	}

	for (i = 0; i < MR_all_desc_var_count(vars); i++) {
		if (var_num == vars->MR_slvs_names[i].MR_var_num) {
			if (i < MR_long_desc_var_count(vars)) {
				return MR_lookup_long_lval_base(
					MR_long_desc_var_locn(vars, i),
					saved_regs, MR_saved_sp(saved_regs),
					MR_saved_curfr(saved_regs), succeeded);
			} else {
				return MR_lookup_short_lval_base(
					MR_short_desc_var_locn(vars, i),
					saved_regs, MR_saved_sp(saved_regs),
				MR_saved_curfr(saved_regs), succeeded);
			}
		}
	}

	*succeeded = FALSE;
	return 0;
}
