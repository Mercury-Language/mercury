/*
** Copyright (C) 1997-1998 The University of Melbourne.
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
#include <unistd.h>		/* for the write system call */

static	MR_Trace_Cmd_Info	MR_trace_ctrl = { MR_CMD_GOTO, 0, 0,
					MR_PRINT_LEVEL_SOME, FALSE };

Code 		*MR_trace_real(const MR_Stack_Layout_Label *layout,
			MR_Trace_Port port, Unsigned seqno, Unsigned depth,
			const char *path, int max_r_num);

static	Code	*MR_trace_event(MR_Trace_Cmd_Info *cmd, bool interactive,
			const MR_Stack_Layout_Label *layout,
			MR_Trace_Port port, Unsigned seqno, Unsigned depth,
			const char *path, int max_r_num);

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
MR_trace_real(const MR_Stack_Layout_Label *layout, MR_Trace_Port port,
	Unsigned seqno, Unsigned depth, const char *path, int max_r_num)
{
	MR_Spy_Action	action;
	bool		match;

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
			if (MR_trace_ctrl.MR_trace_stop_depth == depth
					&& MR_port_is_final(port))
			{
				return MR_trace_event(&MR_trace_ctrl, TRUE,
						layout, port, seqno, depth,
						path, max_r_num);
			} else {
				goto check_stop_print;
			}

		case MR_CMD_GOTO:
			if (MR_trace_event_number >=
					MR_trace_ctrl.MR_trace_stop_event)
			{
				return MR_trace_event(&MR_trace_ctrl, TRUE,
						layout, port, seqno, depth,
						path, max_r_num);
			} else {
				goto check_stop_print;
			}

		case MR_CMD_RESUME_FORWARD:
			if (port != MR_PORT_REDO &&
			    port != MR_PORT_FAIL &&
			    port != MR_PORT_EXCEPTION)
			{
				return MR_trace_event(&MR_trace_ctrl, TRUE,
						layout, port, seqno, depth,
						path, max_r_num);
			} else {
				goto check_stop_print;
			}

		case MR_CMD_RETURN:
			if (port != MR_PORT_EXIT) {
				return MR_trace_event(&MR_trace_ctrl, TRUE,
						layout, port, seqno, depth,
						path, max_r_num);
			} else {
				goto check_stop_print;
			}

		case MR_CMD_MIN_DEPTH:
			if (MR_trace_ctrl.MR_trace_stop_depth <= depth) {
				return MR_trace_event(&MR_trace_ctrl, TRUE,
						layout, port, seqno, depth,
						path, max_r_num);
			} else {
				goto check_stop_print;
			}

		case MR_CMD_MAX_DEPTH:
			if (MR_trace_ctrl.MR_trace_stop_depth >= depth) {
				return MR_trace_event(&MR_trace_ctrl, TRUE,
						layout, port, seqno, depth,
						path, max_r_num);
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

		match = MR_event_matches_spy_point(layout, port, &action);
		if (! match) {
			if (MR_trace_ctrl.MR_trace_print_level ==
					MR_PRINT_LEVEL_ALL)
			{
				return MR_trace_event(&MR_trace_ctrl, FALSE,
						layout, port, seqno, depth,
						path, max_r_num);
			}

			return NULL;
		}

		if ((! MR_trace_ctrl.MR_trace_strict)
				&& action == MR_SPY_STOP)
		{
			return MR_trace_event(&MR_trace_ctrl, TRUE,
					layout, port, seqno, depth,
					path, max_r_num);
		}

		if (MR_trace_ctrl.MR_trace_print_level != MR_PRINT_LEVEL_NONE) {
			/*
			** It doesn't matter whether action is MR_SPY_STOP or
			** MR_SPY_PRINT; even if it is MR_SPY_STOP, we want
			** to print it if printing is allowed and we did not
			** stop.
			*/

			return MR_trace_event(&MR_trace_ctrl, FALSE,
					layout, port, seqno, depth,
					path, max_r_num);
		}
	}

	return NULL;
}

static Code *
MR_trace_event(MR_Trace_Cmd_Info *cmd, bool interactive,
	const MR_Stack_Layout_Label *layout, MR_Trace_Port port,
	Unsigned seqno, Unsigned depth, const char *path, int max_r_num)
{
	int	max_mr_num;
	Code	*jumpaddr;
	Word	saved_regs[MAX_FAKE_REG];

	if (max_r_num + MR_NUM_SPECIAL_REG > MR_MAX_SPECIAL_REG_MR) {
		max_mr_num = max_r_num + MR_NUM_SPECIAL_REG;
	} else {
		max_mr_num = MR_MAX_SPECIAL_REG_MR;
	}

	/* This also saves the regs in MR_fake_regs. */
	MR_copy_regs_to_saved_regs(max_mr_num, saved_regs);

#ifdef MR_USE_EXTERNAL_DEBUGGER
	if (MR_trace_handler == MR_TRACE_EXTERNAL) {
		if (!interactive) {
			fatal_error("reporting event for external debugger");
		}

		MR_trace_event_external(cmd, layout, saved_regs,
			port, seqno, depth, path);
		jumpaddr = NULL;
	} else {
		jumpaddr = MR_trace_event_internal(cmd, interactive,
				layout, saved_regs, port, seqno,
				depth, path, &max_mr_num);
	}
#else
	/*
	** We should get here only if MR_trace_handler == MR_TRACE_INTERNAL.
	** This is enforced by mercury_wrapper.c.
	*/

	jumpaddr = MR_trace_event_internal(cmd, interactive,
			layout, saved_regs, port, seqno,
			depth, path, &max_mr_num);
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
	MR_copy_saved_regs_to_regs(max_mr_num, saved_regs);
	return jumpaddr;
}
