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
** Utility functions usable by both the internal and external debuggers
** are in mercury_trace_util.c.
**
** The C functions in all these modules which use any of the Mercury registers
** expect the transient registers to be in fake_reg, and the others to be in
** their normal homes.
**
** Main author: Zoltan Somogyi.
*/

#include "mercury_imp.h"
#include "mercury_trace.h"
#include "mercury_trace_util.h"
#include "mercury_trace_internal.h"
#include "mercury_trace_external.h"
#include "mercury_engine.h"
#include "mercury_wrapper.h"
#include "mercury_misc.h"
#include <stdio.h>
#include <unistd.h>		/* for the write system call */

static	MR_trace_cmd_info	MR_trace_ctrl = { MR_CMD_GOTO, 0, 0, FALSE };

static	void	MR_trace_event(MR_trace_cmd_info *cmd,
			const MR_Stack_Layout_Label *layout,
			MR_trace_port port, Unsigned seqno, Unsigned depth,
			const char *path, int max_r_num);

static	void	MR_trace_event_report(const MR_Stack_Layout_Label *layout,
			MR_trace_port port, Unsigned seqno, Unsigned depth,
			const char *path, int max_r_num);

/*
** This function is called from compiled code whenever an event to be traced
** occurs.
*/

void
MR_trace(const MR_Stack_Layout_Label *layout, MR_trace_port port,
	Unsigned seqno, Unsigned depth, const char *path, int max_r_num,
	bool trace_this)
{
	if (! (MR_trace_enabled && trace_this))
		return;

	MR_trace_event_number++;

	switch (MR_trace_ctrl.MR_trace_cmd) {
		case MR_CMD_FINISH:
			if (MR_trace_ctrl.MR_trace_stop_seqno == seqno
			&& MR_port_is_final(port)) {
				MR_trace_event(&MR_trace_ctrl, layout,
					port, seqno, depth, path, max_r_num);

			} else if (MR_trace_ctrl.MR_trace_print_intermediate) {
				MR_trace_event_report(layout,
					port, seqno, depth, path, max_r_num);
			}

			break;

		case MR_CMD_GOTO:
			if (MR_trace_event_number >=
				MR_trace_ctrl.MR_trace_stop_event
			|| MR_event_matches_spy_point(layout)) {
				MR_trace_event(&MR_trace_ctrl, layout,
					port, seqno, depth, path, max_r_num);
			} else if (MR_trace_ctrl.MR_trace_print_intermediate) {
				MR_trace_event_report(layout,
					port, seqno, depth, path, max_r_num);
			}

			break;

		case MR_CMD_RESUME_FORWARD:
			if (MR_port_is_final(port)) {
				MR_trace_event(&MR_trace_ctrl, layout,
					port, seqno, depth, path, max_r_num);
			} else if (MR_trace_ctrl.MR_trace_print_intermediate) {
				MR_trace_event_report(layout,
					port, seqno, depth, path, max_r_num);
			}

			break;

		case MR_CMD_TO_END:
			if (MR_event_matches_spy_point(layout)) {
				MR_trace_event(&MR_trace_ctrl, layout,
					port, seqno, depth, path, max_r_num);
			} else if (MR_trace_ctrl.MR_trace_print_intermediate) {
				MR_trace_event_report(layout,
					port, seqno, depth, path, max_r_num);
			}

			break;

		default:
			fatal_error("invalid cmd in MR_trace");
			break;
	}
}

static void
MR_trace_event(MR_trace_cmd_info *cmd,
	const MR_Stack_Layout_Label *layout, MR_trace_port port,
	Unsigned seqno, Unsigned depth, const char *path, int max_r_num)
{
	int	max_mr_num;

	if (max_r_num + MR_NUM_SPECIAL_REG > MR_MAX_SPECIAL_REG_MR) {
		max_mr_num = max_r_num + MR_NUM_SPECIAL_REG;
	} else {
		max_mr_num = MR_MAX_SPECIAL_REG_MR;
	}

	MR_copy_regs_to_saved_regs(max_mr_num);
#ifdef MR_USE_EXTERNAL_DEBUGGER
	if (MR_trace_debugger == MR_TRACE_EXTERNAL) {
		MR_trace_event_external(layout, port, seqno, depth, path);
	} else {
		MR_trace_event_internal(cmd, layout, port, seqno, depth, path);
	}
#else
	/*
	** We should get here only if MR_trace_debugger == MR_TRACE_INTERNAL.
	** This is enforced by mercury_wrapper.c.
	*/

	MR_trace_event_internal(cmd, layout, port, seqno, depth, path);
#endif
	MR_copy_saved_regs_to_regs(max_mr_num);
}

static void
MR_trace_event_report(const MR_Stack_Layout_Label *layout, MR_trace_port port,
	Unsigned seqno, Unsigned depth, const char *path, int max_r_num)
{
#ifdef MR_USE_EXTERNAL_DEBUGGER
	if (MR_trace_debugger == MR_TRACE_EXTERNAL) {
		fatal_abort("trying to report an event to external debugger");
	} else {
		MR_trace_event_internal_report(layout,
			port, seqno, depth, path);
	}
#else
	/*
	** We should get here only if MR_trace_debugger == MR_TRACE_INTERNAL.
	** This is enforced by mercury_wrapper.c.
	*/

	MR_trace_event_internal_report(layout, port, seqno, depth, path);
#endif
}
