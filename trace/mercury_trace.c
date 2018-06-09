// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1997-2009, 2011 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_trace.c - implements the top levels of the tracing subsystem,
// which interface to the automatically generated code in user programs.
// The trace events can be processed either by our internal debugger,
// or by an external trace-analysis style debugger. These are in
// mercury_trace_internal.c and mercury_trace_external.c respectively.
//
// After MR_trace has determined that the event needs to be processed,
// it saves all the Mercury virtual machine registers that are live at the
// call site in both the MR_fake_regs array and a separate saved_regs array.
//
// Any debugger code that wants to inspect the contents of the registers
// at the time of the call must look at the saved_regs array, which is
// passed around to all the functions that need it.
//
// Any Mercury code invoked from the debugger may update the real registers
// and the MR_fake_reg array. This includes both the general purpose registers
// and the control registers. Just before we return control to the Mercury
// program being debugged, we copy the contents of the saved_regs array back
// into the machine registers, thus overwriting the changes made by all the
// Mercury predicates we have called. There are two exceptions to this. One,
// we keep any changes to the MR_global_hp register. This is necessary to
// ensure that data structures that we want to keep permanently (e.g. help
// system nodes added by debugger commands) are not overwritten by later code.
// Two, when the debugger executes a retry command, it restores the state
// of the machine registers to a state that is appropriate for the call site.
//
// Main author: Zoltan Somogyi.

#include "mercury_imp.h"
#include "mercury_layout_util.h"
#include "mercury_memory.h"
#include "mercury_engine.h"
#include "mercury_wrapper.h"
#include "mercury_misc.h"
#include "mercury_array_macros.h"
#include "mercury_init.h"

#include "mercury_trace.h"
#include "mercury_trace_internal.h"
#include "mercury_trace_external.h"
#include "mercury_trace_declarative.h"
#include "mercury_trace_spy.h"
#include "mercury_trace_util.h"
#include "mercury_trace_vars.h"

#include <stdio.h>

MR_TraceCmdInfo MR_trace_ctrl = {
    MR_CMD_STEP,
    0,                      // stop depth
    0,                      // stop event
    NULL,                   // stop generator
    MR_FALSE,               // print level not specified
    MR_PRINT_LEVEL_SOME,
    MR_FALSE,               // not strict
#ifdef  MR_TRACE_CHECK_INTEGRITY
    MR_FALSE,               // don't check integrity
#endif
    MR_TRUE,                // must check
    NULL                    // pointer to filter/4 for collect queries
};

MR_Code             *MR_trace_real(const MR_LabelLayout *layout);
static  MR_Code     *MR_trace_event(MR_TraceCmdInfo *cmd,
                        MR_bool interactive, const MR_LabelLayout *layout,
                        MR_TracePort port, MR_Unsigned seqno,
                        MR_Unsigned depth, const char *msg);
static  MR_bool     MR_in_traced_region(const MR_ProcLayout *proc_layout,
                        MR_Word *base_sp, MR_Word *base_curfr);
static  MR_bool     MR_is_io_state(MR_TypeInfoParams type_params,
                        MR_PseudoTypeInfo pti);
static  MR_bool     MR_find_saved_io_counter(const MR_LabelLayout *call_label,
                        MR_Word *base_sp, MR_Word *base_curfr,
                        MR_Unsigned *saved_io_counter_ptr);
static  MR_bool     MR_retry_within_io_tabled_region(MR_bool all_actions_tabled,
                        MR_Unsigned retry_event_num, MR_Unsigned cur_event_num);
static  const MR_LabelLayout
                    *MR_unwind_stacks_for_retry(const MR_LabelLayout
                        *top_layout, int ancestor_level, MR_Word **base_sp_ptr,
                        MR_Word **base_curfr_ptr, MR_Word **base_maxfr_ptr,
                        const char **problem);
static  const char  *MR_undo_updates_of_maxfr(const MR_ProcLayout
                        *level_layout, MR_Word *base_sp, MR_Word *base_curfr,
                        MR_Word **maxfr_ptr);
static  MR_Word     MR_trace_find_input_arg(const MR_LabelLayout *label,
                        MR_Word *saved_regs, MR_Word *base_sp,
                        MR_Word *base_curfr, MR_Float *saved_f_regs,
                        MR_uint_least16_t var_num, MR_bool *succeeded);

#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY
static  MR_RetryResult
                    MR_check_minimal_model_calls(MR_EventInfo *event_info,
                        int ancestor_level, MR_Word *target_maxfr,
                        const char **problem);
#endif

static  void        MR_init_call_table_array(void);
static  void        MR_maybe_record_call_table(const MR_ProcLayout
                        *level_layout, MR_Word *base_sp, MR_Word *base_curfr);
static  void        MR_reset_call_table_array(void);
static  void        MR_abandon_call_table_array(void);

// Reserve room for event counts for this many depths initially.

#define INIT_HISTOGRAM  128

// MR_trace_real() is called via a function pointer from MR_trace()
// in runtime/mercury_trace_base.c, which in turn is called from
// compiled code whenever an event to be traced occurs.
//
// The initial part, MR_TRACE_REAL_SETUP_CODE, is shared by MR_trace_real,
// MR_trace_real_decl and MR_trace_real_decl_implicit_subtree.

#if defined(MR_USE_MINIMAL_MODEL_STACK_COPY) && defined(MR_MINIMAL_MODEL_DEBUG)
  #define MR_TRACE_SETUP_MAYBE_SAVE_SUBGOAL_CUR_PROC                        \
        if ((MR_TracePort) layout->MR_sll_port == MR_PORT_CALL) {           \
            MR_subgoal_debug_cur_proc = layout->MR_sll_entry;               \
        }
#else
  #define MR_TRACE_SETUP_MAYBE_SAVE_SUBGOAL_CUR_PROC
#endif

#define MR_TRACE_REAL_SETUP_CODE()                                          \
    do {                                                                    \
        /* In case MR_sp or MR_curfr is transient. */                       \
        MR_restore_transient_registers();                                   \
                                                                            \
        maybe_from_full = layout->MR_sll_entry->MR_sle_maybe_from_full;     \
        if (MR_DETISM_DET_STACK(layout->MR_sll_entry->MR_sle_detism)) {     \
            if (maybe_from_full > 0 && ! MR_stackvar(maybe_from_full)) {    \
                return NULL;                                                \
            }                                                               \
                                                                            \
            seqno = (MR_Unsigned) MR_call_num_stackvar(MR_sp);              \
            depth = (MR_Unsigned) MR_call_depth_stackvar(MR_sp);            \
        } else {                                                            \
            if (maybe_from_full > 0 && ! MR_framevar(maybe_from_full)) {    \
                return NULL;                                                \
            }                                                               \
                                                                            \
            seqno = (MR_Unsigned) MR_call_num_framevar(MR_curfr);           \
            depth = (MR_Unsigned) MR_call_depth_framevar(MR_curfr);         \
        }                                                                   \
                                                                            \
        MR_TRACE_SETUP_MAYBE_SAVE_SUBGOAL_CUR_PROC                          \
                                                                            \
        if (layout->MR_sll_hidden && !MR_trace_unhide_events) {             \
            return NULL;                                                    \
        }                                                                   \
                                                                            \
        MR_trace_event_number++;                                            \
    } while (0)

MR_Code *
MR_trace_real(const MR_LabelLayout *layout)
{
    MR_Integer          maybe_from_full;
    MR_Unsigned         seqno;
    MR_Unsigned         depth;
    MR_SpyAction        action;
    MR_bool             match;
    MR_TracePort        port;

    MR_TRACE_REAL_SETUP_CODE();

#ifdef  MR_TRACE_HISTOGRAM

    // The depths do not necessarily increase one-by-one, since a procedure
    // from a fully traced module can be called from deep within interface
    // traced code.

    MR_ensure_big_enough2(depth, MR_trace_histogram, _all, _exp, int,
        INIT_HISTOGRAM);

    if (depth > MR_trace_histogram_hwm) {
        MR_trace_histogram_hwm = depth;
    }

    MR_trace_histogram_all[depth]++;
    MR_trace_histogram_exp[depth]++;

#endif  // MR_TRACE_HISTOGRAM

    switch (MR_trace_ctrl.MR_trace_cmd) {
        case MR_CMD_COLLECT:
            {
#ifdef MR_USE_EXTERNAL_DEBUGGER
                MR_EventInfo    event_info;
                MR_Word         *saved_regs = event_info.MR_saved_regs;
                MR_Float        *saved_f_regs = event_info.MR_saved_f_regs;
                const char      *path;
                MR_bool         stop_collecting = MR_FALSE;
                int             lineno = 0;

                MR_compute_max_mr_num(event_info.MR_max_mr_num, layout);
                event_info.MR_max_f_num =
                    layout->MR_sll_entry->MR_sle_max_f_num;
                port = (MR_TracePort) layout->MR_sll_port;
                path = MR_label_goal_path(layout);
                MR_copy_regs_to_saved_regs(
                    event_info.MR_max_mr_num, saved_regs,
                    event_info.MR_max_f_num, saved_f_regs);
                MR_trace_init_point_vars(layout, saved_regs, saved_f_regs,
                    port, MR_FALSE);

                lineno = MR_get_line_number(saved_regs, layout, port);

                MR_COLLECT_filter(MR_trace_ctrl.MR_filter_ptr, seqno, depth,
                    port, layout, path, lineno, &stop_collecting);
                MR_copy_saved_regs_to_regs(
                    event_info.MR_max_mr_num, saved_regs,
                    event_info.MR_max_f_num, saved_f_regs);
                if (stop_collecting) {
                    MR_trace_ctrl.MR_trace_cmd = MR_CMD_STEP;
                    return MR_trace_event(&MR_trace_ctrl, MR_TRUE, layout,
                        port, seqno, depth, NULL);
                }
#else
                MR_fatal_error("attempt to use external debugger");
#endif

                goto check_stop_print;
            }

        case MR_CMD_STEP:
            port = (MR_TracePort) layout->MR_sll_port;
            return MR_trace_event(&MR_trace_ctrl, MR_TRUE, layout, port,
                seqno, depth, NULL);

        case MR_CMD_GOTO:
#if 0
            // This code is here in case this needs to be debugged again.
            printf("owner generator %p\n",
                MR_ENGINE(MR_eng_this_context)->MR_ctxt_owner_generator);
            printf("owner generator name %s\n",
                MR_gen_addr_name(
                    MR_ENGINE(MR_eng_this_context)->MR_ctxt_owner_generator));
#endif

            if (MR_trace_event_number >= MR_trace_ctrl.MR_trace_stop_event
                && MR_cur_generator_is_named(
                    MR_trace_ctrl.MR_trace_stop_generator))
            {
                port = (MR_TracePort) layout->MR_sll_port;
                return MR_trace_event(&MR_trace_ctrl, MR_TRUE, layout, port,
                    seqno, depth, NULL);
            } else {
                goto check_stop_print;
            }

        case MR_CMD_NEXT:
            {
                const MR_ProcLayout *proc_layout;
                MR_Unsigned         stop_depth;
                MR_Unsigned         reused_frames;
                const char          *msg;

                stop_depth = MR_trace_ctrl.MR_trace_stop_depth;
                proc_layout = layout->MR_sll_entry;
                MR_trace_find_reused_frames(proc_layout, MR_sp, reused_frames);

                if (depth - reused_frames <= stop_depth && stop_depth <= depth)
                {
                    if (depth != stop_depth) {
                        msg = "This is the next event of a call"
                            " that reused the selected stack frame.\n";
                    } else {
                        msg = NULL;
                    }

                    port = (MR_TracePort) layout->MR_sll_port;
                    return MR_trace_event(&MR_trace_ctrl, MR_TRUE, layout,
                        port, seqno, depth, msg);
                } else {
                    goto check_stop_print;
                }
            }

        case MR_CMD_FINISH:
            port = (MR_TracePort) layout->MR_sll_port;
            if (MR_port_is_final(port)) {
                const MR_ProcLayout *proc_layout;
                MR_Unsigned         stop_depth;
                MR_Unsigned         reused_frames;
                const char          *msg;

                stop_depth = MR_trace_ctrl.MR_trace_stop_depth;
                proc_layout = layout->MR_sll_entry;
                MR_trace_find_reused_frames(proc_layout, MR_sp, reused_frames);

                if (depth - reused_frames <= stop_depth && stop_depth <= depth)
                {
                    if (depth != stop_depth) {
                        msg = "This is the finish of a call"
                            " that reused the selected stack frame.\n";
                    } else {
                        msg = NULL;
                    }

                    return MR_trace_event(&MR_trace_ctrl, MR_TRUE, layout,
                        port, seqno, depth, msg);
                } else {
                    goto check_stop_print;
                }
            } else {
                goto check_stop_print;
            }

        case MR_CMD_FAIL:
            if (MR_trace_ctrl.MR_trace_stop_depth != depth) {
                goto check_stop_print;
            } else {
                port = (MR_TracePort) layout->MR_sll_port;

                if (port == MR_PORT_FAIL || port == MR_PORT_EXCEPTION) {
                    return MR_trace_event(&MR_trace_ctrl, MR_TRUE, layout,
                        port, seqno, depth, NULL);
                } else {
                    goto check_stop_print;
                }
            }

        case MR_CMD_RESUME_FORWARD:
            port = (MR_TracePort) layout->MR_sll_port;
            if (port != MR_PORT_REDO &&
                port != MR_PORT_FAIL &&
                port != MR_PORT_EXCEPTION)
            {
                return MR_trace_event(&MR_trace_ctrl, MR_TRUE, layout, port,
                    seqno, depth, NULL);
            } else {
                goto check_stop_print;
            }

        case MR_CMD_EXCP:
            port = (MR_TracePort) layout->MR_sll_port;
            if (port == MR_PORT_EXCEPTION) {
                return MR_trace_event(&MR_trace_ctrl, MR_TRUE, layout, port,
                    seqno, depth, NULL);
            } else {
                goto check_stop_print;
            }

        case MR_CMD_RETURN:
            port = (MR_TracePort) layout->MR_sll_port;
            if (port != MR_PORT_EXIT) {
                return MR_trace_event(&MR_trace_ctrl, MR_TRUE, layout, port,
                    seqno, depth, NULL);
            } else {
                goto check_stop_print;
            }

        case MR_CMD_USER:
            port = (MR_TracePort) layout->MR_sll_port;
            if (port == MR_PORT_USER) {
                return MR_trace_event(&MR_trace_ctrl, MR_TRUE, layout, port,
                    seqno, depth, NULL);
            } else {
                goto check_stop_print;
            }

        case MR_CMD_MIN_DEPTH:
            if (MR_trace_ctrl.MR_trace_stop_depth <= depth) {
                port = (MR_TracePort) layout->MR_sll_port;
                return MR_trace_event(&MR_trace_ctrl, MR_TRUE, layout, port,
                    seqno, depth, NULL);
            } else {
                goto check_stop_print;
            }

        case MR_CMD_MAX_DEPTH:
            if (MR_trace_ctrl.MR_trace_stop_depth >= depth) {
                port = (MR_TracePort) layout->MR_sll_port;
                return MR_trace_event(&MR_trace_ctrl, MR_TRUE, layout, port,
                    seqno, depth, NULL);
            } else {
                goto check_stop_print;
            }

        case MR_CMD_TO_END:
            goto check_stop_print;

        default:
            MR_fatal_error("invalid command in MR_trace");
    }

check_stop_print:

    if (MR_trace_ctrl.MR_trace_must_check) {
        // The value of MR_trace_ctrl.MR_trace_must_check was precomputed
        // when the command was set up; it was set to MR_TRUE iff any one
        // of the following conditions is true:
        //
        // - MR_trace_ctrl.MR_trace_strict is MR_FALSE (allowing us to stop
        //   at breakpoints whose action is MR_SPY_STOP);
        // - MR_trace_ctrl.MR_trace_print_level is something other than
        //   MR_PRINT_LEVEL_NONE (allowing us to print at least some events);
        // - MR_trace_ctrl.MR_trace_check_vars_integrity is MR_TRUE, requiring
        //   us to check the integrity of all the live variables (which at the
        //   moment means that they can have their sizes computed without
        //   internal errors).
        //
        // The precomputation avoids several jumps in the very frequent case
        // that MR_trace_must_check is false.

        port = (MR_TracePort) layout->MR_sll_port;

#ifdef  MR_TRACE_CHECK_INTEGRITY
        if (MR_trace_ctrl.MR_trace_check_integrity) {
            MR_trace_check_integrity(layout, port);
        }
#endif

        {
            // We ignore the print_list computed here, because we want to
            // execute the print list of a matched spy point even if the event
            // is the target event of a forward movement command such as
            // "goto". In that case, the code above invokes MR_trace_event
            // directly without coming here. We therefore invoke
            // MR_event_matches_spy_point in MR_trace_event. The invocation
            // here is only to find out if we want to stop.

            MR_SpyPrintList print_list;
            match = MR_event_matches_spy_point(layout, port, &action,
                &print_list);
        }

        if (! match) {
            if (MR_trace_ctrl.MR_trace_print_level == MR_PRINT_LEVEL_ALL) {
                return MR_trace_event(&MR_trace_ctrl, MR_FALSE, layout, port,
                    seqno, depth, NULL);
            }

            return NULL;
        }

        if ((! MR_trace_ctrl.MR_trace_strict) && action == MR_SPY_STOP) {
            return MR_trace_event(&MR_trace_ctrl, MR_TRUE, layout, port,
                seqno, depth, NULL);
        }

        if (MR_trace_ctrl.MR_trace_print_level != MR_PRINT_LEVEL_NONE) {
            // It doesn't matter whether action is MR_SPY_STOP or MR_SPY_PRINT;
            // even if it is MR_SPY_STOP, we want to print it if printing
            // is allowed and we did not stop.

            return MR_trace_event(&MR_trace_ctrl, MR_FALSE, layout, port,
                seqno, depth, NULL);
        }
    }

    return NULL;
}

// MR_trace_interrupt() is called via a function pointer from MR_trace()
// in runtime/mercury_trace_base.c, which in turn is called from
// compiled code whenever an event to be traced occurs.
// It is called whenever the user pressed control-C to interrupt the program.
// This is like MR_trace_real(), except that it _always_ calls
// MR_trace_event().

static MR_Code *
MR_trace_interrupt(const MR_LabelLayout *layout)
{
    MR_Unsigned     seqno;
    MR_Unsigned     depth;
    MR_TracePort    port;

    // Restore the original MR_selected_trace_func_ptr value.
    MR_selected_trace_func_ptr = MR_trace_real;

    if (MR_trace_handler == MR_TRACE_INTERNAL) {
        MR_trace_interrupt_message();
    }

    // In case MR_sp or MR_curfr is transient.
    MR_restore_transient_registers();

    if (MR_DETISM_DET_STACK(layout->MR_sll_entry->MR_sle_detism)) {
        seqno = (MR_Unsigned) MR_call_num_stackvar(MR_sp);
        depth = (MR_Unsigned) MR_call_depth_stackvar(MR_sp);
    } else {
        seqno = (MR_Unsigned) MR_call_num_framevar(MR_curfr);
        depth = (MR_Unsigned) MR_call_depth_framevar(MR_curfr);
    }

    port = (MR_TracePort) layout->MR_sll_port;
    MR_trace_event_number++;
    return MR_trace_event(&MR_trace_ctrl, MR_TRUE, layout, port, seqno, depth,
        NULL);
}

void
MR_trace_interrupt_handler(void)
{
    // This function is a signal handler, so there is not much
    // that we can safely do here. We just set the volatile variable
    // MR_selected_trace_func_ptr; the real work will be done by
    // MR_trace_interrupt(), which will be called by MR_trace()
    // at the next debugger event.

    MR_selected_trace_func_ptr = MR_trace_interrupt;
}

// The MR_TRACE_EVENT_DECL_AND_SETUP macro is the initial part of
// MR_trace_event, shared also by MR_trace_real_decl and
// MR_trace_real_decl_implicit_subtree.
//
// It must be included in a context that defines layout, port, seqno and depth.

#define MR_TRACE_EVENT_DECL_AND_SETUP                                   \
    MR_Code         *jumpaddr;                                          \
    MR_EventInfo    event_info;                                         \
    MR_Word         *saved_regs = event_info.MR_saved_regs;             \
    MR_Float        *saved_f_regs = event_info.MR_saved_f_regs;         \
                                                                        \
    event_info.MR_event_number = MR_trace_event_number;                 \
    event_info.MR_call_seqno = seqno;                                   \
    event_info.MR_call_depth = depth;                                   \
    event_info.MR_trace_port = port;                                    \
    event_info.MR_event_sll = layout;                                   \
    event_info.MR_event_path = MR_label_goal_path(layout);              \
                                                                        \
    MR_compute_max_mr_num(event_info.MR_max_mr_num, layout);            \
    event_info.MR_max_f_num = layout->MR_sll_entry->MR_sle_max_f_num;   \
    /* This also saves the regs in MR_fake_regs. */                     \
    MR_copy_regs_to_saved_regs(event_info.MR_max_mr_num, saved_regs,    \
        event_info.MR_max_f_num, saved_f_regs);

// The MR_TRACE_EVENT_TEARDOWN macro is the final part of MR_trace_event,
// shared also by MR_trace_real_decl and MR_trace_real_decl_implicit_subtree.
//
// It must be included in a context that defines jumpaddr, saved_regs and
// event_info.
//
// Whenever the debugger changes the flow of control, e.g. by executing
// a retry command, it also sets up the saved registers to the state
// that is appropriate for the label at which execution will resume.
// There may be more registers live at that point than at the point
// at which MR_trace was called. Therefore max_mr_num should also be set
// appropriately when executing a retry command.
//
// For the treatment of MR_global_hp, see the top of this file.

#define MR_TRACE_EVENT_TEARDOWN                                         \
    /* In case MR_global_hp is transient. */                            \
    MR_restore_transient_registers();                                   \
    MR_saved_global_hp_word(saved_regs) = (MR_Word) MR_global_hp;       \
    MR_copy_saved_regs_to_regs(event_info.MR_max_mr_num, saved_regs,    \
        event_info.MR_max_f_num, saved_f_regs);                         \
    return jumpaddr;

static MR_Code *
MR_trace_event(MR_TraceCmdInfo *cmd, MR_bool interactive,
    const MR_LabelLayout *layout, MR_TracePort port,
    MR_Unsigned seqno, MR_Unsigned depth, const char *msg)
{
    MR_TRACE_EVENT_DECL_AND_SETUP

#ifdef MR_USE_EXTERNAL_DEBUGGER
    switch (MR_trace_handler) {
        case MR_TRACE_EXTERNAL:
            if (!interactive) {
                MR_fatal_error("reporting event for external debugger");
            }

            jumpaddr = MR_trace_event_external(cmd, &event_info);
            break;

        case MR_TRACE_INTERNAL:
#endif
            // MR_TRACE_INTERNAL is the only possible value of MR_trace_handler
            // if MR_USE_EXTERNAL_DEBUGGER is not defined.

            {
                MR_SpyAction        action;         // ignored
                MR_SpyPrintList     print_list;

                print_list = NULL;
                (void) MR_event_matches_spy_point(layout, port, &action,
                    &print_list);
                jumpaddr = MR_trace_event_internal(cmd, interactive,
                    print_list, &event_info, msg);
            }
#ifdef MR_USE_EXTERNAL_DEBUGGER
            break;
        }
#endif

    MR_TRACE_EVENT_TEARDOWN
}

MR_Code *
MR_trace_real_decl(const MR_LabelLayout *layout)
{
    MR_Integer          maybe_from_full;
    MR_Unsigned         seqno;
    MR_Unsigned         depth;
    MR_TracePort        port;

    MR_TRACE_REAL_SETUP_CODE();

    port = (MR_TracePort) layout->MR_sll_port;

    {
        MR_TRACE_EVENT_DECL_AND_SETUP
        jumpaddr = MR_trace_decl_debug(&event_info);
        MR_TRACE_EVENT_TEARDOWN
    }
}

MR_Code *
MR_trace_real_decl_implicit_subtree(const MR_LabelLayout *layout)
{
    MR_Integer          maybe_from_full;
    MR_Unsigned         seqno;
    MR_Unsigned         depth;
    MR_TracePort        port;
    MR_Unsigned         node_depth;

    MR_TRACE_REAL_SETUP_CODE();

    port = (MR_TracePort) layout->MR_sll_port;

    // Note that we do not check for suppressed events or events from compiler
    // generated procedures, since we want this function to be as fast as
    // possible (since it will be executed the most with large search spaces).
    // This has minimal impact on the accuracy of the histogram, since
    // event suppression is not currently used and compiler generated
    // procedures are very likely to be leaf nodes or close to leaf nodes.

    MR_DD_CALC_NODE_DEPTH(port, node_depth, MR_edt_implicit_subtree_depth);

    if (node_depth < MR_edt_implicit_subtree_num_counters) {
        MR_edt_implicit_subtree_counters[node_depth]++;

        if ((node_depth == 0) && MR_port_is_final(port)) {
            MR_selected_trace_func_ptr = MR_trace_real_decl;

            {
                MR_TRACE_EVENT_DECL_AND_SETUP
                jumpaddr = MR_trace_decl_debug(&event_info);
                // MR_TRACE_EVENT_TEARDOWN returns jumpaddr
                MR_TRACE_EVENT_TEARDOWN
            }
        }
    }

    MR_DECL_MAYBE_UPDATE_PROGRESS_IMPLICIT_SUBTREE(MR_trace_event_number);

    return NULL;
}

////////////////////////////////////////////////////////////////////////////

// The initial size of arrays of argument values.
#define MR_INIT_ARG_COUNT   20

MR_RetryResult
MR_trace_retry(MR_EventInfo *event_info,
    MR_Level ancestor_level, MR_RetryAcrossIo across_io,
    MR_bool assume_all_io_is_tabled, const char *retry_interactive_message,
    MR_bool *unsafe_retry, const char **problem,
    FILE *in_fp, FILE *out_fp, MR_Code **jumpaddr)
{
    MR_Word                 *base_sp;
    MR_Word                 *base_curfr;
    MR_Word                 *base_maxfr;
    const MR_LabelLayout    *top_layout;
    const MR_LabelLayout    *return_label_layout;
    const MR_LabelLayout    *call_label;
    const MR_ProcLayout     *level_layout;
    MR_Integer              call_all_var_count;
    MR_Integer              call_long_var_count;
    MR_Word                 *r_args;
    int                     r_arg_max;
    MR_Word                 *f_args;
    int                     f_arg_max;
    int                     arg_num;
    MR_Word                 arg_value;
    MR_bool                 reg_f;
    MR_TypeInfoParams       type_params;
    int                     i;
    MR_bool                 succeeded;
    MR_Word                 *saved_regs;
    MR_Float                *saved_f_regs;
    MR_Unsigned             reused_frames;
    MR_bool                 has_io_state;
    MR_bool                 io_actions_were_performed;
    MR_bool                 is_io_state;
    MR_bool                 found_io_action_counter;
    MR_Unsigned             saved_io_action_counter;
#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY
    MR_RetryResult          result;
#endif

#ifdef  MR_DEEP_PROFILING
    *problem = "retry is incompatible with deep profiling.";
    return MR_RETRY_ERROR;
#endif

    r_args = NULL;
    f_args = NULL;
    MR_init_call_table_array();

    saved_regs = event_info->MR_saved_regs;
    saved_f_regs = event_info->MR_saved_f_regs;
#ifdef  MR_DEBUG_RETRY
    MR_print_stack_regs(stdout, saved_regs);
#endif

    top_layout = event_info->MR_event_sll;
    *problem = NULL;
    base_sp = MR_saved_sp(saved_regs);
    base_curfr = MR_saved_curfr(saved_regs);
    base_maxfr = MR_saved_maxfr(saved_regs);
    return_label_layout = MR_unwind_stacks_for_retry(top_layout,
        ancestor_level, &base_sp, &base_curfr, &base_maxfr, problem);

#ifdef  MR_DEBUG_RETRY
    MR_print_stack_regs(stdout, saved_regs);
#endif

    if (return_label_layout == NULL) {
        if (*problem == NULL) {
            *problem = "MR_unwind_stacks_for_retry failed "
                "without reporting a problem";
        }

        goto report_problem;
    }

    level_layout = return_label_layout->MR_sll_entry;
    if (! MR_PROC_LAYOUT_HAS_EXEC_TRACE(level_layout)) {
        *problem = "that procedure does not have debugging information";
        goto report_problem;
    }

    if (! MR_in_traced_region(level_layout, base_sp, base_curfr)) {
        *problem = "that call is not in a deep traced region";
        goto report_problem;
    }

    call_label = level_layout->MR_sle_call_label;
    if (call_label == NULL || call_label->MR_sll_var_count < 0) {
        *problem = "Cannot perform retry because information about "
            "the input arguments is not available.";
        goto report_problem;
    }

    // With the Boehm collector, args need not be considered a root,
    // since its contents are just copies of values from elsewhere,
    // With the native collector, it need not be considered a root
    // because its lifetime spans only this function, in which
    // no native garbage collection can be triggered.

    r_arg_max = 0;
    f_arg_max = 0;

    // Check if any of the (non-polymorphic) arguments are of type io.state.
    // We check this here and then again for each argument in the for loop
    // below, because if the io.state argument is not polymorphic, then it
    // would have been removed which means the check in the for loop below
    // won't find it. If the argument is polymorphic, then it won't have been
    // removed, however the check here won't pick this up since it only uses
    // the static argument types. The check in the for loop below will pick up
    // polymorphic arguments which have type io.state, because it materializes
    // the type of each argument before checking it.

    if (MR_proc_has_io_state_pair(level_layout)) {
        has_io_state = MR_TRUE;
    } else {
        has_io_state = MR_FALSE;
    }

    type_params = MR_materialize_type_params_base(return_label_layout,
        saved_regs, base_sp, base_curfr);

    call_all_var_count = MR_all_desc_var_count(call_label);
    call_long_var_count = MR_long_desc_var_count(call_label);
    for (i = 0; i < call_all_var_count; i++) {
        // Check if the argument is of type io.state. We check this before
        // calling MR_trace_find_input_arg, since MR_trace_find_input_arg may
        // or may not find the value of the io.state argument. We don't care
        // if it finds the value of an io.state argument or not, since the
        // value is junk and never used. The safety of the retry is addressed
        // by the code handling has_io_state after this for loop.

        is_io_state = MR_is_io_state(type_params, MR_var_pti(call_label, i));

        if (is_io_state) {
            has_io_state = MR_TRUE;
        } else {
            arg_value = MR_trace_find_input_arg(return_label_layout,
                saved_regs, base_sp, base_curfr, saved_f_regs,
                call_label->MR_sll_var_nums[i], &succeeded);

            if (! succeeded) {
                *problem = "Cannot perform retry because the "
                    "values of some input arguments are missing.";
                goto report_problem;
            }

            if (i < call_long_var_count) {
                MR_LongLval     long_locn;

                long_locn = MR_long_desc_var_locn(call_label, i);
                reg_f = (MR_LONG_LVAL_TYPE(long_locn) == MR_LONG_LVAL_TYPE_F);
                arg_num = MR_get_register_number_long(long_locn);
            } else {
                MR_ShortLval    short_locn;

                short_locn = MR_short_desc_var_locn(call_label,
                    i - call_long_var_count);
                reg_f = MR_FALSE;
                arg_num = MR_get_register_number_short(short_locn);
            }

            if (arg_num > 0) {
                if (reg_f) {
                    // This sets f_arg, f_arg_max.
                    MR_ensure_big_enough(arg_num, f_arg, MR_Word,
                        MR_INIT_ARG_COUNT);
                    f_args[arg_num] = arg_value;
                } else {
                    // This sets r_arg, r_arg_max.
                    MR_ensure_big_enough(arg_num, r_arg, MR_Word,
                        MR_INIT_ARG_COUNT);
                    r_args[arg_num] = arg_value;
                }
            } else {
                MR_fatal_error("illegal location for input argument");
            }
        }
    }

    found_io_action_counter = MR_find_saved_io_counter(call_label,
        base_sp, base_curfr, &saved_io_action_counter);

    if ((found_io_action_counter &&
        saved_io_action_counter < MR_io_tabling_counter) ||
        ((! found_io_action_counter) && has_io_state))
    {
        io_actions_were_performed = MR_TRUE;
    } else {
        // XXX We assume calls that weren't I/O tabled and that don't have
        // a pair of I/O state arguments did not do any I/O.
        // This assumption breaks for impure code that does I/O,
        // but we don't have a way to check if predicates are impure in the
        // debugger yet.

        io_actions_were_performed = MR_FALSE;
    }

    if (io_actions_were_performed) {
        MR_Unsigned retry_event_num;
        MR_bool     all_actions_tabled;

        // Event numbers are stored *before* being incremented at calls.

        if (MR_DETISM_DET_STACK(level_layout->MR_sle_detism)) {
            retry_event_num = MR_event_num_stackvar(base_sp) + 1;
        } else {
            retry_event_num = MR_event_num_framevar(base_curfr) + 1;
        }

#ifdef  MR_EXEC_TRACE
        all_actions_tabled = MR_TRUE;
#else
        all_actions_tabled = assume_all_io_is_tabled;
#endif

        if (! MR_retry_within_io_tabled_region(all_actions_tabled,
            retry_event_num, event_info->MR_event_number))
        {
            MR_bool allow_retry;
            char    *answer;

            *unsafe_retry = MR_TRUE;

            switch (across_io) {

                case MR_RETRY_IO_FORCE:
                    // No further checks required.
                    break;

                case MR_RETRY_IO_INTERACTIVE:
                    if (in_fp == NULL || out_fp == NULL) {
                        MR_fatal_error("MR_RETRY_IO_INTERACTIVE but null fp");
                    }

                    answer = MR_trace_getline(retry_interactive_message, in_fp,
                        out_fp);

                    if (answer == NULL) {
                        // The user has pressed EOF.
                        allow_retry = MR_FALSE;
                    } else {
                        allow_retry = (answer[0] == 'y' || answer[0] == 'Y');
                        MR_free(answer);
                    }

                    if (! allow_retry) {
                        *problem = "Retry aborted.";
                        goto report_problem;
                    }

                    break;

                case MR_RETRY_IO_ONLY_IF_SAFE:
                    if (! all_actions_tabled) {
                        *problem = "Cannot perform retry: "
                            "not all I/O actions are tabled";
                    } else {
                        *problem = "Cannot perform retry: "
                            "outside the safe region";
                    }
                    goto report_problem;

                default:
                    MR_fatal_error("MR_trace_retry: unknown across_io");
            }
        } else {
            *unsafe_retry = MR_FALSE;
        }
    } else {
        *unsafe_retry = MR_FALSE;
    }

#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY

    result = MR_check_minimal_model_calls(event_info, ancestor_level,
        base_maxfr, problem);

    switch (result) {
    case MR_RETRY_OK_DIRECT:
        // We just go on to the code below.
        break;

    case MR_RETRY_OK_FINISH_FIRST:
    case MR_RETRY_OK_FAIL_FIRST:
        MR_abandon_call_table_array();
        return result;

    case MR_RETRY_ERROR:
    default:
        if (*problem == NULL) {
            *problem = "MR_check_minimal_model_calls failed "
                "without reporting problem";
        }

        goto report_problem;
    }

#endif  // MR_USE_MINIMAL_MODEL_STACK_COPY

    // At this point, we are now sure that we can carry out the retry
    // operation directly. Before we were sure, we couldn't modify the
    // environment (i.e. the stacks, the saved registers, and the relevant
    // call tables). Now, however, we can set up the environment to make
    // it reflect its state at the time of the entry to the retried call.

    MR_saved_sp_word(saved_regs) = (MR_Word) base_sp;
    MR_saved_curfr_word(saved_regs) = (MR_Word) base_curfr;
    MR_saved_maxfr_word(saved_regs) = (MR_Word) base_maxfr;

    MR_trace_find_reused_frames(level_layout, base_sp, reused_frames);
    if (reused_frames != 0) {
        MR_trace_tailrec_have_reused_frames = MR_TRUE;
        MR_trace_tailrec_num_reused_frames = reused_frames;
    }

    // If the retried call is shallow traced, it must have been called from
    // a deep traced region, since otherwise we wouldn't have had
    // sufficient information to retry it. Since the retry may have been
    // executed from a deep traced procedure called from deep within
    // a shallow traced region, MR_trace_from_full may now be MR_FALSE.
    // We therefore reset it to MR_TRUE.
    //
    // The only case where this assignment does not faithfully recreate the
    // situation that existed at the time the retried call was originally
    // executed is when the retried call is deep traced but was called from
    // inside a shallow traced region. However, this difference will not
    // affect the behavior of either the called procedure (since deep
    // traced procedures do not pay attention to the initial value of
    // MR_trace_from_full) or its caller (since all callers expect that
    // the callee may clobber MR_trace_from_full).

    MR_trace_from_full = MR_TRUE;

    if (MR_DETISM_DET_STACK(level_layout->MR_sle_detism)) {
        MR_LongLval     location;
        MR_Word         *this_frame;

        location = level_layout->MR_sle_succip_locn;
        if (MR_LONG_LVAL_TYPE(location) != MR_LONG_LVAL_TYPE_STACKVAR) {
            MR_fatal_error("illegal location for stored succip");
        }

        this_frame = MR_saved_sp(saved_regs);
#ifdef  MR_DEBUG_RETRY
        MR_print_succip_reg(stdout, saved_regs);
        MR_print_stack_regs(stdout, saved_regs);
#endif
        MR_saved_succip_word(saved_regs) =
            MR_based_stackvar(this_frame, MR_LONG_LVAL_NUMBER(location));
        MR_saved_sp_word(saved_regs) = (MR_Word)
            (MR_saved_sp(saved_regs) - level_layout->MR_sle_stack_slots);
#ifdef  MR_DEBUG_RETRY
        MR_print_succip_reg(stdout, saved_regs);
        MR_print_stack_regs(stdout, saved_regs);
#endif
        MR_trace_event_number = MR_event_num_stackvar(this_frame);
        MR_trace_call_seqno = MR_call_num_stackvar(this_frame) - 1;
        MR_trace_call_depth = MR_call_depth_stackvar(this_frame) - 1;

#ifdef  MR_USE_TRAIL
        if (level_layout->MR_sle_maybe_trail >= 0) {
            MR_Word ticket_counter;
            MR_Word trail_ptr;

            trail_ptr = MR_based_stackvar(this_frame,
                level_layout->MR_sle_maybe_trail);
            ticket_counter = MR_based_stackvar(this_frame,
                level_layout->MR_sle_maybe_trail+1);
            MR_reset_ticket(trail_ptr, MR_retry);
            MR_discard_tickets_to(ticket_counter);
        } else {
            MR_fatal_error("retry cannot restore the trail");
        }
#endif
    } else {
        MR_Word *this_frame;

        this_frame = MR_saved_curfr(saved_regs);
#ifdef  MR_DEBUG_RETRY
        MR_print_succip_reg(stdout, saved_regs);
        MR_print_stack_regs(stdout, saved_regs);
#endif
        MR_saved_succip_word(saved_regs) = MR_succip_slot_word(this_frame);
        MR_saved_curfr_word(saved_regs) = MR_succfr_slot_word(this_frame);
        MR_saved_maxfr_word(saved_regs) = MR_prevfr_slot_word(this_frame);
#ifdef  MR_DEBUG_RETRY
        MR_print_succip_reg(stdout, saved_regs);
        MR_print_stack_regs(stdout, saved_regs);
#endif
        MR_trace_event_number = MR_event_num_framevar(this_frame);
        MR_trace_call_seqno = MR_call_num_framevar(this_frame) - 1;
        MR_trace_call_depth = MR_call_depth_framevar(this_frame) - 1;

#ifdef  MR_USE_TRAIL
        if (level_layout->MR_sle_maybe_trail >= 0) {
            MR_Word ticket_counter;
            MR_Word trail_ptr;

            trail_ptr = MR_based_framevar(this_frame,
                level_layout->MR_sle_maybe_trail);
            ticket_counter = MR_based_framevar(this_frame,
                level_layout->MR_sle_maybe_trail+1);
            MR_reset_ticket(trail_ptr, MR_retry);
            MR_discard_tickets_to(ticket_counter);
        } else {
            MR_fatal_error("retry cannot restore the trail");
        }
#endif
    }

    for (i = 1; i < r_arg_max; i++) {
        MR_saved_reg_assign(saved_regs, i, r_args[i]);
    }
    for (i = 1; i < f_arg_max; i++) {
        MR_saved_reg_assign(saved_f_regs, i, MR_word_to_float(f_args[i]));
    }

    if (io_actions_were_performed && found_io_action_counter) {
        MR_io_tabling_counter = saved_io_action_counter;
    }

    event_info->MR_max_mr_num = MR_max(event_info->MR_max_mr_num, r_arg_max);
    event_info->MR_max_f_num = MR_max(event_info->MR_max_f_num, f_arg_max);
    *jumpaddr = level_layout->MR_sle_code_addr;
#ifdef  MR_DEBUG_RETRY
    printf("jumpaddr is ");
    MR_print_label(stdout, *jumpaddr);
    printf("\n");
#endif

    if (r_args != NULL) {
        MR_free(r_args);
    }
    if (f_args != NULL) {
        MR_free(f_args);
    }

    MR_reset_call_table_array();
#ifdef  MR_DEBUG_RETRY
    MR_print_stack_regs(stdout, saved_regs);
#endif
    return MR_RETRY_OK_DIRECT;

report_problem:
    if (r_args != NULL) {
        MR_free(r_args);
    }
    if (f_args != NULL) {
        MR_free(f_args);
    }

    MR_abandon_call_table_array();
    return MR_RETRY_ERROR;
}

static MR_bool
MR_in_traced_region(const MR_ProcLayout *proc_layout,
    MR_Word *base_sp, MR_Word *base_curfr)
{
    if (proc_layout->MR_sle_maybe_from_full <= 0) {
        // The procedure was deep traced.
        return MR_TRUE;
    } else {
        // The procedure was shallow traced.
        MR_Word from_full;

        if (MR_DETISM_DET_STACK(proc_layout->MR_sle_detism)) {
            from_full = MR_based_stackvar(base_sp,
                proc_layout->MR_sle_maybe_from_full);
        } else {
            from_full = MR_based_framevar(base_curfr,
                proc_layout->MR_sle_maybe_from_full);
        }

        return from_full;
    }
}

static MR_bool
MR_is_io_state(MR_TypeInfoParams type_params, MR_PseudoTypeInfo pti)
{
    MR_TypeCtorInfo type_ctor_info;
    MR_ConstString  module;
    MR_ConstString  type;
    MR_TypeInfo     type_info;

    type_info = MR_create_type_info(type_params, pti);
    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
    module = MR_type_ctor_module_name(type_ctor_info);
    type = MR_type_ctor_name(type_ctor_info);

    return (MR_streq(module, "io") && MR_streq(type, "state"));
}

static MR_bool
MR_find_saved_io_counter(const MR_LabelLayout *call_label,
    MR_Word *base_sp, MR_Word *base_curfr, MR_Unsigned *saved_io_counter_ptr)
{
    const MR_ProcLayout     *level_layout;

    if (call_label == NULL) {
        return MR_FALSE;
    }

    level_layout = call_label->MR_sll_entry;
    if (level_layout->MR_sle_maybe_io_seq <= 0) {
        return MR_FALSE;
    }

    if (! MR_in_traced_region(level_layout, base_sp, base_curfr)) {
        return MR_FALSE;
    }

    if (MR_DETISM_DET_STACK(level_layout->MR_sle_detism)) {
        *saved_io_counter_ptr = MR_based_stackvar(base_sp,
            level_layout->MR_sle_maybe_io_seq);
    } else {
        *saved_io_counter_ptr = MR_based_framevar(base_curfr,
            level_layout->MR_sle_maybe_io_seq);
    }

    return MR_TRUE;
}

static MR_bool
MR_retry_within_io_tabled_region(MR_bool all_actions_tabled,
    MR_Unsigned retry_event_num, MR_Unsigned cur_event_num)
{
    if (!all_actions_tabled) {
        // Regardless of event numbers, some I/O actions may not be tabled.
        return MR_FALSE;
    }

    if (MR_io_tabling_start_event_num == 0) {
        // I/O tabling wasn't ever turned on.
        return MR_FALSE;
    }

    if (retry_event_num < MR_io_tabling_start_event_num) {
        // I/O tabling was turned on, but after the retry event.
        return MR_FALSE;
    }

    if ((MR_io_tabling_stop_event_num > 0)
        && (MR_io_tabling_stop_event_num < cur_event_num))
    {
        // I/O tabling was turned off before the current event.
        return MR_FALSE;
    }

    return MR_TRUE;
}

// This function figures out the state of the stacks (i.e. the values of MR_sp,
// MR_curfr and MR_maxfr) just after entry to the procedure specified by the
// given ancestor level, and returns the proc layout for the specified
// procedure. It also finds the list of call table tips that must be reset
// on a retry of that ancestor.
//
// If it finds that it cannot do its job, it returns NULL and sets *problem
// to point to a string giving the reason for its failure.

static const MR_LabelLayout *
MR_unwind_stacks_for_retry(const MR_LabelLayout *top_layout,
    int ancestor_level, MR_Word **base_sp_ptr, MR_Word **base_curfr_ptr,
    MR_Word **base_maxfr_ptr, const char **problem)
{
    MR_StackWalkStepResult      result;
    const MR_ProcLayout         *level_layout;
    const MR_LabelLayout        *return_label_layout;
    MR_Level                    level;
    MR_Unsigned                 reused_frames;

    if (ancestor_level < 0) {
        *problem = "no such stack frame";
        return NULL;
    }

#ifdef  MR_DEBUG_RETRY_STACKS
    MR_print_detstackptr(MR_mdb_out, *sp_ptr);
    fprintf(MR_mdb_out, "\n");
    MR_print_nondetstackptr(MR_mdb_out, *curfr_ptr);
    fprintf(MR_mdb_out, "\n");
    MR_print_nondetstackptr(MR_mdb_out, *maxfr_ptr);
    fprintf(MR_mdb_out, "\n");
#endif

    return_label_layout = top_layout;
    level_layout = top_layout->MR_sll_entry;
    *problem = MR_undo_updates_of_maxfr(level_layout,
        *base_sp_ptr, *base_curfr_ptr, base_maxfr_ptr);

    if (*problem != NULL) {
        return NULL;
    }

    MR_maybe_record_call_table(level_layout, *base_sp_ptr, *base_curfr_ptr);

#ifdef  MR_DEBUG_RETRY_STACKS
    MR_print_detstackptr(MR_mdb_out, *sp_ptr);
    fprintf(MR_mdb_out, "\n");
    MR_print_nondetstackptr(MR_mdb_out, *curfr_ptr);
    fprintf(MR_mdb_out, "\n");
    MR_print_nondetstackptr(MR_mdb_out, *maxfr_ptr);
    fprintf(MR_mdb_out, "\n");
#endif

    level = 0;
    while (level < ancestor_level) {
        result = MR_stack_walk_step(level_layout, &return_label_layout,
            base_sp_ptr, base_curfr_ptr, &reused_frames, problem);
        if (result != MR_STEP_OK || return_label_layout == NULL) {
            if (*problem == NULL) {
                *problem = "not that many ancestors";
            } else if (MR_streq(*problem, "reached unknown label")) {
                *problem = "cannot retry across non-debuggable region";
            }

            return NULL;
        }

#ifdef  MR_DEBUG_RETRY_STACKS
        MR_print_detstackptr(MR_mdb_out, *sp_ptr);
        fprintf(MR_mdb_out, "\n");
        MR_print_nondetstackptr(MR_mdb_out, *curfr_ptr);
        fprintf(MR_mdb_out, "\n");
        MR_print_nondetstackptr(MR_mdb_out, *maxfr_ptr);
        fprintf(MR_mdb_out, "\n");
        fprintf(MR_mdb_out, "reused_frames: %ld\n", (long) reused_frames);
#endif

        level_layout = return_label_layout->MR_sll_entry;

        // Don't unwind to builtin_catch, because builtin_catch has
        // no CALL event, even though it has a stack frame.

        if ((level == ancestor_level - 1) &&
            MR_trace_proc_layout_is_builtin_catch(level_layout))
        {
            // XXX This seems like it will induce an infinite loop.
            level--;
        }

        *problem = MR_undo_updates_of_maxfr(level_layout,
            *base_sp_ptr, *base_curfr_ptr, base_maxfr_ptr);

        if (*problem != NULL) {
            return NULL;
        }

        MR_maybe_record_call_table(level_layout,
            *base_sp_ptr, *base_curfr_ptr);

        level += 1 + reused_frames;
    }

    if (level != ancestor_level) {
        *problem = "cannot retry a call whose stack frame has been reused";
        return NULL;
    }

#ifdef  MR_DEBUG_RETRY_STACKS
    MR_print_detstackptr(MR_mdb_out, *sp_ptr);
    fprintf(MR_mdb_out, "\n");
    MR_print_nondetstackptr(MR_mdb_out, *curfr_ptr);
    fprintf(MR_mdb_out, "\n");
    MR_print_nondetstackptr(MR_mdb_out, *maxfr_ptr);
    fprintf(MR_mdb_out, "\n");
    fprintf(MR_mdb_out, "final level: %ld\n", (long) level);
#endif

    return return_label_layout;
}

static const char *
MR_undo_updates_of_maxfr(const MR_ProcLayout *level_layout,
    MR_Word *level_sp, MR_Word *level_curfr, MR_Word **maxfr_ptr)
{
    if (MR_DETISM_DET_STACK(level_layout->MR_sle_detism)) {
        // The code of a procedure that lives on the det stack never updates
        // curfr, but may update maxfr by pushing a temporary nondet frame.
        // If it does so, and the procedure is traced, the original value
        // of maxfr will be saved in a stack slot.

        if (! MR_PROC_LAYOUT_HAS_EXEC_TRACE(level_layout)) {
            return "an intervening stack frame has no debugging information";
        } else if (level_layout->MR_sle_maybe_maxfr > 0) {
            *maxfr_ptr = (MR_Word *) MR_based_stackvar(level_sp,
                level_layout->MR_sle_maybe_maxfr);
#if MR_DEBUG_RETRY
            fprintf(stdout, "resetting maxfr to ");
            MR_print_nondetstackptr(stdout, *maxfr_ptr);
            fprintf(stdout, "\n");
#endif
        } // else we need do nothing
    } else {
        // When we finish setting up the stack frame of a procedure that lives
        // on the nondet stack, maxfr == curfr.

        *maxfr_ptr = level_curfr;
    }

    return NULL;
}

static MR_Word
MR_trace_find_input_arg(const MR_LabelLayout *label_layout,
    MR_Word *saved_regs, MR_Word *base_sp, MR_Word *base_curfr,
    MR_Float *saved_f_regs, MR_uint_least16_t var_num, MR_bool *succeeded)
{
    int         i;
    MR_Integer  all_var_count;
    MR_Integer  long_var_count;

    if (label_layout->MR_sll_var_nums == NULL) {
        *succeeded = MR_FALSE;
        return 0;
    }

    all_var_count = MR_all_desc_var_count(label_layout);
    long_var_count = MR_long_desc_var_count(label_layout);
    for (i = 0; i < all_var_count; i++) {
        if (var_num == label_layout->MR_sll_var_nums[i]) {
            if (i < long_var_count) {
                MR_LongLval     long_locn;

                long_locn = MR_long_desc_var_locn(label_layout, i);
                return MR_lookup_long_lval_base(long_locn,
                    saved_regs, base_sp, base_curfr, saved_f_regs,
                    succeeded);
            } else {
                MR_ShortLval    short_locn;

                short_locn = MR_short_desc_var_locn(label_layout,
                    i - long_var_count);
                return MR_lookup_short_lval_base(short_locn,
                    saved_regs, base_sp, base_curfr, succeeded);
            }
        }
    }

    *succeeded = MR_FALSE;
    return 0;
}

////////////////////////////////////////////////////////////////////////////

#ifdef  MR_USE_MINIMAL_MODEL_STACK_COPY

// MR_check_minimal_model_calls scans the nondet stack region about to be
// discarded by the retry operation, searching for the stack frames of
// procedures whose evaluation method is minimal model.
//
// In grades that do enable minimal model tabling, redoip hijacking is
// disabled, so the fact that the redoip slot of a nondet stack frame points
// to a label within a given procedure means that the frame was created by that
// procedure.
//
// If we ever get here, the redoip slot of the home frame of every model_non
// procedure compiled with debugging ought to point to a label in its own code,
// not to a label in the runtime system.

#define INIT_RECORD_ARRAY_SIZE      10

typedef struct {
    MR_Subgoal  *record_subgoal;
    MR_Subgoal  *record_leader;
    MR_bool     found_leader_generator;
} MR_MinimalModelRecord;

static MR_RetryResult
MR_check_minimal_model_calls(MR_EventInfo *event_info, int ancestor_level,
    MR_Word *target_maxfr, const char **problem)
{
    const MR_LabelLayout    *label_layout;
    const MR_ProcLayout     *proc_layout;
    MR_Word                 *top_maxfr;
    MR_Word                 *cur_maxfr;
    MR_Code                 *redoip;
    MR_TrieNode             trienode;
    MR_Subgoal              *subgoal;
    MR_Subgoal              *leader;
    MR_MinimalModelRecord   *record_ptrs;
    int                     record_ptr_max;
    int                     record_ptr_next;
    int                     frame_size;
    int                     cur_gen;
    MR_Internal             *internal;
    int                     i;
    MR_bool                 any_missing_generators;

    top_maxfr = MR_saved_maxfr(event_info->MR_saved_regs);
    cur_gen = MR_gen_next - 1;

    record_ptrs = NULL;
    record_ptr_max = 0;
    record_ptr_next = 0;

    for (cur_maxfr = top_maxfr;
        cur_maxfr > target_maxfr;
        cur_maxfr = MR_prevfr_slot(cur_maxfr))
    {
        frame_size = cur_maxfr - MR_prevfr_slot(cur_maxfr);
        if (frame_size == MR_NONDET_TEMP_SIZE) {
            // These frames represent auxiliary frames of model_non procedures.
            // We will traverse the main frame of every such procedure as well,
            // and we can do everything we need to do then, so we can
            // ignore these frames.

            continue;
        } else if (frame_size == MR_DET_TEMP_SIZE) {
            // These frames represent auxiliary frames of model_det or
            // model_semi procedures, which call a model_non procedure
            // inside a commit. We should queue the associated commit stack
            // entry for resetting, but the future of the commit stack
            // is in flux at the moment (it has some design-level bugs),
            // so for now we just note the need for future work here XXX.

            continue;
        }

        // The remaining frames represent the main frames of model_non
        // procedures.

        redoip = MR_redoip_slot(cur_maxfr);
        internal = MR_lookup_internal_by_addr(redoip);
        if (internal == NULL) {
            // Code in this file depends on the exact string we return here.
            *problem = "reached unknown label";
            return MR_RETRY_ERROR;
        }

        if (internal->MR_internal_layout == NULL) {
            *problem = "reached label without debugging info";
            return MR_RETRY_ERROR;
        }

        label_layout = internal->MR_internal_layout;
        proc_layout = label_layout->MR_sll_entry;

        if (! MR_PROC_LAYOUT_HAS_EXEC_TRACE(proc_layout)) {
            *problem = "reached label without debugging info";
            return MR_RETRY_ERROR;
        }

        if (MR_sle_eval_method(proc_layout)
            != MR_EVAL_METHOD_MINIMAL_STACK_COPY)
        {
            continue;
        }

        if (proc_layout->MR_sle_maybe_call_table <= 0) {
            MR_fatal_error("minimal model procedure has no call table slot");
        }

        trienode = (MR_TrieNode) MR_based_framevar(cur_maxfr,
            proc_layout->MR_sle_maybe_call_table);
        subgoal = trienode->MR_subgoal;
        if (subgoal->MR_sg_leader != NULL) {
            leader = subgoal->MR_sg_leader;
        } else {
            leader = subgoal;
        }

        MR_ensure_room_for_next(record_ptr, MR_MinimalModelRecord,
            INIT_RECORD_ARRAY_SIZE);

        record_ptrs[record_ptr_next].record_subgoal = subgoal;
        record_ptrs[record_ptr_next].record_leader = leader;
        record_ptrs[record_ptr_next].found_leader_generator = MR_FALSE;
        record_ptr_next++;

        if (cur_maxfr == MR_gen_stack[cur_gen].MR_gen_frame) {
            for (i = 0; i < record_ptr_next; i++) {
                if (record_ptrs[i].record_leader == subgoal) {
                    record_ptrs[i].found_leader_generator = MR_TRUE;
                }
            }

            cur_gen--;
        }
    }

    any_missing_generators = MR_FALSE;
    for (i = 0; i < record_ptr_next; i++) {
        if (! record_ptrs[i].found_leader_generator) {
            any_missing_generators = MR_TRUE;
        }
    }

    if (any_missing_generators) {
        *problem = "retry would interfere with minimal model tabling";
        return MR_RETRY_ERROR;
    } else if (record_ptr_next > 0) {
        if (event_info->MR_trace_port == MR_PORT_EXCEPTION
            && ancestor_level == 0)
        {
            *problem = "cannot retry minimal model procedure "
                "from the exception port";
            return MR_RETRY_ERROR;
        }

        label_layout = event_info->MR_event_sll;
        proc_layout = label_layout->MR_sll_entry;

        if (MR_sle_eval_method(proc_layout)
            == MR_EVAL_METHOD_MINIMAL_STACK_COPY)
        {
            return MR_RETRY_OK_FAIL_FIRST;
        } else {
            return MR_RETRY_OK_FINISH_FIRST;
        }
    } else {
        return MR_RETRY_OK_DIRECT;
    }
}

#endif  // MR_USE_MINIMAL_MODEL_STACK_COPY

////////////////////////////////////////////////////////////////////////////

// The rest of this file implements the mechanism that the retry command uses
// to reset the call tables of any calls being retried over.
//
// Each execution of MR_trace_retry is required to make the following
// sequence of calls on this submodule:
//
//  one call to MR_init_call_table_array
//  zero or more calls to MR_maybe_record_call_table
//  one call to either
//      MR_reset_call_table_array (if the direct retry is successful)
//      MR_abandon_call_table_array (if it is not)

static  MR_TrieNode *MR_call_table_ptrs;
static  int         MR_call_table_ptr_max;
static  int         MR_call_table_ptr_next;

#define INIT_CALL_TABLE_ARRAY_SIZE  10

static void
MR_init_call_table_array(void)
{
    MR_call_table_ptr_max = 0;
    MR_call_table_ptr_next = 0;
}

static void
MR_maybe_record_call_table(const MR_ProcLayout *level_layout,
    MR_Word *base_sp, MR_Word *base_curfr)
{
    MR_TrieNode         call_table;
    MR_EvalMethod       eval_method;
    const MR_UserProcId *user;

    if (! MR_PROC_LAYOUT_HAS_EXEC_TRACE(level_layout)) {
        if (MR_PROC_LAYOUT_HAS_PROC_ID(level_layout)) {
            if (! MR_PROC_LAYOUT_IS_UCI(level_layout)) {
                user = &level_layout->MR_sle_user;
                if (MR_streq(user->MR_user_decl_module, "exception") &&
                    MR_streq(user->MR_user_name, "builtin_catch") &&
                    (user->MR_user_arity == 3))
                {
                    // builtin_catch doesn't fill in the MR_sle_exec_trace
                    // field, but we know its evaluation method, so we return.

                    return;
                }
            }
        }
    }

    if (! MR_PROC_LAYOUT_HAS_EXEC_TRACE(level_layout)) {
        // The exec trace seems to have disappeared since the call
        // to MR_undo_updates_of_maxfr ...

        MR_fatal_error("proc layout without exec trace "
            "in MR_maybe_record_call_table");
    }

    eval_method = MR_sle_eval_method(level_layout);
    switch (eval_method) {

    case MR_EVAL_METHOD_NORMAL:
        // Nothing to do.
        return;

    case MR_EVAL_METHOD_MEMO:
    case MR_EVAL_METHOD_LOOP_CHECK:
        if (MR_DETISM_DET_STACK(level_layout->MR_sle_detism)) {
            call_table = (MR_TrieNode) MR_based_stackvar(base_sp,
                level_layout->MR_sle_maybe_call_table);
        } else {
            if (eval_method == MR_EVAL_METHOD_LOOP_CHECK) {
                call_table = (MR_TrieNode) MR_based_framevar(base_curfr,
                    level_layout->MR_sle_maybe_call_table);
            } else {
                MR_MemoNonRecordPtr record;

                record = (MR_MemoNonRecordPtr) MR_based_framevar(base_curfr,
                    level_layout->MR_sle_maybe_call_table);
                call_table = record->MR_mn_back_ptr;

#ifdef  MR_DEBUG_RETRY
                printf("reset: memo non record %p, call_table %p\n",
                    record, call_table);
#endif
            }
        }

        if (call_table != NULL) {
            MR_GC_ensure_room_for_next(MR_call_table_ptr, MR_TrieNode,
                INIT_CALL_TABLE_ARRAY_SIZE, NULL);

            MR_call_table_ptrs[MR_call_table_ptr_next] = call_table;
            MR_call_table_ptr_next++;
        }

        return;

    case MR_EVAL_METHOD_MINIMAL_OWN_STACKS_CONSUMER:
        MR_fatal_error(
            "retry with MR_EVAL_METHOD_MINIMAL_OWN_STACKS_CONSUMER: "
            "not yet implemented");

    case MR_EVAL_METHOD_MINIMAL_OWN_STACKS_GENERATOR:
        MR_fatal_error(
            "retry with MR_EVAL_METHOD_MINIMAL_OWN_STACKS_GENERATOR: "
            "not yet implemented");

    case MR_EVAL_METHOD_MINIMAL_STACK_COPY:
        // We want to process all the minimal model calls whose stack frames
        // are in the part of the nondet stack to be removed by the retry
        // operation, not just those which are direct ancestors of the current
        // call. Such calls are therefore processed in
        // MR_check_minimal_model_calls, not here.

        return;

    case MR_EVAL_METHOD_TABLE_IO:
    case MR_EVAL_METHOD_TABLE_IO_DECL:
    case MR_EVAL_METHOD_TABLE_IO_UNITIZE:
    case MR_EVAL_METHOD_TABLE_IO_UNITIZE_DECL:
        return;
    }

    {
        char    buf[256];

        sprintf(buf,
            "unknown evaluation method %d in MR_maybe_record_call_table",
            MR_sle_eval_method(level_layout));
        MR_fatal_error(buf);
    }
}

static void
MR_reset_call_table_array(void)
{
    int i;

    for (i = 0; i < MR_call_table_ptr_next; i++) {
#ifdef  MR_DEBUG_RETRY
        printf("resetting call table ptr %d (%x)\n",
            (MR_Integer) MR_call_table_ptrs[i],
            (MR_Integer) MR_call_table_ptrs[i]);
#endif
        MR_call_table_ptrs[i]->MR_integer = 0;
    }

    MR_abandon_call_table_array();
}

static void
MR_abandon_call_table_array(void)
{
    if (MR_call_table_ptrs != NULL) {
        MR_GC_free_attrib(MR_call_table_ptrs);
        MR_call_table_ptrs = NULL;
    }
}

void
MR_trace_init_modules(void)
{
    MR_do_init_modules();
    MR_do_init_modules_type_tables();
    MR_do_init_modules_debugger();
}
