// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2009, 2011 The University of Melbourne.
// Copyright (C) 2014-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// Main authors: Mark Brown, Ian MacLarty
//
// This file implements the back end of the declarative debugger. The back end
// is an extension to the internal debugger which collects related trace events
// and builds them into an annotated trace. Once built, the structure is
// passed to the front end where it can be analysed to find bugs. The front end
// is implemented in browser/declarative_debugger.m.
//
// The interface between the front and back ends is via the typeclass
// annotated_trace/2, which is documented in browser/declarative_debugger.m.
// It would be possible to replace the front end or the back end with an
// alternative implementation which also conforms to the typeclass constraints.
// For example:
//
// - An alternative back end could generate the same tree structure in a
//   different way, such as via program transformation.
// - An alternative front end could graphically display the generated trees
//   as part of a visualization tool rather than analyzing them for bugs.
//
// The back end decides which events should be included in the annotated trace.
// The back end can be called multiple times to materialize different portions
// of the annotated trace. It is the responsibility of the front end to
// connect the various portions of the annotated trace together into a
// complete tree. This is done in declarative_edt.m.
//
// Overview of the declarative debugger back end
// --------------------------------------------
//
// Building of a new portion of the annotated trace is started when either the
// user issues a `dd' command from the procedural debugger, or the front end
// requests that a new explicit subtree or supertree be built.
//
// Initially the trace is materialized down to a predefined depth, given by the
// value of MR_edt_default_depth_limit. We retry to the CALL event
// corresponding to the EXIT, FAIL or EXCP event where the `dd' command was
// issued and rerun the program, collecting all events above the depth limit.
// Once we get to the EXIT, FAIL or EXCP event where the `dd' command was
// issued, we call the front end (in browser/declarative_debugger.m) and ask it
// to analyse the generated trace. The front end then returns with one of
// three possible responses:
//
// 1. The front end wants control to return to the procedural debugger.
//    This could be because the bug has been found, or the user has
//    exited the declarative debugging session.
// 2. The front end wants the subtree of a specific node in the annotated
//    trace. Here the front end will tell the back end what depth it wants
//    the new subtree built to.
// 3. The front end wants nodes generated above the currently materialized
//    portion of the annotated trace (referred to here as a supertree).
//
// In case 1 the front end may want control returned to the procedural debugger
// at an event other than the event where the `dd' command was issued. If this
// is the case then we perform a retry to get to an event before the desired
// event and then simulate a `goto' command, so that mdb prompts the user
// at the desired event.
//
// In case 2 the front end will supply the event number and call sequence
// number of the EXIT, FAIL or EXCP event at the root of the required subtree.
// The back end will then retry to a CALL event before or equal to the CALL
// with the given sequence number. The program is then reexecuted and nodes
// are added to the annotated trace to the depth limit specified by the
// front end.
//
// If, while the program is being reexecuted, the call depth exceeds the depth
// limit, then we record (in the array MR_edt_implicit_subtree_counters) how
// many events are at each depth below the depth limit. The data collected is
// used to populate a field at each CALL event that is the root of an implicit
// (unmaterialized) subtree in the annotated trace. The field (called the
// ideal depth) gives the maximum depth to build the subtree to so that no more
// than MR_edt_desired_nodes_in_subtree nodes are materialized. The front end
// passes the ideal depth to the back end when requesting a new subtree.
//
// In case 3 the front end will supply the event number and call sequence
// number of the EXIT, FAIL or EXCP event at the root of the currently
// materialized tree. As with case 2 the back end will retry to a CALL before
// the given call sequence number and start reexecuting the program, however
// no events are added to the annotated trace yet. When execution reaches the
// CALL event matching the sequence number given by the front end, another
// retry is performed to get MR_edt_default_depth_limit levels above the
// currently materialized tree. Execution is then restarted from this point
// and collection of events begins. Events are collected down to the depth of
// the root of the previously materialized tree as illustrated in the following
// diagram.
//
//                     /\     |
//                    /  \    |- Newly materialized supertree
//                   /    \   |
//                     /\       |
//                    /  \      |
//                   /    \     |- Previously materialized tree
//                  /      \    |
//                 /        \   |

#include "mercury_imp.h"
#include "mercury_trace_declarative.h"

#include "mercury_init.h"               // for MR_trace_real
#include "mercury_trace.h"
#include "mercury_trace_browse.h"
#include "mercury_trace_help.h"
#include "mercury_trace_internal.h"
#include "mercury_trace_tables.h"
#include "mercury_trace_util.h"
#include "mercury_trace_vars.h"

#include "mercury_layout_util.h"
#include "mercury_deep_copy.h"
#include "mercury_stack_trace.h"
#include "mercury_string.h"
#include "mercury_timing.h"
#include "mercury_trace_base.h"
#include "mercury_runtime_util.h"

#include "mdb.declarative_debugger.mh"
#include "mdb.declarative_execution.mh"
#include "mdbcomp.slice_and_dice.mh"

#include "benchmarking.mh"
#include "gc.mh"
#include "std_util.mh"

#include <errno.h>

// These macros are to aid debugging of the code which constructs
// the annotated trace.

#ifdef MR_DEBUG_DD_BACK_END
#define MR_DEBUG_DD_BACK_END_EVENT
#define MR_DEBUG_DD_BACK_END_FILTER
#define MR_DEBUG_DD_BACK_END_LINKS
#define MR_DEBUG_DD_BACK_END_ALLOC
#define MR_DEBUG_DD_BACK_END_PASSES
#endif

#ifdef  MR_DEBUG_DD_BACK_END_EVENT
#define MR_decl_checkpoint_event(event_info)                            \
        MR_decl_checkpoint_event_imp("EVENT", MR_TRUE, event_info)
#else
#define MR_decl_checkpoint_event(event_info)        ((void) 0)
#endif

#ifdef  MR_DEBUG_DD_BACK_END_FILTER
#define MR_decl_checkpoint_filter(event_info)                           \
        MR_decl_checkpoint_event_imp("FILTER", MR_TRUE, event_info)
#else
#define MR_decl_checkpoint_filter(event_info)       ((void) 0)
#endif

#ifdef  MR_DEBUG_DD_BACK_END_LINKS
#define MR_decl_checkpoint_find(location)                               \
        MR_decl_checkpoint_loc("FIND", location)
#define MR_decl_checkpoint_step(location)                               \
        MR_decl_checkpoint_loc("STEP", location)
#define MR_decl_checkpoint_match(location)                              \
        MR_decl_checkpoint_loc("MATCH", location)
#else
#define MR_decl_checkpoint_find(location)           ((void) 0)
#define MR_decl_checkpoint_step(location)           ((void) 0)
#define MR_decl_checkpoint_match(location)          ((void) 0)
#endif

#ifdef  MR_DEBUG_DD_BACK_END_ALLOC
#define MR_decl_checkpoint_alloc(location)                              \
        MR_decl_checkpoint_loc("ALLOC", location)
#else
#define MR_decl_checkpoint_alloc(location)          ((void) 0)
#endif

#ifdef  MR_DEBUG_DD_BACK_END_PASSES
#define MR_decl_checkpoint_pass(msg, print, event_info)                 \
        MR_decl_checkpoint_event_imp(msg, print, event_info)
#define MR_decl_checkpoint_subtree(final, top_seqno, depth)             \
        MR_decl_checkpoint_tree("SUBTREE", final, top_seqno, depth)
#define MR_decl_checkpoint_supertree(final, top_seqno, depth)           \
        MR_decl_checkpoint_tree("SUPERTREE", final, top_seqno, depth)
#else
#define MR_decl_checkpoint_pass(msg, print, event_info)         ((void) 0)
#define MR_decl_checkpoint_subtree(final, top_seqno, depth)     ((void) 0)
#define MR_decl_checkpoint_supertree(final, top_seqno, depth)   ((void) 0)
#endif

// If this macro is defined then some statistics, such as how many nodes were
// added to the annotated trace in the current run and the amount of memory
// consumed so far,  will be printed to stderr whenever a new subtree or
// supertree is built.

#ifdef MR_DD_PRINT_EDT_STATS

static  MR_Unsigned         MR_edt_stats_total_constructed_nodes = 0;
static  MR_Unsigned         MR_edt_stats_constructed_nodes_this_time = 0;
static  MR_Unsigned         MR_edt_stats_total_reexecutions = 0;

#define MR_decl_maybe_print_edt_stats()         MR_decl_print_edt_stats()
#define MR_decl_maybe_inc_constructed_nodes()   MR_decl_inc_constructed_nodes()

#else // MR_DD_PRINT_EDT_STATS

#define MR_decl_maybe_print_edt_stats()
#define MR_decl_maybe_inc_constructed_nodes()

#endif // MR_DD_PRINT_EDT_STATS

// The declarative debugger back end is controlled by the
// settings of the following variables. They are set in
// MR_trace_start_decl_debug when the back end is started.
// They are used by MR_trace_decl_debug to decide what action to
// take for a particular trace event.

static  MR_DeclMode         MR_decl_mode = MR_DECL_NODUMP;

// If we are building a subtree then reaching this event will cause the
// declarative debugger to continue its analysis. Reaching this event means
// generation of the desired subtree is complete. This variable is not used
// when building a new explicit supertree.

static  MR_Unsigned         MR_edt_last_event;

// If we are materializing a new subtree then MR_edt_start_seqno is the
// call sequence number of the call at the top of the subtree we want to
// materialize. If we are building a new supertree then MR_edt_start_seqno
// is the call sequence number of the call at the top of the existing
// materialized portion of the annotated trace.

static  MR_Unsigned         MR_edt_start_seqno;

// This tells MR_trace_decl_debug whether it is inside a portion of the
// annotated trace that should be materialized (ignoring any depth limit).
// It has opposite meanings depending on whether an explicit supertree or
// subtree has been requested. When materializing a subtree, it will be true
// for all nodes in the subtree. When materializing a supertree, it will be
// true for all nodes outside the subtree above which a supertree was requested
// (we don't want to include nodes in the subtree because that has already been
// materialized).

static  MR_bool             MR_edt_inside;

// The initial event at which the `dd' command was given. This is used when
// aborting diagnosis to return the user to the event where they initiated
// the declarative debugging session.

static  MR_Unsigned         MR_edt_initial_event;

// The node returned to the front end once a subtree or supertree has been
// generated. If a supertree is generated then the implicit root in the
// new supertree that represents the existing tree is returned, otherwise
// the root of the new explicit subtree is returned.

static  MR_TraceNode        MR_edt_return_node;

// The time (in milliseconds since the start of the program) when collection of
// events, for the current portion of the annotated trace being materialized,
// started.

static  MR_Unsigned         MR_edt_start_time;

// This global keeps track of how many ticks have been printed so far in
// the progress message.

static  MR_Unsigned         MR_edt_progress_last_tick = 0;

// The first event executed during the current re-execution.

static  MR_Unsigned         MR_edt_first_event;

// The declarative debugger ignores modules that were not compiled with
// the required information. However, this may result in incorrect
// assumptions being made about the code, so the debugger gives a warning
// if this happens. The following flag indicates whether a warning
// should be printed before calling the front end.

static  MR_bool             MR_edt_compiler_flag_warning;

// When building a supertree there will be 2 retries. The first will
// retry to an event before the topmost node of the currently materialized
// tree and the second will be a retry from the topmost node to the root
// of the new supertree. This global records whether the user said it
// was safe to do the first retry across untabled IO. If they said this was
// okay then there is no point asking them again for the second retry.

static  MR_bool             MR_edt_unsafe_retry_already_asked;

// If trace counts are provided for failing and passing test cases, then
// we add the suspicion (an integer between 0 and MR_TRACE_DECL_MAX_SUSPICION)
// for each event to the accumulator below. We then store the value of the
// accumulator at CALL, EXIT, REDO, FAIL and EXCP events, which allows
// the frontend to easily calculate the suspicion of any subtree in the EDT.

static  MR_Integer  MR_edt_suspicion_accumulator;
static  MR_bool     MR_edt_update_suspicion_accumulator = MR_FALSE;

// This is used as the abstract map from node identifiers to nodes
// in the data structure passed to the front end. It should be
// incremented each time the data structure is destructively
// updated, before being passed to Mercury code again.

static  MR_Unsigned         MR_trace_node_store;

// The front end state is stored here in between calls to it.
// MR_trace_decl_ensure_init should be called before using the state.

static  MR_Word             MR_trace_front_end_state;

static  void                MR_trace_decl_ensure_init(void);

// MR_trace_current_node always contains the last node allocated,
// or NULL if the collection has just started.

static  MR_TraceNode        MR_trace_current_node;

// When in test mode, MR_trace_store_file points to an open file to
// which the store should be written when built. This global is set
// in MR_trace_start_decl_debug, and keeps the same value
// throughout the declarative debugging session.

static  FILE                *MR_trace_store_file;

////////////////////////////////////////////////////////////////////////////

static    MR_TraceNode      MR_trace_decl_call(MR_EventInfo *event_info,
                                MR_TraceNode prev);
static    MR_TraceNode      MR_trace_decl_exit(MR_EventInfo *event_info,
                                MR_TraceNode prev);
static    MR_TraceNode      MR_trace_decl_redo(MR_EventInfo *event_info,
                                MR_TraceNode prev);
static    MR_TraceNode      MR_trace_decl_fail(MR_EventInfo *event_info,
                                MR_TraceNode prev);
static    MR_TraceNode      MR_trace_decl_excp(MR_EventInfo *event_info,
                                MR_TraceNode prev);
static    MR_TraceNode      MR_trace_decl_switch(MR_EventInfo *event_info,
                                MR_TraceNode prev);
static    MR_TraceNode      MR_trace_decl_disj_first(MR_EventInfo *event_info,
                                MR_TraceNode prev);
static    MR_TraceNode      MR_trace_decl_disj_later(MR_EventInfo *event_info,
                                MR_TraceNode prev);
static    MR_TraceNode      MR_trace_decl_cond(MR_EventInfo *event_info,
                                MR_TraceNode prev);
static    MR_TraceNode      MR_trace_decl_then(MR_EventInfo *event_info,
                                MR_TraceNode prev);
static    MR_TraceNode      MR_trace_decl_else(MR_EventInfo *event_info,
                                MR_TraceNode prev);
static    MR_TraceNode      MR_trace_decl_neg_enter(MR_EventInfo *event_info,
                                MR_TraceNode prev);
static    MR_TraceNode      MR_trace_decl_neg_success(
                                MR_EventInfo *event_info, MR_TraceNode prev);
static    MR_TraceNode      MR_trace_decl_neg_failure(
                                MR_EventInfo *event_info, MR_TraceNode prev);
static    MR_TraceNode      MR_trace_matching_call(MR_TraceNode node);
static    MR_bool           MR_trace_first_disjunct(MR_EventInfo *event_info);
static    MR_bool           MR_trace_matching_cond(const char *path,
                                MR_TraceNode node);
static    MR_bool           MR_trace_matching_neg(const char *path,
                                MR_TraceNode node);
static    MR_bool           MR_trace_matching_disj(const char *path,
                                MR_TraceNode node);
static    MR_bool           MR_trace_same_construct(const char *p1,
                                const char *p2);
static    MR_bool           MR_trace_single_component(const char *path);
static    MR_Word           MR_decl_make_atom_args(
                                const MR_LabelLayout *layout,
                                MR_Word *saved_regs, MR_Float *saved_f_regs,
                                MR_TracePort port);
static    MR_Word           MR_decl_atom_args(const MR_LabelLayout *layout,
                                MR_Word *saved_regs);
static    const char        *MR_trace_start_collecting(MR_Unsigned event,
                                MR_Unsigned seqno, MR_Unsigned maxdepth,
                                MR_bool create_supertree,
                                MR_TraceCmdInfo *cmd,
                                MR_EventInfo *event_info,
                                MR_Code **jumpaddr);
static    MR_Code           *MR_trace_restart_decl_debug(
                                MR_TraceNode call_preceding,
                                MR_Unsigned event,
                                MR_Unsigned seqno,
                                MR_bool create_supertree,
                                MR_Unsigned depth_limit,
                                MR_TraceCmdInfo *cmd,
                                MR_EventInfo *event_info);
static    MR_Code           *MR_decl_diagnosis(MR_TraceNode root,
                                MR_TraceCmdInfo *cmd,
                                MR_EventInfo *event_info,
                                MR_bool new_tree);
static    MR_Code           *MR_decl_go_to_selected_event(MR_Unsigned event,
                                MR_TraceCmdInfo *cmd,
                                MR_EventInfo *event_info);
static    MR_Code           *MR_trace_decl_retry_supertree(
                                MR_Unsigned max_distance,
                                MR_EventInfo *event_info);
static    MR_String         MR_trace_node_path(MR_TraceNode node);
static    MR_TracePort      MR_trace_node_port(MR_TraceNode node);
static    MR_Unsigned       MR_trace_node_seqno(MR_TraceNode node);
static    MR_TraceNode      MR_trace_node_first_disj(MR_TraceNode node);
static    MR_TraceNode      MR_trace_step_left_in_contour(MR_TraceNode node);
static    MR_TraceNode      MR_trace_find_prev_contour(MR_TraceNode node);
static    void              MR_decl_checkpoint_event_imp(const char *str,
                                MR_bool print_event, MR_EventInfo *event_info);
static    void              MR_decl_checkpoint_loc(const char *str,
                                MR_TraceNode node);
static    void              MR_decl_checkpoint_tree(const char *tree_kind,
                                MR_Unsigned final_event,
                                MR_Unsigned top_seqno,
                                MR_Unsigned depth_limit);
static    void              MR_decl_print_edt_stats(void);
static    void              MR_decl_inc_constructed_nodes(void);
static    void              MR_trace_edt_build_sanity_check(
                                MR_EventInfo *event_info,
                                const MR_ProcLayout *entry);
static    MR_bool           MR_trace_include_event(const MR_ProcLayout *entry,
                                MR_EventInfo *event_info,
                                MR_Code **jumpaddr);
static    MR_Unsigned       MR_trace_calculate_event_depth(
                                MR_EventInfo *event_info);
static    void              MR_trace_construct_node(MR_EventInfo *event_info);
static    void              MR_trace_count_event_in_implicit_subtree(
                                MR_EventInfo *event_info, MR_Unsigned depth);
static    void              MR_trace_maybe_update_implicit_tree_ideal_depth(
                                MR_Unsigned depth, MR_TraceNode call);
static    void              MR_trace_finish_progress(void);
static    void              MR_trace_init_implicit_subtree_counters(
                                MR_Unsigned size);
static    void              MR_trace_reset_implicit_subtree_counters(void);
static    void              MR_trace_free_implicit_subtree_counters(void);
static    MR_Unsigned       MR_trace_calc_implicit_subtree_ideal_depth(void);
static    void              MR_trace_maybe_update_suspicion_accumulator(
                                const MR_LabelLayout *label_layout);

MR_bool         MR_trace_decl_assume_all_io_is_tabled = MR_FALSE;

MR_Unsigned MR_edt_desired_nodes_in_subtree = MR_TRACE_DESIRED_SUBTREE_NODES;
MR_Unsigned MR_edt_default_depth_limit = MR_TRACE_DECL_INITIAL_DEPTH;

MR_bool         MR_trace_decl_debug_debugger_mode = MR_FALSE;

MR_Integer      MR_edt_depth;
MR_Unsigned     MR_edt_max_depth;
MR_Integer      MR_edt_implicit_subtree_depth;

MR_Unsigned     *MR_edt_implicit_subtree_counters;
MR_Unsigned     MR_edt_implicit_subtree_num_counters;

MR_bool         MR_edt_building_supertree;

// This function is called for every traced event when building the
// annotated trace. It must decide which events are included in the
// annotated trace.

MR_Code *
MR_trace_decl_debug(MR_EventInfo *event_info)
{
    const MR_ProcLayout     *entry;
    MR_Unsigned             node_depth;
    MR_Unsigned             call_seqno;
    MR_TracePort            port;
    MR_Code                 *jumpaddr;

    call_seqno = event_info->MR_call_seqno;
    port = event_info->MR_trace_port;
    entry = event_info->MR_event_sll->MR_sll_entry;

    MR_trace_edt_build_sanity_check(event_info, entry);
    MR_trace_maybe_update_suspicion_accumulator(event_info->MR_event_sll);
    MR_DECL_MAYBE_UPDATE_PROGRESS(MR_trace_event_number);

    if (! MR_trace_include_event(entry, event_info, &jumpaddr)) {
        return jumpaddr;
    }

    MR_DD_CALC_NODE_DEPTH(port, node_depth, MR_edt_depth);
    if (node_depth == MR_edt_max_depth
        && (port == MR_PORT_CALL || port == MR_PORT_REDO))
    {
        // We are entering the top of an implicit subtree. Switch to the
        // event handler that processes the notes in the implicit subtree,
        // and reset the data structures it works with.

        MR_decl_checkpoint_pass("SWITCHING TO IMPLICIT SUBTREE", MR_FALSE,
            event_info);
        MR_trace_reset_implicit_subtree_counters();
        MR_edt_implicit_subtree_counters[0]++;
        MR_edt_implicit_subtree_depth = 0;
        MR_selected_trace_func_ptr = MR_trace_real_decl_implicit_subtree;
    }

    MR_trace_construct_node(event_info);

    if (call_seqno == MR_edt_start_seqno && MR_port_is_final(port)) {
        MR_edt_return_node = MR_trace_current_node;
    }

    if ((!MR_edt_building_supertree &&
            MR_trace_event_number == MR_edt_last_event)
        || (MR_edt_building_supertree && node_depth == 0
            && MR_port_is_final(port)))
    {
        MR_trace_free_implicit_subtree_counters();
        MR_decl_maybe_print_edt_stats();
        if (MR_edt_progress_last_tick > 0) {
            MR_trace_finish_progress();
        }

        MR_decl_checkpoint_pass("CALL FRONTEND", MR_FALSE, event_info);
        return MR_decl_diagnosis(MR_edt_return_node, &MR_trace_ctrl,
            event_info, MR_TRUE);
    }

    return NULL;
}

static void
MR_trace_edt_build_sanity_check(MR_EventInfo *event_info,
    const MR_ProcLayout *entry)
{
    if (event_info->MR_event_number > MR_edt_last_event
        && !MR_edt_building_supertree)
    {
        // This shouldn't ever be reached.
        fprintf(MR_mdb_err, "Error: missed final event.\n");
        fprintf(MR_mdb_err, "event %lu\nlast event %lu\n",
            (unsigned long) event_info->MR_event_number,
            (unsigned long) MR_edt_last_event);
        fflush(NULL);
        MR_fatal_error("Aborting.");
    }

    if (!MR_PROC_LAYOUT_HAS_EXEC_TRACE(entry)) {
        // XXX this should be handled better.
        MR_fatal_error("layout has no execution tracing");
    }
}

static MR_bool
MR_trace_include_event(const MR_ProcLayout *entry,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    // Filter out events for compiler generated procedures.

    if (MR_PROC_LAYOUT_IS_UCI(entry)) {
        *jumpaddr = NULL;
        return MR_FALSE;
    }

    if (entry->MR_sle_module_layout->MR_ml_suppressed_events != 0) {
        // We ignore events from modules that were not compiled with the
        // necessary information. Procedures in those modules are effectively
        // assumed correct, so we give the user a warning.

        MR_edt_compiler_flag_warning = MR_TRUE;
        *jumpaddr = NULL;
        return MR_FALSE;
    }

    // Decide if we are inside or outside the subtree or supertree that
    // needs to be materialized, ignoring for now any depth limit.
    // If we are materializing a supertree then MR_edt_inside will
    // be true whenever we are not in the subtree rooted at the call
    // corresponding to MR_edt_start_seqno. If we are materializing a
    // subtree then MR_edt_inside will be true whenever we are in the
    // subtree rooted at the call corresponding to MR_edt_start_segno.

    if (MR_edt_building_supertree) {
        if (!MR_edt_inside) {
            if (event_info->MR_call_seqno == MR_edt_start_seqno &&
                MR_port_is_final(event_info->MR_trace_port))
            {
                // We are exiting the subtree rooted at MR_edt_start_seqno.

                MR_decl_checkpoint_pass("SUPERTREE: SWITCHING TO INSIDE",
                    MR_FALSE, event_info);
                MR_edt_inside = MR_TRUE;
                return MR_TRUE;
            } else if (event_info->MR_call_seqno == MR_edt_start_seqno
                && MR_port_is_entry(event_info->MR_trace_port))
            {
                // We are entering the top of the currently materialized
                // portion of the annotated trace. Since we are building
                // a supertree, we must retry to above the current event
                // and start building the new portion of the annotated trace
                // from there.

                MR_decl_checkpoint_pass("SUPERTREE: RETRYING", MR_TRUE,
                    event_info);
                MR_edt_inside = MR_TRUE;
                *jumpaddr = MR_trace_decl_retry_supertree(MR_edt_max_depth,
                    event_info);

                // Reset the depth since we will now be at the top of the
                // supertree to be materialized. We set it to -1 since the
                // next call to MR_trace_decl_debug will set it to 0.

                MR_edt_depth = -1;
                return MR_FALSE;
            } else {
                // We are in an existing explicit subtree.

                MR_decl_checkpoint_pass("SUPERTREE: FILTER", MR_TRUE,
                    event_info);
                MR_decl_checkpoint_filter(event_info);
                *jumpaddr = NULL;
                return MR_FALSE;
            }
        } else {
            if (event_info->MR_call_seqno == MR_edt_start_seqno) {
                // The port must be either CALL or REDO; we are leaving the
                // supertree and entering the existing explicit subtree.
                // We must still however add this node to the generated EDT.

                MR_decl_checkpoint_pass("SUPERTREE: SWITCHING TO OUTSIDE",
                    MR_FALSE, event_info);
                MR_edt_inside = MR_FALSE;
                return MR_TRUE;
            }
        }
    } else {
        if (MR_edt_inside) {
            if (event_info->MR_call_seqno == MR_edt_start_seqno
                && MR_port_is_final(event_info->MR_trace_port))
            {
                // We are leaving the topmost call.

                MR_decl_checkpoint_pass("INSIDE: SWITCHING TO OUTSIDE",
                    MR_FALSE, event_info);
                MR_edt_inside = MR_FALSE;
                return MR_TRUE;
            }
        } else {
            if (event_info->MR_call_seqno == MR_edt_start_seqno) {
                // The port must be either CALL or REDO;
                // we are (re)entering the topmost call.

                MR_decl_checkpoint_pass("INSIDE: REENTERING TOPMOST",
                    MR_FALSE, event_info);
                MR_edt_inside = MR_TRUE;
                MR_edt_depth = -1;
                return MR_TRUE;
            } else {
                // Ignore this event -- it is outside the topmost call.

                MR_decl_checkpoint_pass("INSIDE: FILTERING", MR_TRUE,
                    event_info);
                MR_decl_checkpoint_filter(event_info);
                *jumpaddr = NULL;
                return MR_FALSE;
            }
        }
    }

    return MR_TRUE;
}

static void
MR_trace_construct_node(MR_EventInfo *event_info)
{
    MR_TraceNode    trace;

    trace = MR_trace_current_node;

    MR_debug_enabled = MR_FALSE;
    MR_update_trace_func_enabled();

    MR_decl_checkpoint_event(event_info);
    switch (event_info->MR_trace_port) {
        case MR_PORT_CALL:
            trace = MR_trace_decl_call(event_info, trace);
            break;

        case MR_PORT_EXIT:
            trace = MR_trace_decl_exit(event_info, trace);
            break;

        case MR_PORT_REDO:
            trace = MR_trace_decl_redo(event_info, trace);
            break;

        case MR_PORT_FAIL:
            trace = MR_trace_decl_fail(event_info, trace);
            break;

        case MR_PORT_DISJ_FIRST:
            trace = MR_trace_decl_disj_first(event_info, trace);
            break;

        case MR_PORT_DISJ_LATER:
            trace = MR_trace_decl_disj_later(event_info, trace);
            break;

        case MR_PORT_SWITCH:
            trace = MR_trace_decl_switch(event_info, trace);
            break;

        case MR_PORT_COND:
            trace = MR_trace_decl_cond(event_info, trace);
            break;

        case MR_PORT_THEN:
            trace = MR_trace_decl_then(event_info, trace);
            break;

        case MR_PORT_ELSE:
            trace = MR_trace_decl_else(event_info, trace);
            break;

        case MR_PORT_NEG_ENTER:
            trace = MR_trace_decl_neg_enter(event_info, trace);
            break;

        case MR_PORT_NEG_SUCCESS:
            trace = MR_trace_decl_neg_success(event_info, trace);
            break;

        case MR_PORT_NEG_FAILURE:
            trace = MR_trace_decl_neg_failure(event_info, trace);
            break;

        case MR_PORT_EXCEPTION:
            trace = MR_trace_decl_excp(event_info, trace);
            break;

        case MR_PORT_USER:
            // do nothing
            break;

        case MR_PORT_TAILREC_CALL:
        default:
            MR_fatal_error("MR_trace_construct_node: unknown port");
    }

    MR_decl_checkpoint_alloc(trace);
    MR_decl_maybe_inc_constructed_nodes();

    MR_debug_enabled = MR_TRUE;
    MR_update_trace_func_enabled();

    MR_trace_current_node = trace;
}

// Retry max_distance if there are that many ancestors, otherwise
// retry as far as possible.

static MR_Code *
MR_trace_decl_retry_supertree(MR_Unsigned max_distance,
    MR_EventInfo *event_info)
{
    MR_Code             *jumpaddr;
    int                 retry_distance;
    const char          *problem;
    MR_RetryResult      retry_result;
    MR_bool             unsafe_retry;
    MR_RetryAcrossIo    retry_mode;

    if (max_distance >= event_info->MR_call_depth) {
        retry_distance = event_info->MR_call_depth - 1;
    } else {
        retry_distance = max_distance;
    }

    // If the user was already asked if they want to do an unsafe retry
    // while building this supertree, then don't ask them again.

    if (MR_edt_unsafe_retry_already_asked) {
        retry_mode = MR_RETRY_IO_FORCE;
    } else {
        retry_mode = MR_RETRY_IO_INTERACTIVE;
    }

    retry_result = MR_trace_retry(event_info, retry_distance, retry_mode,
        MR_trace_decl_assume_all_io_is_tabled,
        MR_DECL_UNTABLED_IO_RETRY_MESSAGE, &unsafe_retry,
        &problem, MR_mdb_in, MR_mdb_out, &jumpaddr);

    if (retry_result != MR_RETRY_OK_DIRECT) {
        if (retry_result == MR_RETRY_ERROR) {
            MR_selected_trace_func_ptr = MR_trace_real;
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err,
                "mdb: retry aborted in MR_trace_decl_retry_supertree: %s\n",
                problem);
            return NULL;
        } else {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: internal error in "
                "MR_trace_decl_retry_supertree: direct retry impossible\n");
            return NULL;
        }
    }

    return jumpaddr;
}

static MR_TraceNode
MR_trace_decl_call(MR_EventInfo *event_info, MR_TraceNode prev)
{
    MR_TraceNode                node;
    MR_Word                     atom_args;
    MR_bool                     at_depth_limit;
    const MR_LabelLayout        *event_label_layout;
    const MR_ProcLayout         *event_proc_layout;
    const MR_LabelLayout        *return_label_layout;
    MR_StackWalkStepResult      result;
    MR_ConstString              problem;
    MR_Word                     *base_sp;
    MR_Word                     *base_curfr;
    MR_Word                     maybe_return_label;
    MR_Unsigned                 reused_frames;

    if (MR_edt_depth == MR_edt_max_depth) {
        at_depth_limit = MR_TRUE;
    } else {
        at_depth_limit = MR_FALSE;
    }

    event_label_layout = event_info->MR_event_sll;
    event_proc_layout = event_label_layout->MR_sll_entry;
    atom_args = MR_decl_make_atom_args(event_label_layout,
        event_info->MR_saved_regs, event_info->MR_saved_f_regs, MR_PORT_CALL);
    base_sp = MR_saved_sp(event_info->MR_saved_regs);
    base_curfr = MR_saved_curfr(event_info->MR_saved_regs);
    result = MR_stack_walk_step(event_proc_layout, &return_label_layout,
        &base_sp, &base_curfr, &reused_frames, &problem);
    assert(reused_frames == 0);

    // return_label_layout may be NULL even if result is MR_STEP_OK,
    // if the current event is inside the code of main/2.

    MR_TRACE_CALL_MERCURY(
        if (result == MR_STEP_OK && return_label_layout != NULL) {
            maybe_return_label =
                MR_DD_make_yes_maybe_label(return_label_layout);
        } else {
            maybe_return_label = MR_DD_make_no_maybe_label();
        }

        node = (MR_TraceNode) MR_DD_construct_call_node((MR_Word) prev,
            atom_args, (MR_Word) event_info->MR_call_seqno,
            (MR_Word) event_info->MR_event_number,
            (MR_Word) at_depth_limit, maybe_return_label,
            event_label_layout, MR_io_tabling_counter,
            MR_edt_suspicion_accumulator);
    );

    return node;
}

static MR_TraceNode
MR_trace_decl_exit(MR_EventInfo *event_info, MR_TraceNode prev)
{
    MR_TraceNode    node;
    MR_TraceNode    call;
    MR_Word         last_interface;
    MR_Word         atom_args;

    atom_args = MR_decl_make_atom_args(event_info->MR_event_sll,
        event_info->MR_saved_regs, event_info->MR_saved_f_regs, MR_PORT_EXIT);

    call = MR_trace_matching_call(prev);
    MR_decl_checkpoint_match(call);

    MR_TRACE_CALL_MERCURY(
        // We need to add 1 to MR_edt_depth since this is an EXIT event,
        // so 1 should already have been subtracted from MR_edt_depth
        // in MR_trace_calculate_event_depth.

        MR_trace_maybe_update_implicit_tree_ideal_depth(
            MR_edt_depth + 1, call);
        last_interface = MR_DD_call_node_get_last_interface((MR_Word) call);
        node = (MR_TraceNode) MR_DD_construct_exit_node((MR_Word) prev,
            (MR_Word) call, last_interface, atom_args,
            (MR_Word) event_info->MR_event_number, event_info->MR_event_sll,
            MR_io_tabling_counter, MR_edt_suspicion_accumulator);
        MR_DD_call_node_set_last_interface((MR_Word) call, (MR_Word) node);
    );

    return node;
}

static MR_TraceNode
MR_trace_decl_redo(MR_EventInfo *event_info, MR_TraceNode prev)
{
    MR_TraceNode    node;
    MR_TraceNode    call;
    MR_TraceNode    next;
    MR_Word         last_interface;

    // Search through previous contour for a matching EXIT event.

    next = MR_trace_find_prev_contour(prev);
    while (MR_trace_node_port(next) != MR_PORT_EXIT
        || MR_trace_node_seqno(next) != event_info->MR_call_seqno)
    {
        next = MR_trace_step_left_in_contour(next);
    }

    MR_decl_checkpoint_match(next);

    MR_TRACE_CALL_MERCURY(
        MR_trace_node_store++;
        if (!MR_DD_trace_node_call(MR_trace_node_store, (MR_Word) next,
            (MR_Word *) &call))
        {
            MR_fatal_error("MR_trace_decl_redo: no matching EXIT");
        }

        last_interface = MR_DD_call_node_get_last_interface((MR_Word) call);
        node = (MR_TraceNode) MR_DD_construct_redo_node((MR_Word) prev,
            last_interface, (MR_Word) event_info->MR_event_number,
            event_info->MR_event_sll, MR_edt_suspicion_accumulator);
        MR_DD_call_node_set_last_interface((MR_Word) call, (MR_Word) node);
    );

    return node;
}

static MR_TraceNode
MR_trace_decl_fail(MR_EventInfo *event_info, MR_TraceNode prev)
{
    MR_TraceNode    node;
    MR_TraceNode    next;
    MR_TraceNode    call;
    MR_Word         redo;

    if (MR_trace_node_port(prev) == MR_PORT_CALL) {
        // We are already at the corresponding call, so there
        // is no need to search for it.

        call = prev;
    } else {
        next = MR_trace_find_prev_contour(prev);
        call = MR_trace_matching_call(next);
    }
    MR_decl_checkpoint_match(call);

    MR_TRACE_CALL_MERCURY(
        // We need to add 1 to MR_edt_depth since this is a FAIL event,
        // so 1 should already have been subtracted from MR_edt_depth
        // in MR_trace_calculate_event_depth.

        MR_trace_maybe_update_implicit_tree_ideal_depth(
            MR_edt_depth + 1, call);
        redo = MR_DD_call_node_get_last_interface((MR_Word) call);
        node = (MR_TraceNode) MR_DD_construct_fail_node((MR_Word) prev,
            (MR_Word) call, (MR_Word) redo,
            (MR_Word) event_info->MR_event_number, event_info->MR_event_sll,
            MR_edt_suspicion_accumulator);
        MR_DD_call_node_set_last_interface((MR_Word) call, (MR_Word) node);
    );

    return node;
}

static MR_TraceNode
MR_trace_decl_excp(MR_EventInfo *event_info, MR_TraceNode prev)
{
    MR_TraceNode    node;
    MR_TraceNode    call;
    MR_Word         last_interface;

    call = MR_trace_matching_call(prev);
    MR_decl_checkpoint_match(call);

    MR_TRACE_CALL_MERCURY(
        // We need to add 1 to MR_edt_depth since this is an EXCP event,
        // so 1 should already have been subtracted from MR_edt_depth
        // in MR_trace_calculate_event_depth.

        MR_trace_maybe_update_implicit_tree_ideal_depth(MR_edt_depth + 1,
            call);
        last_interface = MR_DD_call_node_get_last_interface((MR_Word) call);
        MR_DD_construct_excp_node((MR_Word) prev, (MR_Word) call,
            last_interface, MR_trace_get_exception_value(),
            (MR_Word) event_info->MR_event_number,
            event_info->MR_event_sll, MR_edt_suspicion_accumulator, &node);
        MR_DD_call_node_set_last_interface((MR_Word) call, (MR_Word) node);
    );

    return node;
}

static MR_TraceNode
MR_trace_decl_cond(MR_EventInfo *event_info, MR_TraceNode prev)
{
    MR_TraceNode    node;

    MR_TRACE_CALL_MERCURY(
        node = (MR_TraceNode) MR_DD_construct_cond_node((MR_Word) prev,
            event_info->MR_event_sll);
    );

    return node;
}

static MR_TraceNode
MR_trace_decl_then(MR_EventInfo *event_info, MR_TraceNode prev)
{
    MR_TraceNode    node;
    MR_TraceNode    next;
    MR_TraceNode    cond;
    const char      *path = event_info->MR_event_path;

    // Search through current contour for a matching COND event.

    next = prev;
    while (!MR_trace_matching_cond(path, next)) {
        next = MR_trace_step_left_in_contour(next);
    }

    cond = next;
    MR_decl_checkpoint_match(cond);

    MR_TRACE_CALL_MERCURY(
        MR_DD_cond_node_set_status((MR_Word) cond, MR_TRACE_STATUS_SUCCEEDED);
        node = (MR_TraceNode) MR_DD_construct_then_node((MR_Word) prev,
            (MR_Word) cond, event_info->MR_event_sll);
    );
    return node;
}

static MR_TraceNode
MR_trace_decl_else(MR_EventInfo *event_info, MR_TraceNode prev)
{
    MR_TraceNode    node;
    MR_TraceNode    cond;
    const char      *path = event_info->MR_event_path;

    // Search through previous contour for a matching COND event.

    if (MR_trace_matching_cond(path, prev)) {
        cond = prev;
    } else {
        MR_TraceNode    next;

        next = prev;
        while (!MR_trace_matching_cond(path, next)) {
            next = MR_trace_step_left_in_contour(next);
        }

        cond = next;
    }
    MR_decl_checkpoint_match(cond);

    MR_TRACE_CALL_MERCURY(
        MR_DD_cond_node_set_status((MR_Word) cond, MR_TRACE_STATUS_FAILED);
        node = (MR_TraceNode) MR_DD_construct_else_node((MR_Word) prev,
            (MR_Word) cond, event_info->MR_event_sll);
    );

    return node;
}

static MR_TraceNode
MR_trace_decl_neg_enter(MR_EventInfo *event_info, MR_TraceNode prev)
{
    MR_TraceNode    node;

    MR_TRACE_CALL_MERCURY(
        node = (MR_TraceNode) MR_DD_construct_neg_node((MR_Word) prev,
            event_info->MR_event_sll);
    );
    return node;
}

static MR_TraceNode
MR_trace_decl_neg_success(MR_EventInfo *event_info, MR_TraceNode prev)
{
    MR_TraceNode    node;
    MR_TraceNode    nege;
    const char      *path = event_info->MR_event_path;

    // Search through previous contour for a matching NEGE event.

    if (MR_trace_matching_neg(path, prev)) {
        nege = MR_trace_current_node;
    } else {
        MR_TraceNode    next;

        next = prev;
        while (!MR_trace_matching_neg(path, next)) {
            next = MR_trace_step_left_in_contour(next);
        }

        nege = next;
    }
    MR_decl_checkpoint_match(nege);

    MR_TRACE_CALL_MERCURY(
        MR_DD_neg_node_set_status((MR_Word) nege, MR_TRACE_STATUS_SUCCEEDED);
        node = (MR_TraceNode) MR_DD_construct_neg_succ_node((MR_Word) prev,
            (MR_Word) nege, event_info->MR_event_sll);
    );

    return node;
}

static MR_TraceNode
MR_trace_decl_neg_failure(MR_EventInfo *event_info, MR_TraceNode prev)
{
    MR_TraceNode    node;
    MR_TraceNode    next;

    // Search through current contour for a matching NEGE event.

    next = prev;
    while (!MR_trace_matching_neg(event_info->MR_event_path, next)) {
        next = MR_trace_step_left_in_contour(next);
    }

    MR_decl_checkpoint_match(next);

    MR_TRACE_CALL_MERCURY(
        MR_DD_neg_node_set_status((MR_Word) next, MR_TRACE_STATUS_FAILED);
        node = (MR_TraceNode) MR_DD_construct_neg_fail_node((MR_Word) prev,
            (MR_Word) next, event_info->MR_event_sll);
    );

    return node;
}

static MR_TraceNode
MR_trace_decl_switch(MR_EventInfo *event_info, MR_TraceNode prev)
{
    MR_TraceNode    node;

    MR_TRACE_CALL_MERCURY(
        node = (MR_TraceNode) MR_DD_construct_switch_node((MR_Word) prev,
            event_info->MR_event_sll);
    );

    return node;
}

static MR_TraceNode
MR_trace_decl_disj_first(MR_EventInfo *event_info, MR_TraceNode prev)
{
    MR_TraceNode    node;

    MR_TRACE_CALL_MERCURY(
        node = (MR_TraceNode) MR_DD_construct_first_disj_node(
            (MR_Word) prev, event_info->MR_event_sll);
    );

    return node;
}

static MR_TraceNode
MR_trace_decl_disj_later(MR_EventInfo *event_info, MR_TraceNode prev)
{
    MR_TraceNode    node;
    const char      *path;
    MR_TraceNode    next;
    MR_TraceNode    first;

    path = event_info->MR_event_path;

    // Search through previous nodes for a matching DISJ event.

    next = MR_trace_find_prev_contour(prev);
    while (!MR_trace_matching_disj(path, next)) {
        next = MR_trace_step_left_in_contour(next);
    }

    MR_decl_checkpoint_match(next);

    // Find the first disj event of this disjunction.

    first = MR_trace_node_first_disj(next);
    if (first == (MR_TraceNode) NULL) {
        first = next;
    }

    MR_TRACE_CALL_MERCURY(
        node = (MR_TraceNode) MR_DD_construct_later_disj_node(
            MR_trace_node_store, (MR_Word) prev, event_info->MR_event_sll,
            (MR_Word) first);
    );

    return node;
}

static MR_TraceNode
MR_trace_matching_call(MR_TraceNode node)
{
    MR_TraceNode    next;

    // Search through contour for any CALL event. Since there is only one
    // CALL event which can be reached, we assume it is the correct one.

    next = node;
    while (MR_trace_node_port(next) != MR_PORT_CALL) {
        next = MR_trace_step_left_in_contour(next);
    }

    return next;
}

static MR_bool
MR_trace_first_disjunct(MR_EventInfo *event_info)
{
    const char  *path;

    // Return MR_TRUE iff the last component of the path is "d1;".

    path = event_info->MR_event_path;
    while (*path) {
        if (MR_string_equal(path, "d1;")) {
            return MR_TRUE;
        }

        path++;
    }

    return MR_FALSE;
}

static MR_bool
MR_trace_matching_cond(const char *path, MR_TraceNode node)
{
    MR_TracePort    port;
    const char      *node_path;

    MR_TRACE_CALL_MERCURY(
        port = (MR_TracePort) MR_DD_trace_node_port(node);
    );

    if (port != MR_PORT_COND) {
        return MR_FALSE;
    }

    node_path = MR_trace_node_path(node);
    return MR_trace_same_construct(path, node_path);
}

static MR_bool
MR_trace_matching_neg(const char *path, MR_TraceNode node)
{
    MR_TracePort    port;
    const char      *node_path;

    MR_TRACE_CALL_MERCURY(
        port = (MR_TracePort) MR_DD_trace_node_port(node);
    );

    if (port != MR_PORT_NEG_ENTER) {
        return MR_FALSE;
    }

    node_path = MR_trace_node_path(node);
    return MR_trace_same_construct(path, node_path);
}

static MR_bool
MR_trace_matching_disj(const char *path, MR_TraceNode node)
{
    MR_TracePort    port;
    const char      *node_path;

    MR_TRACE_CALL_MERCURY(
        port = (MR_TracePort) MR_DD_trace_node_port(node);
    );

    if (port == MR_PORT_DISJ_FIRST || port == MR_PORT_DISJ_LATER) {
        node_path = MR_trace_node_path(node);
        return MR_trace_same_construct(path, node_path);
    } else {
        return MR_FALSE;
    }
}

static MR_bool
MR_trace_same_construct(const char *p1, const char *p2)
{
    // Checks if the two arguments represent goals in the same construct.
    // If both strings are identical up to the last component, return MR_TRUE,
    // otherwise return MR_FALSE. If the arguments point to identical strings,
    // return MR_TRUE.
    //
    // See the comment on the MR_sll_path field in
    // runtime/mercury_stack_layout.h for a possible way to do this test
    // in linear time, and why we currently do not do it that way.

    while (*p1 == *p2) {
        if (*p1 == '\0' && *p2 == '\0') {
            return MR_TRUE;     // They are identical.
        }
        if (*p1 == '\0' || *p2 == '\0') {
            return MR_FALSE;    // Different number of elements.
        }

        p1++;
        p2++;
    }

    // If there is exactly one component left in each string, then
    // the goal paths match, otherwise they don't.

    return MR_trace_single_component(p1) && MR_trace_single_component(p2);
}

static MR_bool
MR_trace_single_component(const char *path)
{
    while (*path != ';') {
        if (*path == '\0') {
            return MR_FALSE;
        }

        path++;
    }

    path++;
    return (*path == '\0');
}

static MR_Word
MR_decl_make_atom_args(const MR_LabelLayout *layout, MR_Word *saved_regs,
    MR_Float *saved_f_regs, MR_TracePort port)
{
    MR_PredFunc             pred_or_func;
    int                     arity;
    MR_Word                 atom_args;
    int                     hv;                 // Any head variable.
    int                     num_added_args;
    const MR_ProcLayout     *entry;

    entry = layout->MR_sll_entry;
    MR_trace_init_point_vars(layout, saved_regs, saved_f_regs, port, MR_TRUE);
    MR_proc_id_arity_addedargs_predfunc(entry, &arity, &num_added_args,
        &pred_or_func);

    MR_TRACE_CALL_MERCURY(
        atom_args = MR_DD_init_trace_atom_args();
    );

    for (hv = entry->MR_sle_num_head_vars - 1; hv >= 0; hv--) {
        int             hlds_num;
        MR_Word         arg;
        MR_TypeInfo     arg_type;
        MR_Word         arg_value;
        MR_bool         is_prog_visible_headvar;
        const char      *problem;

        hlds_num = entry->MR_sle_head_var_nums[hv];

        is_prog_visible_headvar = hv >= num_added_args ? MR_TRUE : MR_FALSE;

        problem = MR_trace_return_hlds_var_info(hlds_num, &arg_type,
                &arg_value);
        if (problem != NULL) {
            // This head variable is not live at this port.
            MR_TRACE_CALL_MERCURY(
                MR_DD_add_trace_atom_arg_no_value(hlds_num,
                    is_prog_visible_headvar, atom_args, &atom_args);
            );
        } else {
            MR_TRACE_USE_HP(
                MR_new_univ_on_hp(arg, arg_type, arg_value);
            );

            MR_TRACE_CALL_MERCURY(
                MR_DD_add_trace_atom_arg_value(hlds_num,
                    is_prog_visible_headvar, arg, atom_args, &atom_args);
            );
        }
    }

    return atom_args;
}

static void
MR_trace_decl_ensure_init(void)
{
    static MR_bool        done = MR_FALSE;
    static MercuryFile    mdb_in;
    static MercuryFile    mdb_out;

    MR_mercuryfile_init(MR_mdb_in, 1, &mdb_in);
    MR_mercuryfile_init(MR_mdb_out, 1, &mdb_out);

    if (! done) {
        MR_trace_browse_ensure_init();
        MR_TRACE_CALL_MERCURY(
            MR_trace_node_store = 0;
            MR_DD_decl_diagnosis_state_init(
                MR_wrap_input_stream(&mdb_in),
                MR_wrap_output_stream(&mdb_out),
                MR_trace_browser_persistent_state, MR_trace_help_system,
                &MR_trace_front_end_state);
        );
        done = MR_TRUE;
    }
}

void
MR_trace_decl_session_init()
{
    MR_trace_decl_ensure_init();
    MR_TRACE_CALL_MERCURY(
        MR_DD_decl_session_init(
            MR_trace_front_end_state, &MR_trace_front_end_state);
    );
}

void
MR_trace_decl_set_fallback_search_mode(MR_DeclSearchMode search_mode)
{
    MR_trace_decl_ensure_init();
    MR_TRACE_CALL_MERCURY(
        MR_DD_decl_set_fallback_search_mode(MR_trace_node_store, search_mode,
            MR_trace_front_end_state, &MR_trace_front_end_state);
    );
}

void
MR_trace_decl_reset_knowledge_base()
{
    MR_trace_decl_ensure_init();
    MR_TRACE_CALL_MERCURY(
        MR_DD_decl_reset_knowledge_base(
            MR_trace_front_end_state, &MR_trace_front_end_state);
    );
}

void
MR_trace_decl_set_testing_flag(MR_bool testing)
{
    MR_trace_decl_ensure_init();
    MR_TRACE_CALL_MERCURY(
        MR_DD_decl_set_diagnoser_testing_flag(testing,
            MR_trace_front_end_state, &MR_trace_front_end_state);
    );
}

MR_bool
MR_trace_is_valid_search_mode_string(const char *search_mode_string,
    MR_DeclSearchMode *search_mode, MR_bool *search_mode_requires_trace_counts)
{
    MR_bool is_valid;

    *search_mode_requires_trace_counts = MR_FALSE;

    MR_TRACE_CALL_MERCURY(
        if (MR_streq(search_mode_string, "top_down")
            || MR_streq(search_mode_string, "top-down")
            || MR_streq(search_mode_string, "td"))
        {
            *search_mode = MR_DD_decl_top_down_search_mode();
            is_valid = MR_TRUE;
        } else if (MR_streq(search_mode_string, "divide_and_query")
            || MR_streq(search_mode_string, "divide-and-query")
            || MR_streq(search_mode_string, "dq"))
        {
            *search_mode = MR_DD_decl_divide_and_query_search_mode();
            is_valid = MR_TRUE;
        } else if (MR_streq(search_mode_string, "suspicion_divide_and_query")
            || MR_streq(search_mode_string, "suspicion-divide-and-query")
            || MR_streq(search_mode_string, "sdq"))
        {
            *search_mode = MR_DD_decl_suspicion_divide_and_query_search_mode();
            is_valid = MR_TRUE;
            *search_mode_requires_trace_counts = MR_TRUE;
        } else {
            is_valid = MR_FALSE;
        }
    );

    return is_valid;
}

MR_DeclSearchMode
MR_trace_get_default_search_mode(void)
{
    MR_DeclSearchMode   search_mode;

    MR_TRACE_CALL_MERCURY(
        search_mode = MR_DD_decl_top_down_search_mode();
    );

    return search_mode;
}

void
MR_decl_add_trusted_module(const char *module_name)
{
    MR_String aligned_module_name;

    MR_TRACE_USE_HP(
        MR_make_aligned_string(aligned_module_name, (MR_String) module_name);
    );
    MR_trace_decl_ensure_init();
    MR_TRACE_CALL_MERCURY(
        MR_DD_decl_add_trusted_module((MR_String)aligned_module_name,
            MR_trace_front_end_state, &MR_trace_front_end_state);
    );
}

void
MR_decl_add_trusted_pred_or_func(const MR_ProcLayout *entry)
{
    MR_trace_decl_ensure_init();
    MR_TRACE_CALL_MERCURY(
        MR_DD_decl_add_trusted_pred_or_func(entry,
            MR_trace_front_end_state, &MR_trace_front_end_state);
    );
}

void
MR_decl_trust_standard_library(void)
{
    MR_trace_decl_ensure_init();
    MR_TRACE_CALL_MERCURY(
        MR_DD_decl_trust_standard_library(
            MR_trace_front_end_state, &MR_trace_front_end_state);
    );
}

MR_bool
MR_decl_remove_trusted(MR_Integer n)
{
    MR_bool success;
    MR_Word new_diagnoser;

    MR_trace_decl_ensure_init();
    MR_TRACE_CALL_MERCURY(
        success = MR_DD_decl_remove_trusted(n, MR_trace_front_end_state,
            &new_diagnoser);
    );

    if (success) {
        MR_trace_front_end_state = new_diagnoser;
    }

    return success;
}

void
MR_decl_print_all_trusted(FILE *fp, MR_bool mdb_command_format)
{
    MR_String    trusted_list;

    MR_trace_decl_ensure_init();
    MR_TRACE_CALL_MERCURY(
        MR_DD_decl_get_trusted_list(MR_trace_front_end_state,
            mdb_command_format, &trusted_list);
    );

    fputs(trusted_list, fp);
}

MR_bool
MR_trace_start_decl_debug(MR_DeclMode mode, const char *outfile,
    MR_bool new_session, MR_TraceCmdInfo *cmd, MR_EventInfo *event_info,
    MR_Code **jumpaddr)
{
    const MR_ProcLayout     *entry;
    FILE                    *out;
    const char              *message;
    MR_TraceLevel           trace_level;
    static MR_bool          first_time = MR_TRUE;

    MR_edt_initial_event = event_info->MR_event_number;

    // If it was requested that the previous session be resumed and
    // there was a previous dd session, then there is no need to
    // build a new annotated trace.

    if (!new_session && !first_time) {
        MR_decl_mode = mode;
        MR_selected_trace_func_ptr = MR_trace_real_decl;
        *jumpaddr = MR_decl_diagnosis((MR_TraceNode) NULL, cmd,
            event_info, MR_FALSE);
        return MR_TRUE;
    }

    MR_edt_return_node = (MR_TraceNode) NULL;

    if (!MR_port_is_final(event_info->MR_trace_port)) {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err,
            "mdb: declarative debugging is only available"
            " from EXIT, FAIL or EXCP events.\n");
        return MR_FALSE;
    }

    entry = event_info->MR_event_sll->MR_sll_entry;
    if (!MR_PROC_LAYOUT_HAS_EXEC_TRACE(entry)) {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err,
            "mdb: cannot start declarative debugging, "
            "because this procedure was not\n"
            "compiled with execution tracing enabled.\n");
        return MR_FALSE;
    }

    if (MR_PROC_LAYOUT_IS_UCI(entry)) {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err,
            "mdb: cannot start declarative debugging "
            "at compiler generated procedures.\n");
        return MR_FALSE;
    }

    trace_level = entry->MR_sle_module_layout->MR_ml_trace_level;
    if (trace_level != MR_TRACE_LEVEL_DEEP
        && trace_level != MR_TRACE_LEVEL_DECL_REP)
    {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err,
            "mdb: cannot start declarative debugging, "
            "because this procedure was not\n"
            "compiled with trace level `deep' or `rep'.\n");
        return MR_FALSE;
    }

    if (entry->MR_sle_module_layout->MR_ml_suppressed_events != 0) {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err,
            "mdb: cannot start declarative debugging, "
            "because some event types were\n"
            "suppressed when this procedure was compiled.\n");
        return MR_FALSE;
    }

    if (mode == MR_DECL_DUMP) {
        out = fopen(outfile, "w");
        if (out == NULL) {
            char    errbuf[MR_STRERROR_BUF_SIZE];

            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: cannot open file `%s' for output: %s.\n",
                outfile, MR_strerror(errno, errbuf, sizeof(errbuf)));
            return MR_FALSE;
        } else {
            MR_trace_store_file = out;
        }
    }

    MR_decl_mode = mode;
    MR_selected_trace_func_ptr = MR_trace_real_decl;
    MR_trace_decl_ensure_init();
    MR_trace_current_node = (MR_TraceNode) NULL;
    message = MR_trace_start_collecting(event_info->MR_event_number,
        event_info->MR_call_seqno, MR_edt_default_depth_limit,
        MR_FALSE, cmd, event_info, jumpaddr);

    if (message == NULL) {
        first_time = MR_FALSE;
        return MR_TRUE;
    } else {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "mdb: failed to start collecting events:\n%s\n",
            message);

        return MR_FALSE;
    }
}

static MR_Code *
MR_trace_restart_decl_debug(MR_TraceNode call_preceding, MR_Unsigned event,
    MR_Unsigned seqno, MR_bool create_supertree, MR_Unsigned depth_limit,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info)
{
    const char  *message;
    MR_Code     *jumpaddr;

    MR_edt_return_node = (MR_TraceNode) NULL;

    // Set this to the preceding node, so the new explicit tree's parent is
    // resolved correctly.

    MR_trace_current_node = call_preceding;

    message = MR_trace_start_collecting(event, seqno, depth_limit,
        create_supertree, cmd, event_info, &jumpaddr);

    if (message != NULL) {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "mdb: diagnosis aborted:\n%s\n", message);
        MR_selected_trace_func_ptr = MR_trace_real;
        MR_debug_enabled = MR_TRUE;
        MR_update_trace_func_enabled();
        return MR_trace_event_internal(cmd, MR_TRUE, NULL, event_info, NULL);
    }

    return jumpaddr;
}

static const char *
MR_trace_start_collecting(MR_Unsigned event, MR_Unsigned seqno,
    MR_Unsigned maxdepth, MR_bool create_supertree, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    const char      *problem;
    MR_RetryResult  retry_result;
    int             retry_distance;
    MR_bool         unsafe_retry;
    int             counter_depth;

    MR_edt_unsafe_retry_already_asked = MR_FALSE;

    // We need to do a retry if the current event is greater than the
    // call event number corresponding to seqno. Since we don't have the
    // call event number for seqno (`event' is the final event number,
    // not the call event number), we do a retry if:
    //
    //    a) The call sequence number of the current call is greater than
    //       or equal to seqno, or
    //    b) The current event number is greater than the final event
    //       for seqno.
    //
    // Case b) covers the situation where the current event is after the
    // final event for seqno and case a) covers the case where the current
    // event is greater than or equal to the call event for seqno and less
    // than or equal to the final event for seqno. This means we will do
    // a retry if the call event for seqno is equal to the current event
    // but that is not a problem since the retry will be a no-op.

    if (event_info->MR_call_seqno >= seqno ||
        event_info->MR_event_number > event)
    {
        retry_distance = MR_find_first_call_less_eq_seq_or_event(
            MR_FIND_FIRST_CALL_BEFORE_SEQ, seqno, event_info->MR_event_sll,
            MR_saved_sp(event_info->MR_saved_regs),
            MR_saved_curfr(event_info->MR_saved_regs), &problem);

        if (retry_distance < 0) {
            return problem;
        }

        retry_result = MR_trace_retry(event_info,
            retry_distance, MR_RETRY_IO_INTERACTIVE,
            MR_trace_decl_assume_all_io_is_tabled,
            MR_DECL_UNTABLED_IO_RETRY_MESSAGE, &unsafe_retry,
            &problem, MR_mdb_in, MR_mdb_out, jumpaddr);
        if (retry_result != MR_RETRY_OK_DIRECT) {
            if (retry_result == MR_RETRY_ERROR) {
                return problem;
            } else {
                return "internal error: direct retry impossible";
            }
        }

        if (unsafe_retry) {
            MR_edt_unsafe_retry_already_asked = MR_TRUE;
        }
    } else {
        *jumpaddr = NULL;
    }

    // Clear any warnings.

    MR_edt_compiler_flag_warning = MR_FALSE;

    // Start collecting the trace from the desired call, with the
    // desired depth bound.

    MR_edt_last_event = event;
    MR_edt_start_seqno = seqno;
    MR_edt_max_depth = maxdepth;
    MR_edt_inside = MR_FALSE;
    MR_edt_building_supertree = create_supertree;
    MR_edt_suspicion_accumulator = 0;
    MR_edt_start_time = MR_get_user_cpu_milliseconds();
    MR_edt_first_event = event_info->MR_event_number;

    // The deepest we will build any implicit subtree to will be
    // the number of desired nodes divided by two, since the minimum
    // number of events at each depth will be 2 (the CALL and EXIT).

    counter_depth = (MR_edt_desired_nodes_in_subtree / 2) + 1;
    MR_trace_init_implicit_subtree_counters(counter_depth);

    // Single step through every event.

    cmd->MR_trace_cmd = MR_CMD_STEP;
    cmd->MR_trace_strict = MR_TRUE;
    cmd->MR_trace_print_level_specified = MR_TRUE;
    cmd->MR_trace_print_level = MR_PRINT_LEVEL_NONE;
    cmd->MR_trace_must_check = MR_FALSE;

    MR_debug_enabled = MR_TRUE;
    MR_update_trace_func_enabled();
    return NULL;
}

static MR_Code *
MR_decl_diagnosis(MR_TraceNode root, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_bool new_tree)
{
    MR_Word             response;
    MR_bool             bug_found;
    MR_bool             symptom_found;
    MR_bool             no_bug_found;
    MR_bool             require_subtree;
    MR_bool             require_supertree;
    MR_Unsigned         bug_event;
    MR_Unsigned         symptom_event;
    MR_Unsigned         final_event;
    MR_Unsigned         topmost_seqno;
    MR_TraceNode        call_preceding;
    MercuryFile         stream;
    MR_Integer          requested_subtree_depth;

    if (MR_edt_compiler_flag_warning) {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "Warning: some modules were compiled with"
            " a trace level lower than `decl'.\n"
            "This may result in calls being omitted from"
            " the debugging tree.\n");
    }

    if (MR_decl_mode == MR_DECL_DUMP && new_tree) {
        MR_mercuryfile_init(MR_trace_store_file, 1, &stream);

        MR_TRACE_CALL_MERCURY(
            MR_DD_save_trace(MR_wrap_output_stream(&stream),
                MR_trace_node_store, root);
        );

        fclose(MR_trace_store_file);
        MR_selected_trace_func_ptr = MR_trace_real;
        MR_debug_enabled = MR_TRUE;
        MR_update_trace_func_enabled();

        return MR_trace_event_internal(cmd, MR_TRUE, NULL, event_info, NULL);
    }

    if (MR_trace_decl_debug_debugger_mode) {
        // This is a quick and dirty way to debug the front end.

        MR_debug_enabled = MR_TRUE;
        MR_update_trace_func_enabled();
        MR_selected_trace_func_ptr = MR_trace_real;
    } else {
        MR_debug_enabled = MR_FALSE;
        MR_update_trace_func_enabled();
        MR_selected_trace_func_ptr = MR_trace_real_decl;
    }

    MR_TRACE_CALL_MERCURY(
        if (new_tree) {
            MR_DD_decl_diagnosis_new_tree(MR_trace_node_store,
                root, &response,
                MR_trace_front_end_state, &MR_trace_front_end_state,
                MR_trace_browser_persistent_state,
                &MR_trace_browser_persistent_state
            );
        } else {
            MR_DD_decl_diagnosis_resume_previous(MR_trace_node_store,
                &response,
                MR_trace_front_end_state, &MR_trace_front_end_state,
                MR_trace_browser_persistent_state,
                &MR_trace_browser_persistent_state
            );
        }

        bug_found = MR_DD_diagnoser_bug_found(response,
            (MR_Integer *) &bug_event);
        symptom_found = MR_DD_diagnoser_symptom_found(response,
            (MR_Integer *) &symptom_event);
        no_bug_found = MR_DD_diagnoser_no_bug_found(response);
        require_subtree = MR_DD_diagnoser_require_subtree(response,
            (MR_Integer *) &final_event,
            (MR_Integer *) &topmost_seqno,
            (MR_TraceNode *) &call_preceding,
            (MR_Integer *) &requested_subtree_depth);
        require_supertree = MR_DD_diagnoser_require_supertree(response,
            (MR_Integer *) &final_event,
            (MR_Integer *) &topmost_seqno);
    );

    // Turn off interactive debugging after the diagnosis in case
    // a new explicit subtree or supertree needs to be constructed.

    MR_debug_enabled = MR_FALSE;
    MR_update_trace_func_enabled();
    MR_selected_trace_func_ptr = MR_trace_real_decl;

    if (bug_found) {
        return MR_decl_go_to_selected_event(bug_event, cmd, event_info);
    }

    if (symptom_found) {
        return MR_decl_go_to_selected_event(symptom_event, cmd, event_info);
    }

    if (no_bug_found) {
        // No bug found. Return to the procedural debugger at the event
        // where the `dd' command was initially given.

        return MR_decl_go_to_selected_event(MR_edt_initial_event, cmd,
            event_info);
    }

    if (require_subtree) {
        // The front end requires a subtree to be made explicit.
        // Restart the declarative debugger with the appropriate depth limit.

        MR_decl_checkpoint_subtree(final_event, topmost_seqno,
            requested_subtree_depth);
        return MR_trace_restart_decl_debug(call_preceding, final_event,
            topmost_seqno, MR_FALSE, requested_subtree_depth, cmd, event_info);
    }

    if (require_supertree) {
        // Front end requires a supertree to be made explicit.

        MR_decl_checkpoint_supertree(final_event, topmost_seqno,
            MR_edt_default_depth_limit);
        return MR_trace_restart_decl_debug((MR_TraceNode) NULL, final_event,
            topmost_seqno, MR_TRUE, MR_edt_default_depth_limit, cmd,
            event_info);
    }

    // We shouldn't ever get here.
    MR_fatal_error("unknown diagnoser response");
}

static MR_Code *
MR_decl_go_to_selected_event(MR_Unsigned event, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info)
{
    const char      *problem;
    MR_RetryResult  retry_result;
    MR_Code         *jumpaddr;
    int             ancestor_level;
    MR_bool         unsafe_retry;

    // Initialise this to avoid warnings that it might be used uninitialised.

    retry_result = MR_RETRY_OK_DIRECT;

    // We only need to do a retry if the event number we want to be at
    // is less than or equal to the current event number (we need to do a retry
    // if the event numbers are equal, because MR_trace_real will increment
    // the current event number, so the next event displayed by mdb
    // will be the current event + 1.

    if (event <= event_info->MR_event_number) {
        ancestor_level = MR_find_first_call_less_eq_seq_or_event(
            MR_FIND_FIRST_CALL_BEFORE_EVENT, event,
            event_info->MR_event_sll,
            MR_saved_sp(event_info->MR_saved_regs),
            MR_saved_curfr(event_info->MR_saved_regs), &problem);

        if (ancestor_level >= 0) {
            // Perform a retry to get to before the given event. Then set
            // the command to go to the given event and return to interactive
            // mode.

#ifdef    MR_DEBUG_RETRY
            MR_print_stack_regs(stdout, event_info->MR_saved_regs);
            MR_print_succip_reg(stdout, event_info->MR_saved_regs);
#endif
            retry_result = MR_trace_retry(event_info,
                ancestor_level, MR_RETRY_IO_INTERACTIVE,
                MR_trace_decl_assume_all_io_is_tabled,
                MR_DECL_UNTABLED_IO_RETRY_MESSAGE,
                &unsafe_retry, &problem, MR_mdb_in, MR_mdb_out, &jumpaddr);
#ifdef    MR_DEBUG_RETRY
            MR_print_stack_regs(stdout, event_info->MR_saved_regs);
            MR_print_succip_reg(stdout, event_info->MR_saved_regs);
            MR_print_r_regs(stdout, event_info->MR_saved_regs);
#endif
        }
        if ((ancestor_level < 0) || (retry_result != MR_RETRY_OK_DIRECT)) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: diagnosis aborted:\n");
            if (ancestor_level < 0) {
                fprintf(MR_mdb_err, "couldn't find call on stack: %s\n",
                    problem);
            } else {
                if (retry_result == MR_RETRY_ERROR) {
                    fprintf(MR_mdb_err, "%s\n", problem);
                } else {
                    fprintf(MR_mdb_err, "direct retry impossible\n");
                }
            }

            MR_selected_trace_func_ptr = MR_trace_real;
            MR_debug_enabled = MR_TRUE;
            MR_update_trace_func_enabled();
            return MR_trace_event_internal(cmd, MR_TRUE, NULL, event_info,
                NULL);
        }
    } else {
        // Since the event we want to be at is after the current event,
        // don't jump anywhere, just do forward execution until we
        // get to the right event.

        jumpaddr = NULL;
    }

    cmd->MR_trace_cmd = MR_CMD_GOTO;
    cmd->MR_trace_stop_event = event;
    cmd->MR_trace_print_level_specified = MR_TRUE;
    cmd->MR_trace_print_level = MR_PRINT_LEVEL_NONE;
    cmd->MR_trace_strict = MR_TRUE;
    cmd->MR_trace_must_check = MR_FALSE;
    MR_selected_trace_func_ptr = MR_trace_real;
    MR_debug_enabled = MR_TRUE;
    MR_update_trace_func_enabled();
    return jumpaddr;
}

static MR_String
MR_trace_node_path(MR_TraceNode node)
{
    MR_String   path;

    MR_trace_node_store++;
    MR_TRACE_CALL_MERCURY(
        path = MR_DD_trace_node_path((MR_Word) node);
    );

    return path;
}

static MR_TracePort
MR_trace_node_port(MR_TraceNode node)
{
    MR_TracePort    port;

    MR_TRACE_CALL_MERCURY(
        port = (MR_TracePort) MR_DD_trace_node_port((MR_Word) node);
    );

    return port;
}

static MR_Unsigned
MR_trace_node_seqno(MR_TraceNode node)
{
    MR_Unsigned seqno;

    MR_trace_node_store++;
    MR_TRACE_CALL_MERCURY(
        if (!MR_DD_trace_node_seqno(MR_trace_node_store, (MR_Word) node,
            (MR_Integer *) &seqno))
        {
            MR_fatal_error("MR_trace_node_seqno: not an interface event");
        }
    );

    return seqno;
}

static MR_TraceNode
MR_trace_node_first_disj(MR_TraceNode node)
{
    MR_TraceNode    first;

    MR_TRACE_CALL_MERCURY(
        if (!MR_DD_trace_node_first_disj((MR_Word) node, (MR_Word *) &first)) {
            MR_fatal_error("MR_trace_node_first_disj: not a DISJ event");
        }
    );

    return first;
}

static MR_TraceNode
MR_trace_step_left_in_contour(MR_TraceNode node)
{
    MR_TraceNode    next;

    MR_decl_checkpoint_step(node);

    MR_trace_node_store++;
    MR_TRACE_CALL_MERCURY(
        next = (MR_TraceNode) MR_DD_step_left_in_contour(MR_trace_node_store,
            node);
    );

    return next;
}

static MR_TraceNode
MR_trace_find_prev_contour(MR_TraceNode node)
{
    MR_TraceNode    next;

    MR_decl_checkpoint_find(node);

    MR_trace_node_store++;
    MR_TRACE_CALL_MERCURY(
        next = (MR_TraceNode) MR_DD_find_prev_contour(MR_trace_node_store,
            node);
    );

    return next;
}

static void
MR_trace_reset_implicit_subtree_counters(void)
{
    int i;

    i = 0;
    while ((i < MR_edt_implicit_subtree_num_counters)
        && (MR_edt_implicit_subtree_counters[i] != 0))
    {
        MR_edt_implicit_subtree_counters[i] = 0;
        i++;
    }
}

static void
MR_trace_init_implicit_subtree_counters(MR_Unsigned size)
{
    size_t  i;

    MR_edt_implicit_subtree_counters = (MR_Unsigned *)
        malloc(size * sizeof(MR_Unsigned));

    for (i = 0; i < size; i++) {
        MR_edt_implicit_subtree_counters[i] = 0;
    }

    MR_edt_implicit_subtree_num_counters = size;
}

static void
MR_trace_free_implicit_subtree_counters(void)
{
    free(MR_edt_implicit_subtree_counters);
}

static MR_Unsigned
MR_trace_calc_implicit_subtree_ideal_depth(void)
{
    MR_Integer  depth;
    MR_Unsigned total;
    MR_Unsigned events_at_depth;

    depth = 0;
    total = 0;
    while (depth < MR_edt_implicit_subtree_num_counters) {
        events_at_depth = MR_edt_implicit_subtree_counters[depth];
        total += events_at_depth;

        if (total > MR_edt_desired_nodes_in_subtree) {
            // Since we have gone over the desired number of nodes,
            // the ideal depth is depth - 1. Note that we want the depth limit
            // to be at least one, otherwise we won't add any new nodes
            // to the annotated trace.

            return (depth - 1) < 1 ? 1 : (depth - 1);
        }

        if (events_at_depth == 0) {
            return MR_edt_implicit_subtree_num_counters;
        }

        depth++;
    }

    // We didn't record the number of events at greater depths, so
    // be conservative and return the biggest depth we recorded up to.

    return (depth - 1) < 1 ? 1 : (depth - 1);
}

// NOTE:  This function must be called within a MR_TRACE_CALL_MERCURY
// wrapper.

static void
MR_trace_maybe_update_implicit_tree_ideal_depth(MR_Unsigned current_depth,
    MR_TraceNode call)
{
    if (current_depth == MR_edt_max_depth) {
        MR_Unsigned    ideal_depth;
        MR_Unsigned    prev_ideal_depth;

        ideal_depth = MR_trace_calc_implicit_subtree_ideal_depth();
        // Use the lowest depth if the ideal depth was set on a previous EXIT.
        // XXX In future the implicit subtree information should be stored
        // at final events, not CALL events, however this goes hand in hand
        // with a change to build pieces of the annotated trace
        // only between a REDO and its EXIT, if the events between
        // the CALL and the previous EXIT have already been materialized
        // (currently the events between the CALL and EXIT will be materialized
        // twice in this kind of situation).

        prev_ideal_depth = MR_DD_get_implicit_tree_ideal_depth(call);
        if (prev_ideal_depth == 0 || prev_ideal_depth > ideal_depth) {
            MR_DD_call_node_update_implicit_tree_info(call, ideal_depth);
        }
    }
}

static  void
MR_trace_maybe_update_suspicion_accumulator(
    const MR_LabelLayout *label_layout)
{
    if (MR_edt_update_suspicion_accumulator) {
        MR_Unsigned *label_suspicion;

        label_suspicion = MR_trace_lookup_trace_count(label_layout);
        MR_edt_suspicion_accumulator += *label_suspicion;
    }
}

MR_bool
MR_trace_decl_init_suspicion_table(char *pass_trace_counts_file,
    char *fail_trace_counts_file, MR_String *problem)
{
    MR_String                   aligned_pass_trace_counts_file;
    MR_String                   aligned_fail_trace_counts_file;
    MR_Word                     maybe_dice;
    MR_Word                     dice;
    int                         num_modules;
    int                         module_num;
    MR_Integer                  num_files;
    int                         file_num;
    int                         num_labels;
    int                         label_num;
    int                         label_index;
    const MR_ModuleLayout       *module;
    const MR_ModuleFileLayout   *file;
    const MR_LabelLayout        *label;
    MR_Unsigned                 *table_cell;
    MR_Float                    f_suspicion;

    MR_TRACE_USE_HP(
        MR_make_aligned_string(aligned_pass_trace_counts_file,
            (MR_String) pass_trace_counts_file);
        MR_make_aligned_string(aligned_fail_trace_counts_file,
            (MR_String) fail_trace_counts_file);
    );

    MR_TRACE_CALL_MERCURY(
        MR_MDBCOMP_read_dice(
            aligned_pass_trace_counts_file,
            aligned_fail_trace_counts_file,
            &maybe_dice);
        MR_MDBCOMP_maybe_dice_error_to_problem_string(maybe_dice, problem);
    );
    if (! MR_streq(*problem, "")) {
        return MR_FALSE;
    } else {
        MR_TRACE_CALL_MERCURY(
            MR_MDBCOMP_det_maybe_dice_error_to_dice(maybe_dice, &dice);
        );
    }

    // We have read in a valid dice, so we can go ahead and set up the
    // suspicion table. We use the execution count table to store the
    // suspicions of each label. This is a good idea because
    // (a) it is quick to look up a value in this table given a label, and
    // (b) it is not used for counting events during an interactive mdb
    // session.

    num_modules = MR_module_info_next;
    for (module_num = 0; module_num < num_modules; module_num++) {
        module = MR_module_infos[module_num];
        num_files = module->MR_ml_filename_count;

        for (file_num = 0; file_num < num_files; file_num++) {
            file = module->MR_ml_module_file_layout[file_num];
            num_labels = file->MR_mfl_label_count;

            for (label_num = 0; label_num < num_labels; label_num++) {
                label = file->MR_mfl_label_layout[label_num];
                label_index = label->MR_sll_label_num_in_module;
                table_cell = &(module->MR_ml_label_exec_count[label_index]);
                MR_TRACE_CALL_MERCURY(
                    f_suspicion =
                        MR_MDBCOMP_get_suspicion_for_label_layout(dice, label);
                );
                // Instead of using a ratio between 0 and 1 we store an integer
                // between 0 and MR_TRACE_DECL_MAX_SUSPICION, since this is
                // quicker and requires less storage space.

                *table_cell = (MR_Unsigned)
                    ((MR_Float) MR_TRACE_DECL_MAX_SUSPICION * f_suspicion);
            }
        }
    }

    MR_edt_update_suspicion_accumulator = MR_TRUE;
    return MR_TRUE;
}

void
MR_trace_show_progress_subtree(MR_Unsigned event_number)
{
    MR_Unsigned        current_tick;

    if (event_number != MR_edt_last_event &&
        MR_edt_progress_last_tick == 0 &&
        (MR_edt_start_time + MR_DECL_DISPLAY_PROGRESS_DELAY
            < MR_get_user_cpu_milliseconds()))
    {
        fprintf(MR_mdb_out, MR_DECL_PROGRESS_MESSAGE_SUBTREE);
        fflush(MR_mdb_out);
        // We count the initial progress message as the first tick.
        MR_edt_progress_last_tick = 1;
    } else if (MR_edt_progress_last_tick > 0) {
        current_tick = (MR_Unsigned) ((
            (float) (event_number - MR_edt_first_event)
                * (float) MR_DECL_PROGRESS_TOTAL)
            / (float)(MR_edt_last_event - MR_edt_first_event));
        if (current_tick != MR_edt_progress_last_tick) {
            for (; MR_edt_progress_last_tick < current_tick;
                MR_edt_progress_last_tick++)
            {
                fprintf(MR_mdb_out, MR_DECL_PROGRESS_TICK_STRING);
                fflush(MR_mdb_out);
            }
        }
    }
}

void
MR_trace_show_progress_supertree(MR_Unsigned event_number)
{
    // If we are building a supertree we don't know what the final event
    // will be, so we just show a tick every MR_DECL_DISPLAY_PROGRESS_DELAY
    // milliseconds, so at least the user knows something is happening.

    if (MR_edt_progress_last_tick == 0 &&
        (MR_edt_start_time + MR_DECL_DISPLAY_PROGRESS_DELAY
            < MR_get_user_cpu_milliseconds()))
    {
        fprintf(MR_mdb_out, MR_DECL_PROGRESS_MESSAGE_SUPERTREE);
        fflush(MR_mdb_out);
        MR_edt_progress_last_tick = 1;
    } else if ((MR_edt_start_time
        + (MR_edt_progress_last_tick + 1) * MR_DECL_DISPLAY_PROGRESS_DELAY)
            < MR_get_user_cpu_milliseconds())
    {
        MR_edt_progress_last_tick++;
        fprintf(MR_mdb_out, MR_DECL_PROGRESS_TICK_STRING);
        fflush(MR_mdb_out);
    }
}

static void
MR_trace_finish_progress(void)
{
    if (MR_mdb_decl_print_progress) {
        fprintf(MR_mdb_out, "\n");
        fflush(MR_mdb_out);
    }

    MR_edt_progress_last_tick = 0;
}

static void
MR_decl_checkpoint_event_imp(const char *str, MR_bool print_event,
    MR_EventInfo *event_info)
{
    if (print_event) {
        fprintf(MR_mdb_out, "DD %s %ld: #%ld %ld %s ",
            str,
            (long) event_info->MR_event_number,
            (long) event_info->MR_call_seqno,
            (long) event_info->MR_call_depth,
            MR_actual_port_names[event_info->MR_trace_port]);
        MR_print_proc_id(MR_mdb_out, event_info->MR_event_sll->MR_sll_entry);
        fprintf(MR_mdb_out, "\n");
    } else {
        fprintf(MR_mdb_out, "DD AT EVENT %ld: %s\n",
            (long) event_info->MR_event_number, str);
    }
}

static void
MR_decl_checkpoint_loc(const char *str, MR_TraceNode node)
{
    MercuryFile mdb_out;

    MR_mercuryfile_init(MR_mdb_out, 1, &mdb_out);
    fprintf(MR_mdb_out, "DD %s: %ld ", str, (long) node);
    MR_TRACE_CALL_MERCURY(
        MR_DD_print_trace_node((MR_Word) &mdb_out, (MR_Word) node);
    );
    fprintf(MR_mdb_out, "\n");
}

static void
MR_decl_checkpoint_tree(const char *tree_kind, MR_Unsigned final_event,
    MR_Unsigned top_seqno, MR_Unsigned depth_limit)
{
    fprintf(MR_mdb_out, "DD STARTING %s: ", tree_kind);
    fprintf(MR_mdb_out, "final event %lu, topmost seqno %lu, depth %lu\n",
        (unsigned long) final_event, (unsigned long) top_seqno,
        (unsigned long) depth_limit);
}

#ifdef MR_DD_PRINT_EDT_STATS

static void
MR_decl_print_edt_stats(void)
{
    MR_bool debug_enabled_before = MR_debug_enabled;
    pid_t   pid;
    char    cmdstr[200];

    MR_edt_stats_total_constructed_nodes +=
        MR_edt_stats_constructed_nodes_this_time;
    MR_edt_stats_total_reexecutions++;

    fflush(NULL);
    // We use stderr instead of MR_mdb_err since ML_report_stats()
    // writes to stderr.

    fprintf(stderr, "EDT construction stats: \n");
    fprintf(stderr, "Total reexecutions so far = %i\n",
        MR_edt_stats_total_reexecutions);
    fprintf(stderr, "Nodes constructed in this run = %i\n",
        MR_edt_stats_constructed_nodes_this_time);
    fprintf(stderr, "Max depth for this run = %i\n",
        MR_edt_max_depth);
    fprintf(stderr, "Total nodes constructed so far = %i\n",
        MR_edt_stats_total_constructed_nodes);
    fprintf(stderr, "Current event = %i\n", MR_trace_event_number);
    fprintf(stderr, "Total CPU time = %.2f\n",
        MR_get_user_cpu_milliseconds() / 1000.0);
    pid = getpid();
    sprintf(cmdstr, "ps -p %i -o rss,vsz | tail -1 |"
        "awk '{print \"RSS = \" $1 \"\\nVSZ = \" $2}' 1>&2", pid);
    MR_trace_call_system_display_error_on_failure(stderr, cmdstr);

    MR_debug_enabled = MR_FALSE;
    MR_update_trace_func_enabled();

    fprintf(stderr, "Benchmarking stats:\n");
    MR_TRACE_CALL_MERCURY(
        ML_report_stats();
    );

    MR_debug_enabled = debug_enabled_before;
    MR_update_trace_func_enabled();

    fprintf(stderr, "\n");
    fflush(stderr);

    MR_edt_stats_constructed_nodes_this_time = 0;
}

static void
MR_decl_inc_constructed_nodes(void)
{
    MR_edt_stats_constructed_nodes_this_time++;
}

#endif // MR_DD_PRINT_EDT_STATS
