// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2002, 2004-2007 The University of Melbourne.
// Copyright (C) 2014-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#ifndef MERCURY_TRACE_DECLARATIVE_H
#define MERCURY_TRACE_DECLARATIVE_H

#include "mercury_imp.h"
#include "mercury_trace.h"
#include "mercury_trace_internal.h"

// When in declarative debugging mode, the internal debugger calls
// MR_trace_decl_debug for each event.

extern  MR_Code     *MR_trace_decl_debug(MR_EventInfo *event_info);

// The following functions update the progress indicator when building
// a subtree or a supertree.

extern  void        MR_trace_show_progress_subtree(MR_Unsigned event_number);
extern  void        MR_trace_show_progress_supertree(MR_Unsigned event_number);

// The internal (interactive) debugger calls this function to enter
// declarative debugging mode. It returns MR_TRUE if successful, and MR_FALSE
// if there was some problem that prevented this mode from being entered.

typedef enum {
    MR_DECL_NODUMP,
    MR_DECL_DUMP
} MR_DeclMode;

extern  MR_bool     MR_trace_start_decl_debug(MR_DeclMode mode,
                        const char *outfile, MR_bool new_session,
                        MR_TraceCmdInfo *cmd, MR_EventInfo *event_info,
                        MR_Code **jumpaddr);

// The declarative debugger may need to perform many retries during one
// diagnosis session. If the goal being debugged can do I/O, these retries
// are safe only if all I/O primitives in the program are tabled. Normally,
// this is guaranteed by the grade being a debugging grade. However, we also
// want to check the functioning of the declarative debugger in non-debug
// grades. In these grades, we only call tabled I/O primitives in the test
// cases themselves, so the existence of non-tabled primitives in the standard
// library doesn't matter. An option of the dd (or dd_dd) command can assert
// that all the I/O primitives being backtracked over are tabled. This global
// variable records the presence or absence of this option on the last dd or
// dd_dd command. It must be stored in a global instead of being passed around
// as a parameter because the front end can cause retries even after the
// initial retry that starts collecting the annotated trace.

extern  MR_bool     MR_trace_decl_assume_all_io_is_tabled;

// These functions add (or remove) a module, pred or func to (or from)
// the set of trusted objects in the oracle_state inside the current
// diagnoser_state. They will call MR_trace_decl_ensure_init to ensure
// the diagnoser_state is initialised first.

extern  void        MR_decl_add_trusted_module(const char *module_name);
extern  void        MR_decl_add_trusted_pred_or_func(
                        const MR_ProcLayout *entry);
extern  void        MR_decl_trust_standard_library(void);
extern  MR_bool     MR_decl_remove_trusted(MR_Integer n);

// MR_trace_decl_set_default_search_mode sets the default search mode for
// the analyser.

typedef MR_Word MR_DeclSearchMode;

extern  void        MR_trace_decl_set_fallback_search_mode(
                        MR_DeclSearchMode search_mode);

// MR_trace_decl_session_init performs per-session initialization.

extern void         MR_trace_decl_session_init(void);

// MR_trace_decl_reset_knowledge_base resets the oracle's knowledge base.

extern  void        MR_trace_decl_reset_knowledge_base(void);

// This function checks to see if the supplied string is a valid search mode.
// If it is then it returns MR_TRUE and sets the value at search_mode
// to the corresponding search mode. If it isn't, then it returns MR_FALSE
// and leaves the value at search_mode unchanged.

extern  MR_bool     MR_trace_is_valid_search_mode_string(
                        const char *search_mode_string,
                        MR_DeclSearchMode *search_mode,
                        MR_bool *search_mode_requires_trace_counts);

// Return the default search mode to use when the --search-mode option for the
// `dd' command is not given.

extern MR_DeclSearchMode MR_trace_get_default_search_mode(void);

// Prints a list of the trusted objects. If mdb_command_format is true,
// it prints the list as a series of mdb `trust' commands. Otherwise,
// it prints the list in a format suitable for display.

extern  void        MR_decl_print_all_trusted(FILE *fp,
                        MR_bool mdb_command_format);

// Set up the table of suspicions for each label. This must be done
// before generation of the annotated trace is started if
// MR_edt_update_suspicion_accumulator is true.
// Returns MR_TRUE if the table was successfully set up and MR_FALSE
// in there was a problem. A description of the problem is stored at *problem.

extern  MR_bool     MR_trace_decl_init_suspicion_table(
                        char *pass_trace_counts_file,
                        char *fail_trace_counts_file,
                        MR_String *problem);

// Set the testing flag of the diagnoser.
// See the user_state type in browser/declarative_user.m for more details.

extern  void        MR_trace_decl_set_testing_flag(MR_bool testing);

// The depth of the EDT is different from the call depth of the events, since
// the call depth is not guaranteed to be increase by one each time
// -- see comments in MR_trace_real in trace/mercury_trace.c. We use the
// following variable to keep track of the EDT depth. We only keep track of
// the depth of events in the subtree we are materializing.
// MR_edt_depth keeps track of the depth of nodes in the EDT by storing the
// depth of the next event if it is a final or internal event. If the next
// event is a CALL or REDO it will have a depth of MR_edt_depth + 1.
// The CALL and final events of the root of the subtree being materialized
// have depth 0.

extern  MR_Integer      MR_edt_depth;

// MR_edt_implicit_subtree_depth performs the same role as MR_edt_depth,
// except that it keeps track of the depth of nodes in implicit subtrees of
// the tree being materialized.

extern  MR_Integer      MR_edt_implicit_subtree_depth;

// Events where the value of MR_edt_depth above is greater than the value of
// MR_edt_max_depth will not be included in the annotated trace.

extern  MR_Unsigned     MR_edt_max_depth;

// MR_edt_implicit_subtree_counters points to an array that records
// the number of events at different depths in implicit subtrees.
// These are used to determine what depth an implicit subtree should
// be materialized to.

extern  MR_Unsigned     *MR_edt_implicit_subtree_counters;
extern  MR_Unsigned     MR_edt_implicit_subtree_num_counters;

// When building a new explicit tree we build it to the maximum depth such
// that the number of nodes in the explicit tree is less than or equal to
// MR_edt_desired_nodes_in_subtree.

extern  MR_Unsigned     MR_edt_desired_nodes_in_subtree;

// In the event that the ideal depth to build a tree to cannot be calculated,
// either because it is the initial build of the annotated trace or a supertree
// is being built, use the value of the following global as the depth limit.

extern  MR_Unsigned     MR_edt_default_depth_limit;

// The following variable indicates whether the declarative debugger was
// invoked with the --debug option. It is needed so that the declarative
// debugger can continue to be debugged after a new portion of the
// annotated trace has been generated.

extern  MR_bool        MR_trace_decl_debug_debugger_mode;

// This variable indicates whether we are building a supertree
// above a given event or a subtree rooted at a given event.

extern  MR_bool        MR_edt_building_supertree;

// The following macros are provided to help C code manipulate the
// Mercury data structure. The values here must match the corresponding
// values in the definitions in browser/declarative_execution.m.

typedef MR_Word MR_TraceNode;

#define MR_TRACE_STATUS_SUCCEEDED    (MR_Word) 0
#define MR_TRACE_STATUS_FAILED       (MR_Word) 1
#define MR_TRACE_STATUS_UNDECIDED    (MR_Word) 2

// The initial depth step size. We want to be quite conservative with this
// value, since initially we don't know what the branching factor of the tree
// is.

#define MR_TRACE_DECL_INITIAL_DEPTH     5

// The suspicion of each event is represented as an integer between 0 and
// MR_TRACE_DECL_MAX_SUSPICION.

#define MR_TRACE_DECL_MAX_SUSPICION     100

// The default desired number of nodes to add to the annotated trace when
// materializing a new subtree.

#define MR_TRACE_DESIRED_SUBTREE_NODES  10000

// The message to display when attempting to retry over an untabled area.

#define MR_DECL_UNTABLED_IO_RETRY_MESSAGE                               \
    "The declarative debugger needs to perform a retry across\n"        \
    "an area in which IO is not tabled. This is not always safe.\n"     \
    "To avoid this warning restart mdb and issue a `table_io start'\n"  \
    "command at an event before the suspect area.\n"                    \
    "Do you wish to proceed with the retry? "

// How often to update the progress message, expressed in terms of number of
// events. We define separate intervals for when we are materializing
// nodes and when we are in an implicit subtree, since execution is much
// faster in implicit subtrees.

#define MR_DECL_PROGRESS_CHECK_INTERVAL                  100000
#define MR_DECL_PROGRESS_CHECK_INTERVAL_IMPLICIT_SUBTREE 1000000

// The total number of progress ticks that should be displayed when building
// of the current portion of the annotated trace is 100% complete.

#define MR_DECL_PROGRESS_TOTAL    40

// The progress message to display and the tick string to repeatedly display
// after the initial progress message.

#define MR_DECL_PROGRESS_MESSAGE_SUBTREE        "Generating subtree.."
#define MR_DECL_PROGRESS_MESSAGE_SUPERTREE      "Generating supertree.."
#define MR_DECL_PROGRESS_TICK_STRING            "."

// How many milliseconds to wait before displaying progress.

#define MR_DECL_DISPLAY_PROGRESS_DELAY    1000

// The following two macros decide when to display progress.
// We define two versions: one for when we are materializing nodes and one
// for when we are in an implicit subtree, since execution is much faster in
// implicit subtrees.
// In the implicit tree version we don't need to check if the event is the
// last event, since the last event will not be in an implicit subtree.
// This means we can reorder the if-then-elses to be more efficient in
// implicit subtrees. We also check to see if we should update the progress
// less often when in an implicit subtree.
// By defining these checks as macros we only incur the cost of a function call
// when we need to update the progress.

#define MR_DECL_MAYBE_UPDATE_PROGRESS(event_number)                           \
    do {                                                                      \
        if (MR_mdb_decl_print_progress) {                                     \
            if (MR_edt_building_supertree) {                                  \
                if (event_number % MR_DECL_PROGRESS_CHECK_INTERVAL == 0) {    \
                    MR_trace_show_progress_supertree(event_number);           \
                }                                                             \
            } else {                                                          \
                if (event_number % MR_DECL_PROGRESS_CHECK_INTERVAL == 0       \
                    || event_number == MR_edt_last_event)                     \
                {                                                             \
                    MR_trace_show_progress_subtree(event_number);             \
                }                                                             \
            }                                                                 \
        }                                                                     \
    } while (0)

#define MR_DECL_MAYBE_UPDATE_PROGRESS_IMPLICIT_SUBTREE(event_number)          \
    do {                                                                      \
        if (event_number % MR_DECL_PROGRESS_CHECK_INTERVAL_IMPLICIT_SUBTREE   \
            == 0)                                                             \
        {                                                                     \
            if (MR_mdb_decl_print_progress) {                                 \
                if (MR_edt_building_supertree) {                              \
                    MR_trace_show_progress_supertree(event_number);           \
                } else {                                                      \
                    MR_trace_show_progress_subtree(event_number);             \
                }                                                             \
            }                                                                 \
        }                                                                     \
    } while (0)

// The following macro works out the depth of a node in the EDT.
// It also updates next_depth to the depth of the next node if that node is
// a final or internal event.
// If the next node is a CALL or REDO it will have a depth of next_depth + 1.

#define MR_DD_CALC_NODE_DEPTH(port, node_depth, next_depth)                   \
    do {                                                                      \
        if (port == MR_PORT_CALL || port == MR_PORT_REDO) {                   \
            node_depth = ++next_depth;                                        \
        } else {                                                              \
            if (MR_port_is_final(port)) {                                     \
                /*                                                            \
                ** The depth of the EXIT, FAIL or EXCP event is               \
                ** next_depth (not next_depth - 1), however                   \
                ** we need to adjust next_depth here for future events.       \
                */                                                            \
                node_depth = next_depth--;                                    \
            } else {                                                          \
                node_depth = next_depth;                                      \
            }                                                                 \
        }                                                                     \
    } while (0)

#endif  // MERCURY_TRACE_DECLARATIVE_H
