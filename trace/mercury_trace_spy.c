// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2002, 2005-2008, 2011 The University of Melbourne.
// Copyright (C) 2014-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This file contains code to manage spy points for both
// the internal and external debuggers.
//
// Main author: Zoltan Somogyi.

#include "mercury_imp.h"
#include "mercury_array_macros.h"
#include "mercury_layout_util.h"
#include "mercury_trace_base.h"

#include "mercury_trace.h"
#include "mercury_trace_spy.h"
#include "mercury_trace_tables.h"
#include "mercury_trace_util.h"
#include "mercury_trace_vars.h"

#include "mdb.cterm.mh"

#include <stdlib.h>

const char      *MR_spy_when_names[] =
{
    "all",
    "interface",
    "entry",
    "specific",
    "linenumber",
    "user_event",
    "user_event_set",
};

// The table of spy points, with one entry per existing (or deleted but
// not yet reused) spy point, with counters saying which is the next
// free slot and how many slots are allocated.

MR_SpyPoint     **MR_spy_points;
int             MR_spy_point_next = 0;
int             MR_spy_point_max  = 0;

int             MR_most_recent_spy_point = -1;

// The initial size of the spy points table.
#define MR_INIT_SPY_POINTS  10

// The table of spied on procedures, with one entry per procedure that
// has ever been spied on, giving the possibly empty list of spy points
// (enabled or disabled but not deleted) that refer to that procedure,
// with counters saying which is the next free slot and how many slots
// are allocated.

typedef struct {
    const MR_ProcLayout     *MR_sp_proc;
    MR_SpyPoint             *MR_sp_points;
} MR_SpiedProc;

static  MR_SpiedProc        *MR_spied_procs;
static  int                 MR_spied_proc_next = 0;
static  int                 MR_spied_proc_max = 0;

// The initial size of the spied procs table.
#define MR_INIT_SPIED_PROCS 10

// The table of spied on return labels, with one entry per return label
// that is currently being spied on, ordered on label layout structure address,
// and with counters saying which is the next free slot and how many slots
// are allocated.

typedef struct {
    const MR_LabelLayout    *MR_sl_label;
    int                     MR_sl_point_num;
} MR_SpiedLabel;

static  MR_SpiedLabel       *MR_spied_labels;
static  int                 MR_spied_label_next = 0;
static  int                 MR_spied_label_max = 0;

// The initial size of the spied labels table.
#define MR_INIT_SPIED_LABELS    10

// The table of spied on user event names, with one entry per user event
// name that is currently being spied on, ordered on the event name,
// and with counters saying which is the next free slot and how many slots
// are allocated.
//
// All the MR_SPY_USER_EVENT breakpoints are listed in MR_spied_user_events,
// whether or not the breakpoint requires a specific event set as well as a
// specific name.

typedef struct {
    const char                  *MR_sue_user_event_name;
    MR_SpyPoint                 *MR_sue_points;
} MR_SpiedUserEvent;

static  MR_SpiedUserEvent       *MR_spied_user_events;
static  int                     MR_spied_user_event_next = 0;
static  int                     MR_spied_user_event_max = 0;

// The initial size of the spied user events table.
#define MR_INIT_SPIED_USER_EVENTS  10

// The table of spied on user event set names, with one entry per user event
// set name that is currently being spied on, ordered on the event set name,
// and with counters saying which is the next free slot and how many slots
// are allocated.
//
// Only the MR_SPY_USER_EVENT_SET breakpoints that specify an event set
// are listed in MR_spied_user_event_sets. The ones that do not specify
// an event set are all listed in MR_spied_universal_user_events.

typedef struct {
    const char                  *MR_sues_user_event_set;
    MR_SpyPoint                 *MR_sues_points;
} MR_SpiedUserEventSet;

static  MR_SpiedUserEventSet    *MR_spied_user_event_sets;
static  int                     MR_spied_user_event_set_next = 0;
static  int                     MR_spied_user_event_set_max = 0;

// The initial size of the spied user event sets table.
#define MR_INIT_SPIED_USER_EVENT_SETS  10

static  MR_SpyPoint             *MR_spied_universal_user_events;

////////////////////////////////////////////////////////////////////////////

static  int         MR_compare_addr(const void *address1,
                        const void *address2);
static  int         MR_search_spy_table_for_proc(const MR_ProcLayout *entry);
static  int         MR_search_spy_table_for_label(
                        const MR_LabelLayout *label);
static  int         MR_search_spy_table_for_user_event_name(
                        const char *user_event_name);
static  int         MR_search_spy_table_for_user_event_set(
                        const char *user_event_set);
static  MR_bool     MR_spy_cond_is_true(MR_SpyPoint *point,
                        const MR_LabelLayout *label);
static  int         MR_add_spy_point(MR_bool reuse, MR_SpyPoint *point);
static  void        MR_add_line_spy_point_callback(
                        const MR_LabelLayout *label, int spy_point_num);
static  int         MR_compare_spied_labels(const void *, const void *);
static  void        MR_delete_spy_print_list(MR_SpyPrintList print_list);
static  void        MR_update_enabled_action(MR_SpyPoint *point,
                        const MR_LabelLayout *layout, MR_TracePort port,
                        MR_bool *enabled_ptr, MR_SpyAction *action_ptr,
                        MR_SpyPrintList *print_listr_ptr);
static  const char  *MR_ignore_when_to_string(MR_SpyIgnore_When ignore_when);
static  void        MR_print_spy_print_what(FILE *fp, MR_SpyPrint sp);

// Compare two addresses, and return an integer which is <0, 0, or >0
// depending on whether the first address is less than, equal to, or
// greater than the second. Suitable for use with MR_bsearch() and
// MR_prepare_insert_into_sorted().

static int
MR_compare_addr(const void *address1, const void *address2)
{
    // Note that we can't just compare the pointers, because on a
    // segmented architecture, that might only compare the segments,
    // not the offsets (ANSI C doesn't require pointer comparisons to work
    // unless the pointers point into the same array, which is not necessarily
    // going to be the case here).
    //
    // So instead we need to cast the pointers to integers and then
    // compare the integers.

    MR_Unsigned num1 = (MR_Unsigned) address1;
    MR_Unsigned num2 = (MR_Unsigned) address2;
    return (num1 > num2 ? 1 : num1 == num2 ? 0 : -1);
}

// Return the index of the entry in MR_spied_procs whose MR_sp_proc field
// is entry, or a negative number if absent.

static int
MR_search_spy_table_for_proc(const MR_ProcLayout *entry)
{
    int     slot;
    MR_bool found;

    MR_bsearch(MR_spied_proc_next, slot, found,
        MR_compare_addr(MR_spied_procs[slot].MR_sp_proc, entry));
    if (found) {
        return slot;
    } else {
        return -1;
    }
}

// Return the index of the entry in MR_spied_labels whose MR_sl_label field
// is equal to the label argument, or a negative number if absent.

static int
MR_search_spy_table_for_label(const MR_LabelLayout *label)
{
    int     slot;
    MR_bool found;

    MR_bsearch(MR_spied_label_next, slot, found,
        MR_compare_addr(MR_spied_labels[slot].MR_sl_label, label));
    if (found) {
        return slot;
    } else {
        return -1;
    }
}

// Return the index of the entry in MR_spied_user_events whose
// MR_sue_user_event_name field is equal to the user_event_name argument,
// or a negative number if absent.

static int
MR_search_spy_table_for_user_event_name(const char *user_event_name)
{
    int     slot;
    MR_bool found;

    MR_bsearch(MR_spied_user_event_next, slot, found,
        strcmp(MR_spied_user_events[slot].MR_sue_user_event_name,
            user_event_name));
    if (found) {
        return slot;
    } else {
        return -1;
    }
}

// Return the index of the entry in MR_spied_user_event_sets whose
// MR_sues_user_event_set field is equal to the user_event_set argument,
// or a negative number if absent.

static int
MR_search_spy_table_for_user_event_set(const char *user_event_set)
{
    int     slot;
    MR_bool found;

    MR_bsearch(MR_spied_user_event_set_next, slot, found,
        strcmp(MR_spied_user_event_sets[slot].MR_sues_user_event_set,
            user_event_set));
    if (found) {
        return slot;
    } else {
        return -1;
    }
}

MR_bool
MR_event_matches_spy_point(const MR_LabelLayout *layout,
    MR_TracePort port, MR_SpyAction *action_ptr,
    MR_SpyPrintList *print_list_ptr)
{
    int                     slot;
    MR_bool                 enabled;
    MR_SpyPoint             *point;
    MR_SpyAction            action;
    MR_SpyPrintList         print_list;
    const MR_LabelLayout    *parent;
    const MR_UserEvent      *user_event;
    const MR_UserEventSpec  *user_event_spec;
    const char              *user_event_set;
    const char              *user_event_name;
    const char              *problem;
    MR_Word                 *base_sp;
    MR_Word                 *base_curfr;
    MR_Level                actual_level;

    enabled = MR_FALSE;
    action = MR_SPY_PRINT;
    print_list = NULL;

    if (MR_spied_label_next > 0) {
        slot = MR_search_spy_table_for_label(layout);
        if (slot >= 0) {
            point = MR_spy_points[MR_spied_labels[slot].MR_sl_point_num];
            if (point->MR_spy_when != MR_SPY_LINENO) {
                MR_fatal_error("non-lineno spy point in spied labels array");
            }

            MR_update_enabled_action(point, layout, port,
                &enabled, &action, &print_list);
        }

        if (MR_port_is_interface(port)) {
            MR_restore_transient_registers();
            base_sp = MR_sp;
            base_curfr = MR_curfr;
            parent = MR_find_nth_ancestor(layout, 1, &base_sp, &base_curfr,
                &actual_level, &problem);
            if (parent != NULL && actual_level == 1 &&
                0 <= (slot = MR_search_spy_table_for_label(parent)))
            {
                point = MR_spy_points[MR_spied_labels[slot].MR_sl_point_num];
                if (point->MR_spy_when != MR_SPY_LINENO) {
                    MR_fatal_error("non-lineno spy point in "
                        "spied labels array");
                }

                MR_update_enabled_action(point, layout, port,
                    &enabled, &action, &print_list);
            }
        }
    }

    user_event = layout->MR_sll_user_event;
    if (user_event != NULL) {
        user_event_spec = &MR_user_event_spec(layout);
        user_event_name = user_event_spec->MR_ues_event_name;
        user_event_set = MR_user_event_set_name(layout);

        // Check for breakpoints that specify an event name, and possibly
        // and event set.

        slot = MR_search_spy_table_for_user_event_name(user_event_name);
        if (slot >= 0) {
            for (point = MR_spied_user_events[slot].MR_sue_points;
                point != NULL; point = point->MR_spy_next)
            {
                if (point->MR_spy_when != MR_SPY_USER_EVENT) {
                    MR_fatal_error("non-named-user-event spy point "
                        "in named user event array");
                }

                if (point->MR_spy_user_event_set == NULL ||
                    MR_streq(user_event_set, point->MR_spy_user_event_set))
                {
                    MR_update_enabled_action(point, layout, port,
                        &enabled, &action, &print_list);
                }
            }
        }

        // Check for breakpoints that specify just an event set.

        slot = MR_search_spy_table_for_user_event_set(user_event_set);
        if (slot >= 0) {
            for (point = MR_spied_user_event_sets[slot].MR_sues_points;
                point != NULL; point = point->MR_spy_next)
            {
                if (point->MR_spy_when != MR_SPY_USER_EVENT_SET) {
                    MR_fatal_error("non-named-user-event spy point "
                        "in named user event array");
                }

                MR_update_enabled_action(point, layout, port,
                    &enabled, &action, &print_list);
            }
        }

        // Check for breakpoints that specify neither event name nor event set.

        for (point = MR_spied_universal_user_events; point != NULL;
            point = point->MR_spy_next)
        {
            if (point->MR_spy_when != MR_SPY_USER_EVENT_SET) {
                MR_fatal_error("non-unnamed-user-event spy point "
                    "in unnamed user event list");
            }

            MR_update_enabled_action(point, layout, port,
                &enabled, &action, &print_list);
        }
    }

    slot = MR_search_spy_table_for_proc(layout->MR_sll_entry);
    if (slot >= 0) {
        for (point = MR_spied_procs[slot].MR_sp_points; point != NULL;
            point = point->MR_spy_next)
        {
            switch (point->MR_spy_when) {

                case MR_SPY_ALL:
                    MR_update_enabled_action(point, layout, port,
                        &enabled, &action, &print_list);
                    break;

                case MR_SPY_ENTRY:
                    if (MR_port_is_entry(port)) {
                        MR_update_enabled_action(point, layout, port,
                            &enabled, &action, &print_list);
                    } else {
                        continue;
                    }

                    break;

                case MR_SPY_INTERFACE:
                    if (MR_port_is_interface(port)) {
                        MR_update_enabled_action(point, layout, port,
                            &enabled, &action, &print_list);
                    } else {
                        continue;
                    }

                    break;

                case MR_SPY_SPECIFIC:
                    if (layout == point->MR_spy_label) {
                        MR_update_enabled_action(point, layout, port,
                            &enabled, &action, &print_list);
                    } else {
                        continue;
                    }

                    break;

                case MR_SPY_LINENO:
                    MR_fatal_error("lineno spy point in spied procs array");
                    break;

                case MR_SPY_USER_EVENT:
                    MR_fatal_error("user_event spy point "
                        "in spied procs array");
                    break;

                case MR_SPY_USER_EVENT_SET:
                    MR_fatal_error("user_event_set spy point "
                        "in spied procs array");
                    break;

                default:
                    MR_fatal_error("bad spy point when in "
                        "MR_event_matches_spy_point");
            }
        }
    }

    if (enabled) {
        *action_ptr = action;
        *print_list_ptr = print_list;
        return MR_TRUE;
    } else {
        return MR_FALSE;
    }
}

static void
MR_update_enabled_action(MR_SpyPoint *point, const MR_LabelLayout *layout,
    MR_TracePort port, MR_bool *enabled_ptr, MR_SpyAction *action_ptr,
    MR_SpyPrintList *print_list_ptr)
{
    if (point->MR_spy_enabled && MR_spy_cond_is_true(point, layout)) {
        if (point->MR_spy_ignore_count == 0) {
            *enabled_ptr = MR_TRUE;
            *action_ptr = MR_max(*action_ptr, point->MR_spy_action);
            if (*print_list_ptr == NULL) {
                *print_list_ptr = point->MR_spy_print_list;
            }
        } else if (point->MR_spy_ignore_count > 0) {
            switch (point->MR_spy_ignore_when) {

                case MR_SPY_DONT_IGNORE:
                    break;

                case MR_SPY_IGNORE_ENTRY:
                    if (port == MR_PORT_CALL) {
                        --point->MR_spy_ignore_count;
                    }
                    break;

                case MR_SPY_IGNORE_INTERFACE:
                    if (MR_port_is_interface(port)) {
                        --point->MR_spy_ignore_count;
                    }
                    break;

                case MR_SPY_IGNORE_ALL:
                    --point->MR_spy_ignore_count;
                    break;

                default:
                    MR_fatal_error("MR_update_enabled_action: "
                        "invalid ignore_when");
            }
        }
    }
}

const char  *MR_spy_point_cond_problem = NULL;
MR_SpyCond  *MR_spy_point_cond_bad = NULL;

static MR_bool
MR_spy_cond_is_true(MR_SpyPoint *point, const MR_LabelLayout *label_layout)
{
    int             max_mr_num;
    MR_Word         saved_regs[MR_MAX_FAKE_REG];
    int             max_f_num;
    MR_Float        saved_f_regs[MR_MAX_VIRTUAL_F_REG];
    const char      *problem;
    char            *bad_path;
    MR_TypeInfo     type_info;
    MR_Word         value;
    const char      *name;
    MR_Word         *value_ptr;
    MR_TypeInfo     sub_type_info;
    MR_Word         *sub_value_ptr;
    MR_Word         match;
    MR_bool         saved_trace_func_enabled;
    MR_bool         retval;
    MR_SpyCond      *cond;

    if (point->MR_spy_cond == NULL) {
        return MR_TRUE;
    }

    MR_restore_transient_registers();

    cond = point->MR_spy_cond;

    // From this point, returning should be done by setting both
    // MR_spy_point_cond_problem and retval, and goto end.

    MR_spy_point_cond_bad = cond;
    MR_spy_point_cond_problem = "internal error in MR_spy_cond_is_true";
    retval = MR_TRUE;

    MR_compute_max_mr_num(max_mr_num, label_layout);
    max_f_num = label_layout->MR_sll_entry->MR_sle_max_f_num;
    // This also saves the regs in MR_fake_regs.
    MR_copy_regs_to_saved_regs(max_mr_num, saved_regs,
        max_f_num, saved_f_regs);
    MR_trace_init_point_vars(label_layout, saved_regs, saved_f_regs,
        (MR_TracePort) label_layout->MR_sll_port, MR_FALSE);

    problem = MR_lookup_unambiguous_var_spec(cond->MR_cond_var_spec,
        &type_info, &value, &name);
    if (problem != NULL) {
        if (cond->MR_cond_require_var) {
            MR_spy_point_cond_problem = problem;
            retval = MR_TRUE;
        } else {
            MR_spy_point_cond_problem = NULL;
            retval = MR_FALSE;
        }

        goto end;
    }

    value_ptr = &value;
    bad_path = MR_select_specified_subterm(cond->MR_cond_path,
        type_info, value_ptr, &sub_type_info, &sub_value_ptr);

    if (bad_path != NULL) {
        if (cond->MR_cond_require_var) {
            MR_spy_point_cond_problem = MR_trace_bad_path(cond->MR_cond_path,
                bad_path);
            retval = MR_TRUE;
        } else {
            MR_spy_point_cond_problem = NULL;
            retval = MR_FALSE;
        }
        goto end;
    }

#ifdef MR_DEBUG_SPY_COND
    MR_print_cterm(stdout, cond->cond_term);
    fprintf(stdout, ": ");
#endif

    saved_trace_func_enabled = MR_trace_func_enabled;
    MR_trace_func_enabled = MR_FALSE;
    MR_TRACE_CALL_MERCURY(
        ML_BROWSE_match_with_cterm((MR_Word) sub_type_info, *sub_value_ptr,
            cond->MR_cond_term, &match);
    );
    MR_trace_func_enabled = saved_trace_func_enabled;

#ifdef MR_DEBUG_SPY_COND
    fprintf(stdout, "%d\n", match);
#endif

    switch (cond->MR_cond_test) {
        case MR_SPY_TEST_EQUAL:
            MR_spy_point_cond_problem = NULL;
            if (match != 0) {
                retval = MR_TRUE;
                goto end;
            } else {
                retval = MR_FALSE;
                goto end;
            }
            break;

        case MR_SPY_TEST_NOT_EQUAL:
            MR_spy_point_cond_problem = NULL;
            if (match != 0) {
                retval = MR_FALSE;
                goto end;
            } else {
                retval = MR_TRUE;
                goto end;
            }
            break;

        default:
            MR_fatal_error("MR_spy_cond_is_true: invalid spy_cond_test");
            break;
    }

end:
    MR_copy_saved_regs_to_regs(max_mr_num, saved_regs,
        max_f_num, saved_f_regs);
    MR_save_transient_registers();
    return retval;
}

static const char *incompatible =
    "Ignore count is not compatible with break point specification";

static int
MR_add_spy_point(MR_bool reuse, MR_SpyPoint *point)
{
    int i;
    int point_slot;

    if (reuse) {
        // Try to reuse an existing slot in the MR_spy_points array.
        for (i = 0; i < MR_spy_point_next; i++) {
            if (! MR_spy_points[i]->MR_spy_exists) {
                MR_most_recent_spy_point = i;
                MR_spy_points[i] = point;
                return i;
            }
        }
    }

    // Allocate a new slot.
    MR_ensure_room_for_next(MR_spy_point, MR_SpyPoint *, MR_INIT_SPY_POINTS);
    point_slot = MR_spy_point_next;
    MR_spy_points[point_slot] = point;
    MR_spy_point_next++;

    MR_most_recent_spy_point = point_slot;
    return point_slot;
}

int
MR_add_proc_spy_point(MR_SpyWhen when, MR_SpyAction action,
    MR_SpyIgnore_When ignore_when, MR_IgnoreCount ignore_count,
    const MR_ProcLayout *entry, const MR_LabelLayout *label,
    MR_SpyPrintList print_list, const char **problem)
{
    MR_SpyPoint     *point;
    int             proc_slot;

    *problem = NULL;

    // Insert the spy point at the head of the list for the proc.
    point = MR_NEW(MR_SpyPoint);
    point->MR_spy_when             = when;
    point->MR_spy_exists           = MR_TRUE;
    point->MR_spy_enabled          = MR_TRUE;
    point->MR_spy_action           = action;
    point->MR_spy_ignore_when      = ignore_when;
    point->MR_spy_ignore_count     = ignore_count;
    point->MR_spy_cond             = NULL;
    point->MR_spy_print_list       = print_list;
    point->MR_spy_proc             = entry;
    point->MR_spy_label            = label;
    point->MR_spy_filename         = NULL;
    point->MR_spy_linenumber       = 0;
    point->MR_spy_user_event_set   = NULL;
    point->MR_spy_user_event_name  = NULL;

    proc_slot = MR_search_spy_table_for_proc(entry);
    if (proc_slot < 0) {
        MR_ensure_room_for_next(MR_spied_proc, MR_SpiedProc,
            MR_INIT_SPIED_PROCS);
        MR_prepare_insert_into_sorted(MR_spied_procs,
            MR_spied_proc_next, proc_slot,
            MR_compare_addr(MR_spied_procs[proc_slot].MR_sp_proc, entry));
        MR_spied_procs[proc_slot].MR_sp_proc = entry;
        MR_spied_procs[proc_slot].MR_sp_points = NULL;
    }

    point->MR_spy_next = MR_spied_procs[proc_slot].MR_sp_points;
    MR_spied_procs[proc_slot].MR_sp_points = point;

    return MR_add_spy_point(MR_TRUE, point);
}

// 1024 characters should be big enough ...
#define     MR_ERROR_MSG_BUF_SIZE   1024
static char MR_error_msg_buf[MR_ERROR_MSG_BUF_SIZE];

int
MR_add_line_spy_point(MR_SpyAction action, MR_SpyIgnore_When ignore_when,
    MR_IgnoreCount ignore_count, const char *orig_filename, int linenumber,
    MR_SpyPrintList print_list, const char **problem)
{
    MR_SpyPoint     *point;
    int             point_slot;
    int             old_size;
    int             new_size;
    char            *filename;
    int             num_file_matches = 0;
    int             num_line_matches = 0;

    *problem = NULL;
    if (ignore_when != MR_SPY_DONT_IGNORE) {
        *problem = incompatible;
        return -1;
    }

    // The original filename string may have come from a buffer
    // or other volatile storage.

    filename = MR_copy_string(orig_filename);

    point_slot = MR_spy_point_next;

    old_size = MR_spied_label_next;
    MR_process_file_line_layouts(filename, linenumber,
        MR_add_line_spy_point_callback, point_slot,
        &num_file_matches, &num_line_matches);
    new_size = MR_spied_label_next;

    if (new_size == old_size) {
        if (num_line_matches != 0) {
            // Every line match should add a new spy point.
            MR_fatal_error("MR_add_line_spy_point: num_line_matches != 0");
        }

        // There were no matching labels.
        if (num_file_matches == 0) {
            MR_snprintf(MR_error_msg_buf, MR_ERROR_MSG_BUF_SIZE,
                "there is no debuggable source file named %s", filename);
        } else {
            MR_snprintf(MR_error_msg_buf, MR_ERROR_MSG_BUF_SIZE,
                "there is no event at line %d in %s",
                linenumber, filename);
        }
        *problem = MR_error_msg_buf;
        return -1;
    }

    if (num_line_matches == 0) {
        MR_fatal_error("MR_add_line_spy_point: num_line_matches == 0");
    }

    // The matching labels were added at the end of the spied label table.
    // We must make the table sorted again.

    qsort(MR_spied_labels, MR_spied_label_next, sizeof(MR_SpiedLabel),
        MR_compare_spied_labels);

    point = MR_NEW(MR_SpyPoint);
    point->MR_spy_when            = MR_SPY_LINENO;
    point->MR_spy_exists          = MR_TRUE;
    point->MR_spy_enabled         = MR_TRUE;
    point->MR_spy_action          = action;
    point->MR_spy_ignore_when     = ignore_when;
    point->MR_spy_ignore_count    = ignore_count;
    point->MR_spy_cond            = NULL;
    point->MR_spy_print_list      = print_list;
    point->MR_spy_filename        = filename;
    point->MR_spy_linenumber      = linenumber;
    point->MR_spy_user_event_set  = NULL;
    point->MR_spy_user_event_name = NULL;

    return MR_add_spy_point(MR_FALSE, point);
}

static void
MR_add_line_spy_point_callback(const MR_LabelLayout *label, int spy_point_num)
{
    int spied_label_slot;

    MR_ensure_room_for_next(MR_spied_label, MR_SpiedLabel,
        MR_INIT_SPIED_LABELS);
    spied_label_slot = MR_spied_label_next;
    MR_spied_labels[spied_label_slot].MR_sl_label = label;
    MR_spied_labels[spied_label_slot].MR_sl_point_num = spy_point_num;
    MR_spied_label_next++;
}

static int
MR_compare_spied_labels(const void *l1, const void *l2)
{
    const MR_SpiedLabel    *label1;
    const MR_SpiedLabel    *label2;

    label1 = (const MR_SpiedLabel *) l1;
    label2 = (const MR_SpiedLabel *) l2;

    return (int) ((MR_Integer) label1->MR_sl_label
        - (MR_Integer) label2->MR_sl_label);
}

int
MR_add_user_event_spy_point(MR_SpyAction action,
    MR_SpyIgnore_When ignore_when, MR_IgnoreCount ignore_count,
    const char *user_event_set, const char *user_event_name,
    MR_SpyPrintList print_list, const char **problem)
{
    MR_SpyPoint     *point;
    int             name_slot;
    int             set_slot;

    *problem = NULL;

    if (user_event_set != NULL) {
        user_event_set = strdup(user_event_set);
    }

    if (user_event_name != NULL) {
        user_event_name = strdup(user_event_name);
    }

    point = MR_NEW(MR_SpyPoint);
    point->MR_spy_exists           = MR_TRUE;
    point->MR_spy_enabled          = MR_TRUE;
    point->MR_spy_action           = action;
    point->MR_spy_ignore_when      = ignore_when;
    point->MR_spy_ignore_count     = ignore_count;
    point->MR_spy_cond             = NULL;
    point->MR_spy_print_list       = print_list;
    point->MR_spy_proc             = NULL;
    point->MR_spy_label            = NULL;
    point->MR_spy_user_event_set   = user_event_set;
    point->MR_spy_user_event_name  = user_event_name;

    if (user_event_name == NULL) {
        point->MR_spy_when = MR_SPY_USER_EVENT_SET;

        if (user_event_set == NULL) {
            point->MR_spy_next = MR_spied_universal_user_events;
            MR_spied_universal_user_events = point;
        } else {
            set_slot = MR_search_spy_table_for_user_event_set(user_event_set);
            if (set_slot < 0) {
                MR_ensure_room_for_next(MR_spied_user_event_set,
                    MR_SpiedUserEventSet, MR_INIT_SPIED_USER_EVENT_SETS);
                MR_prepare_insert_into_sorted(MR_spied_user_event_sets,
                    MR_spied_user_event_set_next, set_slot,
                    strcmp(MR_spied_user_event_sets[set_slot].
                        MR_sues_user_event_set, user_event_set));
                MR_spied_user_event_sets[set_slot].MR_sues_user_event_set =
                    user_event_set;
                MR_spied_user_event_sets[set_slot].MR_sues_points = NULL;
            }

            point->MR_spy_next =
                MR_spied_user_event_sets[set_slot].MR_sues_points;
            MR_spied_user_event_sets[set_slot].MR_sues_points = point;
        }
    } else {
        point->MR_spy_when = MR_SPY_USER_EVENT;

        name_slot = MR_search_spy_table_for_user_event_name(user_event_name);
        if (name_slot < 0) {
            MR_ensure_room_for_next(MR_spied_user_event,
                MR_SpiedUserEvent, MR_INIT_SPIED_USER_EVENTS);
            MR_prepare_insert_into_sorted(MR_spied_user_events,
                MR_spied_user_event_next, name_slot,
                strcmp(MR_spied_user_events[name_slot].
                    MR_sue_user_event_name, user_event_name));
            MR_spied_user_events[name_slot].MR_sue_user_event_name =
                user_event_name;
            MR_spied_user_events[name_slot].MR_sue_points = NULL;
        }

        point->MR_spy_next =
            MR_spied_user_events[name_slot].MR_sue_points;
        MR_spied_user_events[name_slot].MR_sue_points = point;
    }

    return MR_add_spy_point(MR_TRUE, point);
}

void
MR_add_spy_point_print_list_start(int point_slot, MR_SpyPrintList print_list)
{
    MR_SpyPrintList list;

    list = print_list;
    if (list == NULL) {
        return;
    }

    // Find the last node in print_list.
    while (list->MR_pl_next != NULL) {
        list = list->MR_pl_next;
    }

    // Add the existing spy_print_list at the end of print_list.
    list->MR_pl_next = MR_spy_points[point_slot]->MR_spy_print_list;
    MR_spy_points[point_slot]->MR_spy_print_list = print_list;
}

void
MR_add_spy_point_print_list_end(int point_slot, MR_SpyPrintList print_list)
{
    MR_SpyPrintList list;

    list = MR_spy_points[point_slot]->MR_spy_print_list;
    if (list == NULL) {
        MR_spy_points[point_slot]->MR_spy_print_list = print_list;
        return;
    }

    // Find the last node in print_list.
    while (list->MR_pl_next != NULL) {
        list = list->MR_pl_next;
    }

    // Add the print_list at the end of the existing spy_print_list.
    list->MR_pl_next = print_list;
}

void
MR_clear_spy_point_print_list(int point_slot)
{
    MR_delete_spy_print_list(MR_spy_points[point_slot]->MR_spy_print_list);
    MR_spy_points[point_slot]->MR_spy_print_list = NULL;
}

const char *
MR_ignore_spy_point(int point_slot, MR_SpyIgnore_When ignore_when,
    MR_IgnoreCount ignore_count)
{
    switch (MR_spy_points[point_slot]->MR_spy_when) {
        case MR_SPY_ENTRY:
        case MR_SPY_INTERFACE:
        case MR_SPY_ALL:
            break;

        case MR_SPY_SPECIFIC:
        case MR_SPY_LINENO:
            return incompatible;
            break;

        default:
            MR_fatal_error("MR_add_proc_spy_point: bad when");
            break;
    }

    MR_spy_points[point_slot]->MR_spy_ignore_when  = ignore_when;
    MR_spy_points[point_slot]->MR_spy_ignore_count = ignore_count;
    return NULL;
}

static void
MR_delete_spy_print_list(MR_SpyPrintList print_list)
{
    if (print_list == NULL) {
        return;
    }

    MR_delete_spy_print_list(print_list->MR_pl_next);

    if (print_list->MR_pl_cur->MR_p_word_copy != NULL) {
        MR_free(print_list->MR_pl_cur->MR_p_word_copy);
    }

    MR_free(print_list->MR_pl_cur);
    MR_free(print_list);
}

void
MR_delete_spy_point(int point_table_slot)
{
    MR_SpyPoint     *point;
    MR_SpyPoint     **cur_addr;
    MR_SpyPoint     *cur;
    int             proc_table_slot;
    int             i;
    int             label_slot;

    point = MR_spy_points[point_table_slot];

    if (MR_most_recent_spy_point == point_table_slot) {
        MR_most_recent_spy_point = -1;
    }

    if (! MR_spy_points[point_table_slot]->MR_spy_exists) {
        return;
    }

    MR_spy_points[point_table_slot]->MR_spy_exists = MR_FALSE;

    MR_delete_spy_print_list(point->MR_spy_print_list);
    // In case it gets deleted again.
    point->MR_spy_print_list = NULL;

    if (point->MR_spy_cond != NULL) {
        MR_delete_cterm(point->MR_spy_cond->MR_cond_term);
        MR_free(point->MR_spy_cond->MR_cond_what_string);
        MR_free(point->MR_spy_cond);

        // In case it gets deleted again.
        point->MR_spy_cond = NULL;
    }

    if (point->MR_spy_when == MR_SPY_LINENO) {
        // Release the storage acquired by MR_copy_string.
        MR_free(point->MR_spy_filename);

        // Remove the spy point from the spied label table list.
        label_slot = 0;
        for (i = 0; i < MR_spied_label_next; i++) {
            if (MR_spied_labels[i].MR_sl_point_num != point_table_slot) {
                MR_spied_labels[label_slot].MR_sl_label =
                    MR_spied_labels[i].MR_sl_label;
                MR_spied_labels[label_slot].MR_sl_point_num =
                    MR_spied_labels[i].MR_sl_point_num;
                label_slot++;
            }
        }

        MR_spied_label_next = label_slot;
    } else {
        // Remove the spy point from the spied proc table list for its proc.
        proc_table_slot = MR_search_spy_table_for_proc(point->MR_spy_proc);
        if (proc_table_slot < 0) {
            MR_fatal_error("deleted spy point was not indexed by proc addr");
        }

        cur_addr = &MR_spied_procs[proc_table_slot].MR_sp_points;
        cur = MR_spied_procs[proc_table_slot].MR_sp_points;
        while (cur != NULL && cur != point) {
            cur_addr = &cur->MR_spy_next;
            cur = cur->MR_spy_next;
        }

        if (cur == NULL) {
            MR_fatal_error("deleted spy point was not on proc index list");
        }

        *cur_addr = point->MR_spy_next;
    }
}

void
MR_print_spy_point(FILE *fp, int spy_point_num, MR_bool verbose)
{
    MR_SpyPoint     *point;
    MR_SpyCond      *cond;
    MR_TracePort    port;

    point = MR_spy_points[spy_point_num];
    fprintf(fp, "%2d: %1s %-5s %-9s ",
        spy_point_num,
        point->MR_spy_exists ?
            (point->MR_spy_enabled ? "+" : "-") :
            (point->MR_spy_enabled ? "E" : "D"),
        MR_spy_action_string(point->MR_spy_action),
        MR_spy_when_names[point->MR_spy_when]);

    switch (point->MR_spy_when) {
        case MR_SPY_ALL:
        case MR_SPY_INTERFACE:
        case MR_SPY_ENTRY:
            MR_print_proc_id(fp, point->MR_spy_proc);
            break;

        case MR_SPY_SPECIFIC:
            MR_print_proc_id(fp, point->MR_spy_proc);
            MR_assert(point->MR_spy_label != NULL);
            port = point->MR_spy_label->MR_sll_port;
            if (port < 0) {
                fprintf(fp, " NONE ");
            } else {
                fprintf(fp, " %4s ", MR_simplified_port_names[port]);
            }
            fprintf(fp, "%s", MR_label_goal_path(point->MR_spy_label));
            break;

        case MR_SPY_LINENO:
            fprintf(fp, "%s:%d",
                point->MR_spy_filename, point->MR_spy_linenumber);
            break;

        case MR_SPY_USER_EVENT:
            // MR_spy_when_names has already printed "user_event".
            if (point->MR_spy_user_event_set != NULL) {
                fprintf(fp, "%s %s",
                    point->MR_spy_user_event_set,
                    point->MR_spy_user_event_name);
            } else {
                fprintf(fp, "%s",
                    point->MR_spy_user_event_name);
            }
            break;

        case MR_SPY_USER_EVENT_SET:
            // MR_spy_when_names has already printed "user_event_set".
            if (point->MR_spy_user_event_set != NULL) {
                fprintf(fp, "%s",
                    point->MR_spy_user_event_set);
            }
            break;
    }

    if (point->MR_spy_ignore_count > 1) {
        fprintf(fp,
            "\n%12s(ignore next %" MR_INTEGER_LENGTH_MODIFIER
            "u %s events)\n",
            "", point->MR_spy_ignore_count,
            MR_ignore_when_to_string(point->MR_spy_ignore_when));
    } else if (point->MR_spy_ignore_count > 0) {
        fprintf(fp, "\n%12s(ignore next %s event)\n",
            "", MR_ignore_when_to_string(point->MR_spy_ignore_when));
    } else {
        fprintf(fp, "\n");
    }

    if (point->MR_spy_cond != NULL) {
        cond = point->MR_spy_cond;

        fprintf(fp, "%12s", "");

        if (! cond->MR_cond_require_var) {
            fprintf(fp, "-v ");
        }

        if (! cond->MR_cond_require_path) {
            fprintf(fp, "-p ");
        }

        MR_print_spy_cond(fp, cond);
        fprintf(fp, "\n");
    }

    if (verbose && point->MR_spy_print_list != NULL) {
        MR_SpyPrintList list;
        MR_SpyPrint     node;

        fprintf(fp, "%12s", "");
        for (list = point->MR_spy_print_list; list != NULL;
            list = list->MR_pl_next)
        {
            node = list->MR_pl_cur;
            MR_print_spy_print_what(fp, node);

            fprintf(fp, " (");

            if ((int) node->MR_p_format ==  MR_BROWSE_DEFAULT_FORMAT) {
                fprintf(fp, "default");
            } else {
                switch (node->MR_p_format) {
                    case MR_BROWSE_FORMAT_FLAT:
                        fprintf(fp, "flat");
                        break;

                    case MR_BROWSE_FORMAT_RAW_PRETTY:
                        fprintf(fp, "raw pretty");
                        break;

                    case MR_BROWSE_FORMAT_PRETTY:
                        fprintf(fp, "pretty");
                        break;

                    case MR_BROWSE_FORMAT_VERBOSE:
                        fprintf(fp, "verbose");
                        break;

                    default:
                        MR_fatal_error("invalid node->MR_p_format");
                        break;
                }
            }

            if (! node->MR_p_warn) {
                fprintf(fp, ", nowarn");
            }

            fprintf(fp, ")");

            if (list->MR_pl_next == NULL) {
                fprintf(fp, "\n");
            } else {
                fprintf(fp, ", ");
            }
        }
    }
}

void
MR_print_spy_cond(FILE *fp, MR_SpyCond *cond)
{
    switch (cond->MR_cond_var_spec.MR_var_spec_kind) {
        case MR_VAR_SPEC_NUMBER:
            fprintf(fp, "%" MR_INTEGER_LENGTH_MODIFIER "u",
                cond->MR_cond_var_spec.MR_var_spec_number);
            break;

        case MR_VAR_SPEC_NAME:
            fprintf(fp, "%s", cond->MR_cond_var_spec.MR_var_spec_name);
            break;

        case MR_VAR_SPEC_HELD_NAME:
            fprintf(fp, "$%s", cond->MR_cond_var_spec.MR_var_spec_name);
            break;

        case MR_VAR_SPEC_ATTRIBUTE:
            fprintf(fp, "!%s", cond->MR_cond_var_spec.MR_var_spec_name);
            break;
    }

    if (cond->MR_cond_path != NULL) {
        fprintf(fp, " ^%s", cond->MR_cond_path);
    }

    switch (cond->MR_cond_test) {
        case MR_SPY_TEST_EQUAL:
            fprintf(fp, " = ");
            break;

        case MR_SPY_TEST_NOT_EQUAL:
            fprintf(fp, " != ");
            break;

        default:
            MR_fatal_error("MR_print_spy_point: invalid cond_test");
            break;
    }

    MR_print_cterm(fp, cond->MR_cond_term);
}

static const char *
MR_ignore_when_to_string(MR_SpyIgnore_When ignore_when)
{
    switch (ignore_when) {
        case MR_SPY_IGNORE_ENTRY:
            return "call";

        case MR_SPY_IGNORE_INTERFACE:
            return "interface";

        default:
            MR_fatal_error("MR_ignore_when_to_string: invalid ignore_when");
    }
}

MR_bool
MR_save_spy_points(FILE *fp, FILE *err_fp)
{
    MR_SpyPoint *point;
    int         i;
    int         spy_point_num;

    spy_point_num = 0;
    for (i = 0; i < MR_spy_point_next; i++) {
        if (! MR_spy_points[i]->MR_spy_exists) {
            continue;
        }

        point = MR_spy_points[i];

        switch (point->MR_spy_action) {
            case MR_SPY_STOP:
                fprintf(fp, "break ");
                break;

            case MR_SPY_PRINT:
                fprintf(fp, "break -P ");
                break;

            default:
                fprintf(err_fp, "internal error: unknown spy action\n");
                return MR_TRUE;
        }

        if (point->MR_spy_ignore_count > 0) {
            switch (point->MR_spy_ignore_when) {
                case MR_SPY_IGNORE_INTERFACE:
                    fprintf(fp, " -I%" MR_INTEGER_LENGTH_MODIFIER "u",
                        point->MR_spy_ignore_count);
                    break;

                case MR_SPY_IGNORE_ENTRY:
                    fprintf(fp, " -E%" MR_INTEGER_LENGTH_MODIFIER "u",
                        point->MR_spy_ignore_count);
                    break;

                default:
                    MR_fatal_error("MR_save_spy_points: invalid ignore_when");
            }
        }

        switch (point->MR_spy_when) {
            case MR_SPY_LINENO:
                fprintf(fp, "%s:%d\n",
                    point->MR_spy_filename, point->MR_spy_linenumber);
                break;

            case MR_SPY_ALL:
                fprintf(fp, "-a ");
                MR_print_proc_spec(fp, point->MR_spy_proc);
                fprintf(fp, "\n");
                break;

            case MR_SPY_INTERFACE:
                MR_print_proc_spec(fp, point->MR_spy_proc);
                fprintf(fp, "\n");
                break;

            case MR_SPY_ENTRY:
                fprintf(fp, "-e ");
                MR_print_proc_spec(fp, point->MR_spy_proc);
                fprintf(fp, "\n");
                break;

            case MR_SPY_SPECIFIC:
                fprintf(err_fp, "mdb: cannot save breakpoint on "
                    "specific internal label\n");
                break;

            default:
                fprintf(err_fp, "mdb: internal error: unknown spy when\n");
                return MR_TRUE;
        }

        if (point->MR_spy_cond != NULL) {
            MR_SpyCond *cond;

            cond = point->MR_spy_cond;
            fprintf(fp, "condition ");

            if (!cond->MR_cond_require_var) {
                fprintf(fp, "-v "); // also implies -p
            } else if (!cond->MR_cond_require_path) {
                fprintf(fp, "-p ");
            }

            fprintf(fp, "%s ", cond->MR_cond_what_string);

            switch (cond->MR_cond_test) {
                case MR_SPY_TEST_EQUAL:
                    fprintf(fp, "= ");
                    break;

                case MR_SPY_TEST_NOT_EQUAL:
                    fprintf(fp, "!= ");
                    break;

                default:
                    MR_fatal_error("MR_save_spy_points: bad condition test");
                    break;
            }

            MR_print_cterm(fp, cond->MR_cond_term);
            fprintf(fp, "\n");
        }

        if (!point->MR_spy_enabled) {
            fprintf(fp, "disable\n");
        }

        if (point->MR_spy_print_list != NULL) {
            MR_SpyPrintList   list;
            MR_SpyPrint        node;

            list = point->MR_spy_print_list;
            for (; list != NULL; list = list->MR_pl_next) {
                node = list->MR_pl_cur;

                fprintf(fp, "break_print -e");
                if (! node->MR_p_warn) {
                    fprintf(fp, " -n");
                }

                if ((int) node->MR_p_format != MR_BROWSE_DEFAULT_FORMAT) {
                    switch (node->MR_p_format) {
                        case MR_BROWSE_FORMAT_FLAT:
                            fprintf(fp, " -f");
                            break;

                        case MR_BROWSE_FORMAT_RAW_PRETTY:
                            // -p is the closest approximation.
                            fprintf(fp, " -p");
                            break;

                        case MR_BROWSE_FORMAT_PRETTY:
                            fprintf(fp, " -p");
                            break;

                        case MR_BROWSE_FORMAT_VERBOSE:
                            fprintf(fp, " -v");
                            break;

                        default:
                            MR_fatal_error("invalid node->MR_p_format");
                            break;
                    }
                }

                fprintf(fp, " ");
                MR_print_spy_print_what(fp, node);
                fprintf(fp, "\n");
            }
        }

        spy_point_num++;
    }

    return MR_FALSE;
}

static void
MR_print_spy_print_what(FILE *fp, MR_SpyPrint sp)
{
    switch (sp->MR_p_what) {
        case MR_SPY_PRINT_GOAL:
            fprintf(fp, "goal");
            break;

        case MR_SPY_PRINT_ALL:
            fprintf(fp, "all");
            break;

        case MR_SPY_PRINT_ONE:
            MR_print_var_spec(fp, &sp->MR_p_var_spec);
            if (sp->MR_p_path != NULL) {
                fprintf(fp, "^%s", sp->MR_p_path);
            }
            break;
    }
}
