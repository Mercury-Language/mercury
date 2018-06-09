// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2001, 2005-2007 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This file contains the declarations of the types and functions that
// the internal and external debuggers can use to manipulate spy points.
//
// Main author: Zoltan Somogyi.

#ifndef MERCURY_TRACE_SPY_H
#define MERCURY_TRACE_SPY_H

#include "mercury_stack_layout.h"   // for MR_ProcLayout etc
#include "mercury_trace_term.h"     // for MR_CTerm
#include "mercury_trace_base.h"     // for MR_TracePort etc
#include "mercury_trace_browse.h"   // for MR_BrowseFormat
#include "mercury_trace_vars.h"     // for MR_VarSpec
#include "mercury_trace.h"          // for MR_IgnoreCount

typedef enum {
    MR_SPY_PRINT, MR_SPY_STOP
} MR_SpyAction;

#define MR_spy_action_string(a)     ((a == MR_SPY_STOP) ? "stop" :      \
                                    (a == MR_SPY_PRINT) ? "print" :     \
                                    "unknown spy action")

typedef enum {
    MR_SPY_ALL,
    MR_SPY_INTERFACE,
    MR_SPY_ENTRY,
    MR_SPY_SPECIFIC,
    MR_SPY_LINENO,
    MR_SPY_USER_EVENT,
    MR_SPY_USER_EVENT_SET
} MR_SpyWhen;

typedef enum {
    MR_SPY_DONT_IGNORE,
    MR_SPY_IGNORE_INTERFACE,
    MR_SPY_IGNORE_ENTRY,
    MR_SPY_IGNORE_ALL
} MR_SpyIgnore_When;

extern  const char                      *MR_spy_when_names[];

typedef struct MR_SpyPrint_Struct       *MR_SpyPrint;
typedef struct MR_SpyPrintList_Struct   *MR_SpyPrintList;

typedef enum {
    MR_SPY_TEST_EQUAL, MR_SPY_TEST_NOT_EQUAL
} MR_SpyTest;

typedef enum {
    MR_SPY_PRINT_GOAL,
    MR_SPY_PRINT_ALL,
    MR_SPY_PRINT_ONE,
} MR_SpyPrintWhat;

struct MR_SpyPrint_Struct {
    MR_BrowseFormat MR_p_format;
    MR_SpyPrintWhat MR_p_what;
    char            *MR_p_word_copy;
    MR_VarSpec      MR_p_var_spec;  // if MR_SPY_PRINT_ONE
    char            *MR_p_path;     // if MR_SPY_PRINT_ONE
    MR_bool         MR_p_warn;
};

struct MR_SpyPrintList_Struct {
    MR_SpyPrint     MR_pl_cur;
    MR_SpyPrintList MR_pl_next;
};

typedef struct MR_SpyPoint_Struct MR_SpyPoint;

typedef struct {
    MR_VarSpec      MR_cond_var_spec;
    char            *MR_cond_path;
    MR_SpyTest      MR_cond_test;
    MR_CTerm        MR_cond_term;
    MR_bool         MR_cond_require_var;
    MR_bool         MR_cond_require_path;
    char            *MR_cond_what_string;
} MR_SpyCond;

struct MR_SpyPoint_Struct {
    MR_bool                 MR_spy_exists;          // MR_FALSE if deleted
    MR_bool                 MR_spy_enabled;
    MR_SpyWhen              MR_spy_when;
    MR_SpyAction            MR_spy_action;
    MR_SpyIgnore_When       MR_spy_ignore_when;
    MR_IgnoreCount          MR_spy_ignore_count;
    MR_SpyCond              *MR_spy_cond;
    MR_SpyPrintList         MR_spy_print_list;
    const MR_ProcLayout     *MR_spy_proc;           // if not LINENO
    const MR_LabelLayout    *MR_spy_label;          // if SPECIFIC
    char                    *MR_spy_filename;       // if LINENO
    int                     MR_spy_linenumber;      // if LINENO
    const char              *MR_spy_user_event_set; // if USER_EVENT or SET
    const char              *MR_spy_user_event_name;// if USER_EVENT
    MR_SpyPoint             *MR_spy_next;           // if not LINENO
};

// The table of spy points, with counters saying which is the next free slot
// and how many slots are allocated.

extern  MR_SpyPoint     **MR_spy_points;
extern  int             MR_spy_point_next;
extern  int             MR_spy_point_max;

extern  int             MR_most_recent_spy_point;

extern  MR_SpyCond      *MR_spy_point_cond_bad;
extern  const char      *MR_spy_point_cond_problem;

// Check whether the event described by the given label layout and port
// matches any spy points. If yes, return MR_TRUE, set *action to say what
// action should be executed for the spy point, and set print_list to the
// variable print list of the first matching spy point.

extern  MR_bool         MR_event_matches_spy_point(
                            const MR_LabelLayout *layout, MR_TracePort port,
                            MR_SpyAction *action,
                            MR_SpyPrintList *print_list);

// Add a new spy point on a procedure (as opposed to on a line number or
// user event) to the table. If this cannot be done, return a negative number
// and set *problem to point to an error message.

extern  int             MR_add_proc_spy_point(MR_SpyWhen when,
                            MR_SpyAction action, MR_SpyIgnore_When ignore_when,
                            MR_IgnoreCount ignore_count,
                            const MR_ProcLayout *entry,
                            const MR_LabelLayout *label,
                            MR_SpyPrintList print_list, const char **problem);

// Add a new spy point on a line number (as opposed to on a procedure or user
// event) to the table. If this cannot be done, return a negative number
// and set *problem to point to an error message.

extern  int             MR_add_line_spy_point(MR_SpyAction action,
                            MR_SpyIgnore_When ignore_when,
                            MR_IgnoreCount ignore_count,
                            const char *filename, int linenumber,
                            MR_SpyPrintList print_list, const char **problem);

// Add a new spy point on a user event (as opposed to on a procedure or
// line number) to the table. If this cannot be done, return a negative number
// and set *problem to point to an error message.

extern  int             MR_add_user_event_spy_point(MR_SpyAction action,
                            MR_SpyIgnore_When ignore_when,
                            MR_IgnoreCount ignore_count,
                            const char *user_event_set,
                            const char *user_event_name,
                            MR_SpyPrintList print_list, const char **problem);

// Add the given set of things to be printed to the spy point's list,
// at either the start or the end of the existing list.

extern  void            MR_add_spy_point_print_list_start(int point_slot,
                            MR_SpyPrintList print_list);
extern  void            MR_add_spy_point_print_list_end(int point_slot,
                            MR_SpyPrintList print_list);

// Empty the set of things to be printed at the spy point.

extern  void            MR_clear_spy_point_print_list(int point_slot);

// Apply the given ignore specification to the given spy point.
// If the ignore specification is not appropriate for the spy point,
// return a non-NULL problem report.

extern  const char      *MR_ignore_spy_point(int point_slot,
                            MR_SpyIgnore_When ignore_when,
                            MR_IgnoreCount ignore_count);

// Delete a spy point from the table.

extern  void            MR_delete_spy_point(int point_table_slot);

// Print the spy point with the given number in a nice format for humans to
// read to the given file.

extern  void            MR_print_spy_point(FILE *fp, int i, MR_bool verbose);

// Print the given spy point condition in a user-friendly manner.

extern  void            MR_print_spy_cond(FILE *fp, MR_SpyCond *cond);

// Print the set of current spy points (including those that are currently
// disabled) to fp in a format that, when sourced by mdb, recreates those
// spy points. Any internal errors encountered while trying to do this
// should be reported to err_fp. Return MR_TRUE iff the debugger's data
// structures are inconsistent, and the spy points could not be saved.

extern  MR_bool         MR_save_spy_points(FILE *fp, FILE *err_fp);

#endif  // not MERCURY_TRACE_SPY_H
