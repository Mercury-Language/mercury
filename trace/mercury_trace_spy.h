/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 1998-2001, 2005-2006 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file contains the declarations of the types and functions that
** the internal and external debuggers can use to manipulate spy points.
**
** Main author: Zoltan Somogyi.
*/

#ifndef MERCURY_TRACE_SPY_H
#define MERCURY_TRACE_SPY_H

#include "mercury_stack_layout.h"   /* for MR_ProcLayout etc */
#include "mercury_trace_term.h"     /* for MR_CTerm */
#include "mercury_trace_base.h"     /* for MR_TracePort etc  */
#include "mercury_trace_browse.h"   /* for MR_BrowseFormat */
#include "mercury_trace_vars.h"     /* for MR_VarSpec */

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
    MR_SPY_LINENO
} MR_SpyWhen;

typedef enum {
    MR_SPY_DONT_IGNORE,
    MR_SPY_IGNORE_INTERFACE,
    MR_SPY_IGNORE_ENTRY
} MR_SpyIgnore_When;

extern  const char                      *MR_spy_when_names[];

typedef struct MR_SpyPrint_Struct      *MR_SpyPrint;
typedef struct MR_SpyPrintList_Struct *MR_SpyPrintList;

typedef enum {
    MR_SPY_TEST_EQUAL, MR_SPY_TEST_NOT_EQUAL
} MR_SpyTest;

typedef enum {
    MR_SPY_PRINT_GOAL,
    MR_SPY_PRINT_ALL,
    MR_SPY_PRINT_ONE,
} MR_SpyPrintWhat;

struct MR_SpyPrint_Struct {
    MR_BrowseFormat p_format;
    MR_SpyPrintWhat p_what;
    char            *p_name;    /* if MR_SPY_PRINT_ONE */
    MR_bool         p_warn;
};

struct MR_SpyPrintList_Struct {
    MR_SpyPrint     pl_cur;
    MR_SpyPrintList pl_next;
};

typedef struct MR_SpyPoint_Struct MR_SpyPoint;

typedef struct {
    MR_VarSpec      cond_var_spec;
    char            *cond_path;
    MR_SpyTest      cond_test;
    MR_CTerm        cond_term;
    MR_bool         cond_require_var;
    MR_bool         cond_require_path;
    char            *cond_what_string;
    char            *cond_term_string;
} MR_SpyCond;

struct MR_SpyPoint_Struct {
    MR_bool                 spy_exists;     /* MR_FALSE if deleted */
    MR_bool                 spy_enabled;
    MR_SpyWhen              spy_when;
    MR_SpyAction            spy_action;
    MR_SpyIgnore_When       spy_ignore_when;
    int                     spy_ignore_count;
    MR_SpyCond              *spy_cond;
    MR_SpyPrintList         spy_print_list;
    const MR_ProcLayout     *spy_proc;      /* if not LINENO */
    const MR_LabelLayout    *spy_label;     /* if SPECIFIC */
    char                    *spy_filename;  /* if LINENO */
    int                     spy_linenumber; /* if LINENO */
    MR_SpyPoint             *spy_next;      /* if not LINENO */
};

/*
** The table of spy points, with counters saying which is the next free slot
** and how many slots are allocated.
*/

extern  MR_SpyPoint     **MR_spy_points;
extern  int             MR_spy_point_next;
extern  int             MR_spy_point_max;

extern  int             MR_most_recent_spy_point;

extern  MR_SpyCond      *MR_spy_point_cond_bad;
extern  const char      *MR_spy_point_cond_problem;

/*
** Check whether the event described by the given label layout and port
** matches any spy points. If yes, return MR_TRUE, set *action to say what
** action should be executed for the spy point, and set print_list to the
** variable print list of the first matching spy point.
*/

extern  MR_bool         MR_event_matches_spy_point(
                            const MR_LabelLayout *layout, MR_TracePort port,
                            MR_SpyAction *action,
                            MR_SpyPrintList *print_list);

/*
** Add a new spy point on a procedure (as opposed to on a line number)
** to the table. If this cannot be done, return a negative number and set
** *problem to point to an error message.
*/

extern  int         MR_add_proc_spy_point(MR_SpyWhen when,
                        MR_SpyAction action, MR_SpyIgnore_When ignore_when,
                        int ignore_count, const MR_ProcLayout *entry,
                        const MR_LabelLayout *label,
                        MR_SpyPrintList print_list, const char **problem);

/*
** Add a new spy point on a line number (as opposed to on a procedure)
** to the table. If this cannot be done, return a negative number and set
** *problem to point to an error message.
*/

extern  int         MR_add_line_spy_point(MR_SpyAction action,
                        MR_SpyIgnore_When ignore_when, int ignore_count,
                        const char *filename, int linenumber,
                        MR_SpyPrintList print_list, const char **problem);

/*
** Add the given set of things to be printed to the spy point's list,
** at either the start or the end of the existing list.
*/

extern  void        MR_add_spy_point_print_list_start(int point_slot,
                        MR_SpyPrintList print_list);
extern  void        MR_add_spy_point_print_list_end(int point_slot,
                        MR_SpyPrintList print_list);

/*
** Empty the set of things to be printed at the spy point.
*/

extern  void        MR_clear_spy_point_print_list(int point_slot);

/*
** Apply the given ignore specification to the given spy point.
** If the ignore specification is not appropriate for the spy point,
** return a non-NULL problem report.
*/

extern  const char  *MR_ignore_spy_point(int point_slot,
                        MR_SpyIgnore_When ignore_when, int ignore_count);

/*
** Delete a spy point from the table.
*/

extern  void        MR_delete_spy_point(int point_table_slot);

/*
** Print the spy point with the given number in a nice format for humans to
** read to the given file.
*/

extern  void        MR_print_spy_point(FILE *fp, int i, MR_bool verbose);

/*
** Print the given spy point condition in a user-friendly manner.
*/

extern  void        MR_print_spy_cond(FILE *fp, MR_SpyCond *cond);

/*
** Print the set of current spy points (including those that are currently
** disabled) to fp in a format that, when sourced by mdb, recreates those
** spy points. Any internal errors encountered while trying to do this
** should be reported to err_fp. Return MR_TRUE iff the debugger's data
** structures are inconsistent, and the spy points could not be saved.
*/

extern  MR_bool     MR_save_spy_points(FILE *fp, FILE *err_fp);

#endif  /* not MERCURY_TRACE_SPY_H */
