/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 1998-2006 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module implements the mdb commands in the "breakpoint" category.
**
** The structure of these files is:
**
** - all the #includes
** - local macros and declarations of local static functions
** - one function for each command in the category
** - any auxiliary functions
** - any command argument strings
** - option processing functions.
*/

#include "mercury_std.h"
#include "mercury_getopt.h"

#include "mercury_trace_internal.h"
#include "mercury_trace_cmds.h"
#include "mercury_trace_cmd_breakpoint.h"
#include "mercury_trace_cmd_parameter.h"
#include "mercury_trace_spy.h"
#include "mercury_trace_tables.h"
#include "mercury_trace_util.h"

/****************************************************************************/

typedef enum {
        MR_MULTIMATCH_ASK, MR_MULTIMATCH_ALL, MR_MULTIMATCH_ONE
} MR_MultiMatch;

static  MR_Spy_Print_List
                    MR_add_to_print_list_end(MR_Browse_Format format,
                        char *word, MR_bool warn, 
                        MR_Spy_Print_List print_list);
static  void        MR_maybe_print_spy_point(int slot, const char *problem);
static  MR_bool     MR_parse_source_locn(char *word, const char **file,
                        int *line);

static  MR_bool     MR_trace_options_when_action_multi_ignore(MR_Spy_When *when,
                        MR_Spy_Action *action, MR_MultiMatch *multi_match,
                        MR_Spy_Ignore_When *ignore_when, int *ignore_count,
                        MR_Spy_Print_List *print_list,
                        char ***words, int *word_count);
static  MR_bool     MR_trace_options_condition(int *break_num,
                        MR_bool *require_var, MR_bool *require_path,
                        char ***words, int *word_count);
static  MR_bool     MR_trace_options_ignore_count(
                        MR_Spy_Ignore_When *ignore_when,
                        int *ignore_count, char ***words, int *word_count);
static  MR_bool     MR_trace_options_break_print(MR_Browse_Format *format,
                        MR_bool *at_start, MR_bool *warn, char ***words,
                        int *word_count);
static  MR_bool     MR_trace_options_register(MR_bool *verbose, char ***words,
                        int *word_count);

/****************************************************************************/

MR_Next
MR_trace_cmd_break(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    const MR_Label_Layout   *layout;
    MR_Proc_Spec            spec;
    MR_Spy_When             when;
    MR_Spy_Action           action;
    MR_MultiMatch           multi_match;
    MR_Spy_Ignore_When      ignore_when;
    int                     ignore_count;
    MR_Spy_Print_List       print_list;
    const char              *file;
    int                     line;
    int                     breakline;
    const char              *problem;

    layout = event_info->MR_event_sll;

    if (word_count == 2 && MR_streq(words[1], "info")) {
        int i;
        int count;

        count = 0;
        for (i = 0; i < MR_spy_point_next; i++) {
            if (MR_spy_points[i]->spy_exists) {
                MR_print_spy_point(MR_mdb_out, i, MR_TRUE);
                count++;
            }
        }

        if (count == 0) {
            fprintf(MR_mdb_out, "There are no break points.\n");
        }

        return KEEP_INTERACTING;
    }

    when = MR_default_breakpoint_scope;
    action = MR_SPY_STOP;
    multi_match = MR_MULTIMATCH_ASK;
    /*
    ** The value of ignore_when doesn't matter
    ** while ignore_count contains zero.
    */
    ignore_when = MR_SPY_DONT_IGNORE;
    ignore_count = 0;
    print_list = NULL;
    if (! MR_trace_options_when_action_multi_ignore(&when, &action,
        &multi_match, &ignore_when, &ignore_count, &print_list,
        &words, &word_count))
    {
        ; /* the usage message has already been printed */
    } else if (word_count == 2 && MR_streq(words[1], "here")) {
        int             slot;
        MR_Trace_Port   port;

        port = event_info->MR_trace_port;
        if (ignore_count > 0 && ignore_when == MR_SPY_IGNORE_ENTRY &&
            ! MR_port_is_entry(port))
        {
            fprintf(MR_mdb_out, "That breakpoint "
                "would never become enabled.\n");
            return KEEP_INTERACTING;
        } else if (ignore_count > 0 &&
            ignore_when == MR_SPY_IGNORE_INTERFACE &&
            ! MR_port_is_interface(port))
        {
            fprintf(MR_mdb_out, "That breakpoint "
                "would never become enabled.\n");
            return KEEP_INTERACTING;
        }

        MR_register_all_modules_and_procs(MR_mdb_out, MR_TRUE);
        slot = MR_add_proc_spy_point(MR_SPY_SPECIFIC, action, ignore_when,
            ignore_count, layout->MR_sll_entry, layout, print_list, &problem);
        MR_maybe_print_spy_point(slot, problem);
    } else if (word_count == 2 && MR_parse_proc_spec(words[1], &spec)) {
        MR_Matches_Info matches;
        int             slot;

        MR_register_all_modules_and_procs(MR_mdb_out, MR_TRUE);
        matches = MR_search_for_matching_procedures(&spec);
        if (matches.match_proc_next == 0) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: there is no such procedure.\n");
        } else if (matches.match_proc_next == 1) {
            slot = MR_add_proc_spy_point(when, action, ignore_when,
                ignore_count, matches.match_procs[0], NULL, print_list,
                &problem);
            MR_maybe_print_spy_point(slot, problem);
        } else if (multi_match == MR_MULTIMATCH_ALL) {
            int i;

            for (i = 0; i < matches.match_proc_next; i++) {
                slot = MR_add_proc_spy_point(when, action, ignore_when,
                    ignore_count, matches.match_procs[i], NULL, print_list,
                    &problem);
                MR_maybe_print_spy_point(slot, problem);
            }
        } else {
            char    buf[80];
            int     i;
            char    *line2;

            fflush(MR_mdb_out);
            fprintf(MR_mdb_err,
                "Ambiguous procedure specification. The matches are:\n");

            for (i = 0; i < matches.match_proc_next; i++) {
                fprintf(MR_mdb_out, "%d: ", i);
                MR_print_proc_id_and_nl(MR_mdb_out, matches.match_procs[i]);
            }

            if (multi_match == MR_MULTIMATCH_ONE) {
                return KEEP_INTERACTING;
            }

            sprintf(buf, "\nWhich do you want to put "
                "a breakpoint on (0-%d or *)? ",
                matches.match_proc_next - 1);
            line2 = MR_trace_getline(buf, MR_mdb_in, MR_mdb_out);
            if (line2 == NULL) {
                /* This means the user input EOF. */
                fprintf(MR_mdb_out, "none of them\n");
            } else if (MR_streq(line2, "*")) {
                for (i = 0; i < matches.match_proc_next; i++) {
                    slot = MR_add_proc_spy_point(when, action, ignore_when,
                        ignore_count, matches.match_procs[i], NULL, print_list,
                        &problem);
                    MR_maybe_print_spy_point(slot, problem);
                }

                MR_free(line2);
            } else if (MR_trace_is_natural_number(line2, &i)) {
                if (0 <= i && i < matches.match_proc_next) {
                    slot = MR_add_proc_spy_point(when, action, ignore_when,
                        ignore_count, matches.match_procs[i], NULL, print_list,
                        &problem);
                    MR_maybe_print_spy_point(slot, problem);
                } else {
                    fprintf(MR_mdb_out, "no such match\n");
                }
                MR_free(line2);
            } else {
                fprintf(MR_mdb_out, "none of them\n");
                MR_free(line2);
            }
        }
    } else if (word_count == 2 &&
        MR_parse_source_locn(words[1], &file, &line))
    {
        int slot;

        slot = MR_add_line_spy_point(action, ignore_when, ignore_count, file,
            line, print_list, &problem);
        MR_maybe_print_spy_point(slot, problem);
    } else if (word_count == 2 &&
        MR_trace_is_natural_number(words[1], &breakline))
    {
        int slot;

        if (MR_find_context(layout, &file, &line)) {
            slot = MR_add_line_spy_point(action, ignore_when, ignore_count,
                file, breakline, print_list, &problem);
            MR_maybe_print_spy_point(slot, problem);
        } else {
            MR_fatal_error("cannot find current filename");
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_condition(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    int             break_num;
    MR_bool         require_var;
    MR_bool         require_path;
    int             i;
    const char      *problem;
    MR_CTerm        term;
    MR_Spy_Test     test;
    char            *what_str;
    char            *term_str;
    int             len;
    char            *rest;
    MR_Spy_Cond     *cond;
    MR_Var_Spec     var_spec;
    char            *path;

    break_num = MR_most_recent_spy_point;
    require_var = MR_TRUE;
    require_path = MR_TRUE;
    if (! MR_trace_options_condition(&break_num, &require_var, &require_path,
        &words, &word_count))
    {
        /* the usage message has already been printed */
        return KEEP_INTERACTING;
    } else if (word_count < 4) {
        MR_trace_usage_cur_cmd();
        return KEEP_INTERACTING;
    }

    if (break_num < 0) {
        fprintf(MR_mdb_err, "There is no breakpoint.\n");
        return KEEP_INTERACTING;
    }

    if (! (0 <= break_num && break_num < MR_spy_point_next)) {
        fprintf(MR_mdb_err, "There is no breakpoint %d.\n", break_num);
        return KEEP_INTERACTING;
    }

    if (! MR_spy_points[break_num]->spy_exists) {
        fprintf(MR_mdb_err, "Breakpoint %d has been deleted.\n", break_num);
        return KEEP_INTERACTING;
    }

    cond = MR_malloc(sizeof(MR_Spy_Cond));

    what_str = MR_malloc(strlen(words[1]) + 1);
    strcpy(what_str, words[1]);

    problem = MR_trace_parse_var_path(what_str, &var_spec, &path);
    if (problem != NULL) {
        fprintf(MR_mdb_err, "mdb: %s: %s.\n", what_str, problem);
        return KEEP_INTERACTING;
    }

    if (MR_streq(words[2], "=") || MR_streq(words[2], "==")) {
        test = MR_SPY_TEST_EQUAL;
    } else if (MR_streq(words[2], "!=") || MR_streq(words[2], "\\=")) {
        test = MR_SPY_TEST_NOT_EQUAL;
    } else {
        fprintf(MR_mdb_err, "invalid condition: should be = or !=\n");
        return KEEP_INTERACTING;
    }

    len = 0;
    for (i = 3; i < word_count; i++) {
        len += strlen(words[i]);
    }

    term_str = MR_malloc(len + 1);
    len = 0;
    for (i = 3; i < word_count; i++) {
        strcpy(term_str + len, words[i]);
        len += strlen(words[i]);
    }

    term = MR_create_cterm(term_str, &rest);
    if (term == NULL) {
        fprintf(MR_mdb_out, "syntax error in term\n");
        return KEEP_INTERACTING;
    }

    if (*rest != '\0') {
        fprintf(MR_mdb_out, "syntax error after term\n");
        return KEEP_INTERACTING;
    }

    if (MR_spy_points[break_num]->spy_cond != NULL) {
        MR_delete_cterm(MR_spy_points[break_num]->spy_cond->cond_term);
        MR_free(MR_spy_points[break_num]->spy_cond->cond_what_string);
        MR_free(MR_spy_points[break_num]->spy_cond->cond_term_string);
        MR_free(MR_spy_points[break_num]->spy_cond);
    }

    cond->cond_var_spec = var_spec;
    cond->cond_path = path;
    cond->cond_test = test;
    cond->cond_term = term;
    cond->cond_term_string = term_str;
    cond->cond_what_string = what_str;
    cond->cond_require_var = require_var;
    cond->cond_require_path = require_path;

    MR_spy_points[break_num]->spy_cond = cond;

    MR_print_spy_point(MR_mdb_out, break_num, MR_TRUE);
    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_ignore(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    int                 n;
    MR_Spy_Ignore_When  ignore_when;
    int                 ignore_count;
    const char          *problem;

    ignore_when = MR_SPY_IGNORE_ENTRY;
    ignore_count = 1;
    if (! MR_trace_options_ignore_count(&ignore_when, &ignore_count,
        &words, &word_count))
    {
        ; /* the usage message has already been printed */
    } else if (word_count == 2 && MR_trace_is_natural_number(words[1], &n)) {
        if (0 <= n && n < MR_spy_point_next && MR_spy_points[n]->spy_exists) {
            problem = MR_ignore_spy_point(n, ignore_when, ignore_count);
            MR_maybe_print_spy_point(n, problem);
        } else {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: break point #%d does not exist.\n", n);
        }
    } else if (word_count == 2 && MR_streq(words[1], "*")) {
        int i;
        int count;

        count = 0;
        for (i = 0; i < MR_spy_point_next; i++) {
            if (MR_spy_points[i]->spy_exists) {
                problem = MR_ignore_spy_point(n, ignore_when, ignore_count);
                MR_maybe_print_spy_point(n, problem);
                count++;
            }
        }

        if (count == 0) {
            fprintf(MR_mdb_err, "There are no break points.\n");
        }
    } else if (word_count == 1) {
        if (0 <= MR_most_recent_spy_point
            && MR_most_recent_spy_point < MR_spy_point_next
            && MR_spy_points[MR_most_recent_spy_point]->spy_exists)
        {
            n = MR_most_recent_spy_point;
            problem = MR_ignore_spy_point(n, ignore_when, ignore_count);
            MR_maybe_print_spy_point(n, problem);
        } else {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: there is no most recent break point.\n");
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_break_print(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    int                 n;
    int                 i;
    MR_Browse_Format    format;
    MR_bool             at_start;
    MR_bool             warn;
    MR_Spy_Print_List   print_list;

    if (! MR_trace_options_break_print(&format, &at_start, &warn,
        &words, &word_count))
    {
        ; /* the usage message has already been printed */
    } else if (word_count > 2 && MR_trace_is_natural_number(words[1], &n)) {
        if (word_count == 3 && MR_streq(words[2], "none")) {
            MR_clear_spy_point_print_list(n);
            MR_print_spy_point(MR_mdb_out, n, MR_TRUE);
        } else if (0 <= n && n < MR_spy_point_next
            && MR_spy_points[n]->spy_exists)
        {
            print_list = NULL;
            for (i = 2; i < word_count; i++) {
                print_list = MR_add_to_print_list_end(format, words[i], warn,
                    print_list);
            }

            if (at_start) {
                MR_add_spy_point_print_list_start(n, print_list);
            } else {
                MR_add_spy_point_print_list_end(n, print_list);
            }

            MR_print_spy_point(MR_mdb_out, n, MR_TRUE);
        } else {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: break point #%d does not exist.\n", n);
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_enable(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    int n;

    if (word_count == 2 && MR_trace_is_natural_number(words[1], &n)) {
        if (0 <= n && n < MR_spy_point_next && MR_spy_points[n]->spy_exists) {
            MR_spy_points[n]->spy_enabled = MR_TRUE;
            MR_print_spy_point(MR_mdb_out, n, MR_FALSE);
        } else {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: break point #%d does not exist.\n", n);
        }
    } else if (word_count == 2 && MR_streq(words[1], "*")) {
        int i;
        int count;

        count = 0;
        for (i = 0; i < MR_spy_point_next; i++) {
            if (MR_spy_points[i]->spy_exists) {
                MR_spy_points[i]->spy_enabled = MR_TRUE;
                MR_print_spy_point(MR_mdb_out, i, MR_FALSE);
                count++;
            }
        }

        if (count == 0) {
            fprintf(MR_mdb_err, "There are no break points.\n");
        }
    } else if (word_count == 1) {
        if (0 <= MR_most_recent_spy_point
            && MR_most_recent_spy_point < MR_spy_point_next
            && MR_spy_points[MR_most_recent_spy_point]->spy_exists)
        {
            MR_spy_points[MR_most_recent_spy_point]->spy_enabled = MR_TRUE;
            MR_print_spy_point(MR_mdb_out, MR_most_recent_spy_point, MR_FALSE);
        } else {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: there is no most recent break point.\n");
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_disable(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    int n;

    if (word_count == 2 && MR_trace_is_natural_number(words[1], &n)) {
        if (0 <= n && n < MR_spy_point_next && MR_spy_points[n]->spy_exists) {
            MR_spy_points[n]->spy_enabled = MR_FALSE;
            MR_print_spy_point(MR_mdb_out, n, MR_FALSE);
        } else {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: break point #%d does not exist.\n", n);
        }
    } else if (word_count == 2 && MR_streq(words[1], "*")) {
        int i;
        int count;

        count = 0;
        for (i = 0; i < MR_spy_point_next; i++) {
            if (MR_spy_points[i]->spy_exists) {
                MR_spy_points[i]->spy_enabled = MR_FALSE;
                MR_print_spy_point(MR_mdb_out, i, MR_FALSE);
                count++;
            }
        }

        if (count == 0) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "There are no break points.\n");
        }
    } else if (word_count == 1) {
        if (0 <= MR_most_recent_spy_point
            && MR_most_recent_spy_point < MR_spy_point_next
            && MR_spy_points[MR_most_recent_spy_point]->spy_exists)
        {
            MR_spy_points[MR_most_recent_spy_point]->spy_enabled = MR_FALSE;
            MR_print_spy_point(MR_mdb_out, MR_most_recent_spy_point, MR_FALSE);
        } else {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "There is no most recent break point.\n");
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_delete(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    int n;

    if (word_count == 2 && MR_trace_is_natural_number(words[1], &n)) {
        if (0 <= n && n < MR_spy_point_next && MR_spy_points[n]->spy_exists) {
            MR_spy_points[n]->spy_exists = MR_FALSE;
            MR_print_spy_point(MR_mdb_out, n, MR_FALSE);
            MR_spy_points[n]->spy_exists = MR_TRUE;
            MR_delete_spy_point(n);
        } else {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: break point #%d does not exist.\n", n);
        }
    } else if (word_count == 2 && MR_streq(words[1], "*")) {
        int i;
        int count;

        count = 0;
        for (i = 0; i < MR_spy_point_next; i++) {
            if (MR_spy_points[i]->spy_exists) {
                MR_spy_points[i]->spy_exists = MR_FALSE;
                MR_print_spy_point(MR_mdb_out, i, MR_FALSE);
                MR_spy_points[i]->spy_exists = MR_TRUE;
                MR_delete_spy_point(i);
                count++;
            }
        }

        if (count == 0) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "There are no break points.\n");
        }
    } else if (word_count == 1) {
        if (0 <= MR_most_recent_spy_point
            && MR_most_recent_spy_point < MR_spy_point_next
            && MR_spy_points[MR_most_recent_spy_point]->spy_exists)
        {
            int slot;

            slot = MR_most_recent_spy_point;
            MR_spy_points[slot]->spy_exists = MR_FALSE;
            MR_print_spy_point(MR_mdb_out, slot, MR_FALSE);
            MR_spy_points[slot]->spy_exists = MR_TRUE;
            MR_delete_spy_point(slot);
        } else {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: there is no most recent break point.\n");
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_register(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    MR_bool verbose;

    verbose = MR_TRUE;

    if (! MR_trace_options_register(&verbose, &words, &word_count)) {
        ; /* the usage message has already been printed */
    } else if (word_count == 1) {
        MR_register_all_modules_and_procs(MR_mdb_out, verbose);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_modules(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    if (word_count == 1) {
        MR_register_all_modules_and_procs(MR_mdb_out, MR_TRUE);
        MR_dump_module_list(MR_mdb_out);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_procedures(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    if (word_count == 2) {
        MR_register_all_modules_and_procs(MR_mdb_out, MR_TRUE);
        MR_dump_module_procs(MR_mdb_out, words[1]);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

/****************************************************************************/

static MR_Spy_Print_List
MR_add_to_print_list_end(MR_Browse_Format format, char *word, MR_bool warn,
    MR_Spy_Print_List print_list)
{
    MR_Spy_Print_List   list;
    MR_Spy_Print_List   new_list;
    MR_Spy_Print        new_node;

    new_node = MR_malloc(sizeof(struct MR_Spy_Print_Struct));
    new_node->p_format = format;
    new_node->p_warn = warn;
    if (MR_streq(word, "*")) {
        new_node->p_what = MR_SPY_PRINT_ALL;
        new_node->p_name = NULL;
    } else if (MR_streq(word, "goal")) {
        new_node->p_what = MR_SPY_PRINT_GOAL;
        new_node->p_name = NULL;
    } else {
        new_node->p_what = MR_SPY_PRINT_ONE;
        new_node->p_name = MR_copy_string(word);
    }

    new_list = MR_malloc(sizeof(struct MR_Spy_Print_List_Struct));
    new_list->pl_cur = new_node;
    new_list->pl_next = NULL;

    list = print_list;
    if (list == NULL) {
        return new_list;
    }

    while (list->pl_next != NULL) {
        list = list->pl_next;
    }

    list->pl_next = new_list;
    return print_list;
}

static void
MR_maybe_print_spy_point(int slot, const char *problem)
{
    if (slot < 0) {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "mdb: %s.\n", problem);
    } else {
        MR_print_spy_point(MR_mdb_out, slot, MR_TRUE);
    }
}

static MR_bool
MR_parse_source_locn(char *word, const char **file, int *line)
{
    char        *s;
    const char  *t;

    if ((s = strrchr(word, ':')) != NULL) {
        for (t = s+1; *t != '\0'; t++) {
            if (! MR_isdigit(*t)) {
                return MR_FALSE;
            }
        }

        *s = '\0';
        *file = word;
        *line = atoi(s+1);
        return MR_TRUE;
    }

    return MR_FALSE;
}

/****************************************************************************/

const char *const    MR_trace_break_cmd_args[] =
    { "-A", "-E", "-I", "-O", "-P", "-S", "-a", "-e", "-i",
    "--all", "--entry", "--ignore-entry", "--ignore-interface",
    "--interface", "--print", "--select-all", "--select-one",
    "--stop", "here", "info", NULL };

const char *const    MR_trace_ignore_cmd_args[] =
    { "-E", "-I", "--ignore-entry", "--ignore-interface", NULL };

/****************************************************************************/

static struct MR_option MR_trace_when_action_multi_ignore_opts[] =
{
    { "all",                MR_no_argument,         NULL,   'a' },
    { "entry",              MR_no_argument,         NULL,   'e' },
    { "interface",          MR_no_argument,         NULL,   'i' },
    { "ignore-entry",       MR_required_argument,   NULL,   'E' },
    { "ignore-interface",   MR_required_argument,   NULL,   'I' },
    { "print-list",         MR_required_argument,   NULL,   'p' },
    { "no-warn",            MR_no_argument,         NULL,   'n' },
    { "print",              MR_no_argument,         NULL,   'P' },
    { "stop",               MR_no_argument,         NULL,   'S' },
    { "select-all",         MR_no_argument,         NULL,   'A' },
    { "select-one",         MR_no_argument,         NULL,   'O' },
    { NULL,                 MR_no_argument,         NULL,   0 }
};

static MR_bool
MR_trace_options_when_action_multi_ignore(MR_Spy_When *when,
    MR_Spy_Action *action, MR_MultiMatch *multi_match,
    MR_Spy_Ignore_When*ignore_when, int *ignore_count,
    MR_Spy_Print_List *print_list,
    char ***words, int *word_count)
{
    int                 c;
    MR_Spy_Print        node;
    MR_Spy_Print_List   list;
    MR_bool             warn;

    warn = MR_TRUE;
    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "AE:I:OPSaeinp:",
        MR_trace_when_action_multi_ignore_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'a':
                *when = MR_SPY_ALL;
                break;

            case 'e':
                *when = MR_SPY_ENTRY;
                break;

            case 'i':
                *when = MR_SPY_INTERFACE;
                break;

            case 'n':
                warn = MR_FALSE;
                break;

            case 'p':
                *print_list = MR_add_to_print_list_end(MR_BROWSE_FORMAT_FLAT,
                    MR_optarg, warn, *print_list);
                break;

            case 'E':
                if (! MR_trace_is_natural_number(MR_optarg, ignore_count)) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }
                *ignore_when = MR_SPY_IGNORE_ENTRY;
                break;

            case 'I':
                if (! MR_trace_is_natural_number(MR_optarg, ignore_count)) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }
                *ignore_when = MR_SPY_IGNORE_INTERFACE;
                break;

            case 'A':
                *multi_match = MR_MULTIMATCH_ALL;
                break;

            case 'O':
                *multi_match = MR_MULTIMATCH_ONE;
                break;

            case 'P':
                *action = MR_SPY_PRINT;
                break;

            case 'S':
                *action = MR_SPY_STOP;
                break;

            default:
                MR_trace_usage_cur_cmd();
                return MR_FALSE;
        }
    }

    *words = *words + MR_optind - 1;
    *word_count = *word_count - MR_optind + 1;
    return MR_TRUE;
}

static struct MR_option MR_trace_condition_opts[] =
{
    { "break-num",          MR_required_argument,   NULL,   'n' },
    { "dont-require-var",   MR_no_argument,         NULL,   'v' },
    { "dont-require-path",  MR_no_argument,         NULL,   'p' },
    { NULL,                 MR_no_argument,         NULL,   0 }
};

static MR_bool
MR_trace_options_condition(int *break_num, MR_bool *require_var,
    MR_bool *require_path, char ***words, int *word_count)
{
    int c;
    int n;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "n:vp",
        MR_trace_condition_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'n':
                if (! MR_trace_is_natural_number(MR_optarg, &n)) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }
                *break_num = n;
                break;

            case 'p':
                *require_path = MR_FALSE;
                break;

            case 'v':
                /*
                ** If a variable is missing, then the path inside
                ** is missing as well.
                */

                *require_path = MR_FALSE;
                *require_var = MR_FALSE;
                break;

            default:
                MR_trace_usage_cur_cmd();
                return MR_FALSE;
        }
    }

    *words = *words + MR_optind - 1;
    *word_count = *word_count - MR_optind + 1;
    return MR_TRUE;
}

static struct MR_option MR_trace_ignore_count_opts[] =
{
    { "ignore-entry",       MR_required_argument,   NULL,   'E' },
    { "ignore-interface",   MR_required_argument,   NULL,   'I' },
    { NULL,                 MR_no_argument,         NULL,   0 }
};

static MR_bool
MR_trace_options_ignore_count(MR_Spy_Ignore_When *ignore_when,
    int *ignore_count, char ***words, int *word_count)
{
    int c;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "E:I:",
        MR_trace_ignore_count_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'E':
                if (! MR_trace_is_natural_number(MR_optarg, ignore_count)) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }
                *ignore_when = MR_SPY_IGNORE_ENTRY;
                break;

            case 'I':
                if (! MR_trace_is_natural_number(MR_optarg, ignore_count)) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }
                *ignore_when = MR_SPY_IGNORE_INTERFACE;
                break;

            default:
                MR_trace_usage_cur_cmd();
                return MR_FALSE;
        }
    }

    *words = *words + MR_optind - 1;
    *word_count = *word_count - MR_optind + 1;
    return MR_TRUE;
}

static struct MR_option MR_trace_break_print_opts[] =
{
    { "end",        MR_no_argument, NULL,   'e' },
    { "no-warn",    MR_no_argument, NULL,   'n' },
    { "flat",       MR_no_argument, NULL,   'f' },
    { "raw-pretty", MR_no_argument, NULL,   'r' },
    { "verbose",    MR_no_argument, NULL,   'v' },
    { "pretty",     MR_no_argument, NULL,   'p' },
    { NULL,         MR_no_argument, NULL,   0 }
};

static MR_bool
MR_trace_options_break_print(MR_Browse_Format *format, MR_bool *at_start,
    MR_bool *warn, char ***words, int *word_count)
{
    int c;

    *format = MR_BROWSE_FORMAT_FLAT;
    *at_start = MR_TRUE;
    *warn = MR_TRUE;
    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "enfrvp",
        MR_trace_break_print_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'e':
                *at_start = MR_FALSE;
                break;

            case 'n':
                *warn = MR_FALSE;
                break;

            case 'f':
                *format = MR_BROWSE_FORMAT_FLAT;
                break;

            case 'r':
                *format = MR_BROWSE_FORMAT_RAW_PRETTY;
                break;

            case 'v':
                *format = MR_BROWSE_FORMAT_VERBOSE;
                break;

            case 'p':
                *format = MR_BROWSE_FORMAT_PRETTY;
                break;

            default:
                MR_trace_usage_cur_cmd();
                return MR_FALSE;
        }
    }

    *words = *words + MR_optind - 1;
    *word_count = *word_count - MR_optind + 1;
    return MR_TRUE;
}

static struct MR_option MR_trace_register_opts[] =
{
    { "quiet",      MR_no_argument, NULL,   'q' },
    { "verbose",    MR_no_argument, NULL,   'v' },
    { NULL,         MR_no_argument, NULL,   0 }
};

static MR_bool
MR_trace_options_register(MR_bool *verbose, char ***words, int *word_count)
{
    int c;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "qv",
        MR_trace_register_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'q':
                *verbose = MR_FALSE;
                break;

            case 'v':
                *verbose = MR_TRUE;
                break;

            default:
                MR_trace_usage_cur_cmd();
                return MR_FALSE;
        }
    }

    *words = *words + MR_optind - 1;
    *word_count = *word_count - MR_optind + 1;
    return MR_TRUE;
}
