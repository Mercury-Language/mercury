/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 1998-2006 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module implements the mdb commands in the "browsing" category.
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
#include "mercury_string.h"

#include "mercury_trace_internal.h"
#include "mercury_trace_cmds.h"
#include "mercury_trace_cmd_browsing.h"
#include "mercury_trace_cmd_parameter.h"
#include "mercury_trace_vars.h"
#include "mercury_trace_hold_vars.h"
#include "mercury_trace_browse.h"
#include "mercury_trace_util.h"

#include "mdb.listing.mh"
#include "mdb.diff.mh"
#include "mdb.declarative_execution.mh"
#include "mdbcomp.program_representation.mh"

/****************************************************************************/

static  void        MR_trace_set_level_and_report(int ancestor_level,
                        MR_bool detailed, MR_bool print_optionals);
static  const char  *MR_trace_browse_exception(MR_Event_Info *event_info,
                        MR_Browser browser, MR_Browse_Caller_Type caller,
                        MR_Browse_Format format);
static  const char  *MR_trace_browse_proc_body(MR_Event_Info *event_info,
                        MR_Browser browser, MR_Browse_Caller_Type caller,
                        MR_Browse_Format format);
                        
/* Functions to invoke the user's XML browser on terms or goals */
static  void        MR_trace_browse_xml(MR_Word type_info, MR_Word value,
                        MR_Browse_Caller_Type caller, MR_Browse_Format format);
static  void        MR_trace_browse_goal_xml(MR_ConstString name,
                        MR_Word arg_list, MR_Word is_func,
                        MR_Browse_Caller_Type caller, MR_Browse_Format format);

static  void        MR_trace_cmd_stack_2(MR_Event_Info *event_info,
                        MR_bool detailed, int frame_limit, int line_limit);

static  const char  *MR_trace_new_source_window(const char *window_cmd,
                        const char *server_cmd, const char *server_name,
                        int timeout, MR_bool force, MR_bool verbose,
                        MR_bool split);

static  MR_bool     MR_trace_options_detailed(MR_bool *detailed, char ***words,
                        int *word_count);
static  MR_bool     MR_trace_options_stack_trace(MR_bool *detailed,
                        int *frame_limit, char ***words, int *word_count);
static  MR_bool     MR_trace_options_format(MR_Browse_Format *format,
                        MR_bool *xml, char ***words, int *word_count);
static  MR_bool     MR_trace_options_view(const char **window_cmd,
                        const char **server_cmd, const char **server_name,
                        int *timeout, MR_bool *force, MR_bool *verbose,
                        MR_bool *split, MR_bool *close_window, char ***words,
                        int *word_count);
static  MR_bool     MR_trace_options_diff(int *start, int *max,
                        char ***words, int *word_count);
static  MR_bool     MR_trace_options_dump(MR_bool *xml,
                        char ***words, int *word_count);

/****************************************************************************/

MR_Next
MR_trace_cmd_level(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    int n;
    MR_bool detailed;

    detailed = MR_FALSE;
    if (! MR_trace_options_detailed(&detailed, &words, &word_count)) {
        ; /* the usage message has already been printed */
    } else if (word_count == 2 && MR_trace_is_natural_number(words[1], &n)) {
        MR_trace_set_level_and_report(n, detailed, MR_print_optionals);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_up(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    int     n;
    MR_bool detailed;

    detailed = MR_FALSE;
    if (! MR_trace_options_detailed(&detailed, &words, &word_count)) {
        ; /* the usage message has already been printed */
    } else if (word_count == 2 && MR_trace_is_natural_number(words[1], &n)) {
        MR_trace_set_level_and_report(MR_trace_current_level() + n, detailed,
            MR_print_optionals);
    } else if (word_count == 1) {
        MR_trace_set_level_and_report(MR_trace_current_level() + 1, detailed,
            MR_print_optionals);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_down(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    int     n;
    MR_bool detailed;

    detailed = MR_FALSE;
    if (! MR_trace_options_detailed(&detailed, &words, &word_count)) {
        ; /* the usage message has already been printed */
    } else if (word_count == 2 && MR_trace_is_natural_number(words[1], &n)) {
        MR_trace_set_level_and_report(MR_trace_current_level() - n, detailed,
            MR_print_optionals);
    } else if (word_count == 1) {
        MR_trace_set_level_and_report(MR_trace_current_level() - 1, detailed,
            MR_print_optionals);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_vars(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    if (word_count == 1) {
        const char  *problem;

        problem = MR_trace_list_vars(MR_mdb_out);
        if (problem != NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: %s.\n", problem);
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_held_vars(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    if (word_count == 1) {
        MR_trace_list_held_vars(MR_mdb_out);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_print(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    MR_Browse_Format    format;
    MR_bool             xml;
    int             n;

    if (! MR_trace_options_format(&format, &xml, &words, &word_count)) {
        ; /* the usage message has already been printed */
    } else if (xml) {
        /* the --xml option is not valid for print */
        MR_trace_usage_cur_cmd();
    } else if (word_count == 1) {
        const char  *problem;

        problem = MR_trace_browse_one_goal(MR_mdb_out,
            MR_trace_browse_goal_internal,
            MR_BROWSE_CALLER_PRINT, format);

        if (problem != NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: %s.\n", problem);
        }
    } else if (word_count == 2) {
        const char  *problem;

        if (MR_streq(words[1], "*")) {
            problem = MR_trace_browse_all(MR_mdb_out,
                MR_trace_browse_internal, format);
        } else if (MR_streq(words[1], "goal")) {
            problem = MR_trace_browse_one_goal(MR_mdb_out,
                MR_trace_browse_goal_internal, MR_BROWSE_CALLER_PRINT, format);
        } else if (MR_streq(words[1], "exception")) {
            problem = MR_trace_browse_exception(event_info,
                MR_trace_browse_internal, MR_BROWSE_CALLER_PRINT, format);
        } else if (MR_streq(words[1], "proc_body")) {
            problem = MR_trace_browse_proc_body(event_info,
                MR_trace_browse_internal, MR_BROWSE_CALLER_PRINT, format);
        } else {
            problem = MR_trace_parse_browse_one(MR_mdb_out, MR_TRUE, words[1],
                MR_trace_browse_internal, MR_BROWSE_CALLER_PRINT, format,
                MR_FALSE);
        }

        if (problem != NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: %s.\n", problem);
        }
    } else if (word_count == 3 && MR_streq(words[1], "action")
        && MR_trace_is_natural_number(words[2], &n))
    {
        const char  *problem;

        problem = MR_trace_browse_action(MR_mdb_out, n,
            MR_trace_browse_goal_internal,
            MR_BROWSE_CALLER_PRINT, format);

        if (problem != NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: %s.\n", problem);
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_browse(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    MR_Browse_Format    format;
    MR_bool             xml;
    int                 n;
    MR_GoalBrowser      goal_browser;
    MR_Browser          browser;

    if (! MR_trace_options_format(&format, &xml, &words, &word_count)) {
        ; /* the usage message has already been printed */
    } else {
        if (xml) {
            goal_browser = MR_trace_browse_goal_xml;
            browser = MR_trace_browse_xml;
        } else {
            goal_browser = MR_trace_browse_goal_internal;
            browser = MR_trace_browse_internal;
        }

        if (word_count == 1) {
            const char  *problem;

            problem = MR_trace_browse_one_goal(MR_mdb_out, goal_browser,
                MR_BROWSE_CALLER_BROWSE, format);

            if (problem != NULL) {
                fflush(MR_mdb_out);
                fprintf(MR_mdb_err, "mdb: %s.\n", problem);
            }
        } else if (word_count == 2) {
            const char  *problem;

            if (MR_streq(words[1], "goal")) {
                problem = MR_trace_browse_one_goal(MR_mdb_out, goal_browser,
                    MR_BROWSE_CALLER_BROWSE, format);
            } else if (MR_streq(words[1], "exception")) {
                problem = MR_trace_browse_exception(event_info, browser,
                    MR_BROWSE_CALLER_BROWSE, format);
            } else if (MR_streq(words[1], "proc_body")) {
                problem = MR_trace_browse_proc_body(event_info, browser,
                    MR_BROWSE_CALLER_BROWSE, format);
            } else {
                problem = MR_trace_parse_browse_one(MR_mdb_out, MR_FALSE,
                    words[1], browser, MR_BROWSE_CALLER_BROWSE, format,
                    MR_TRUE);
            }

            if (problem != NULL) {
                fflush(MR_mdb_out);
                fprintf(MR_mdb_err, "mdb: %s.\n", problem);
            }
        } else if (word_count == 3 && MR_streq(words[1], "action")
            && MR_trace_is_natural_number(words[2], &n))
        {
            const char  *problem;

            problem = MR_trace_browse_action(MR_mdb_out, n, goal_browser,
                MR_BROWSE_CALLER_BROWSE, format);

            if (problem != NULL) {
                fflush(MR_mdb_out);
                fprintf(MR_mdb_err, "mdb: %s.\n", problem);
            }
        } else {
            MR_trace_usage_cur_cmd();
        }
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_stack(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    MR_bool     detailed;
    int         frame_limit = 0;
    int         line_limit = MR_stack_default_line_limit;
    int         spec_line_limit;

    detailed = MR_FALSE;
    if (! MR_trace_options_stack_trace(&detailed, &frame_limit,
        &words, &word_count))
    {
        ; /* the usage message has already been printed */
    } else if (word_count == 1) {
        MR_trace_cmd_stack_2(event_info, detailed, frame_limit, line_limit);
    } else if (word_count == 2 &&
        MR_trace_is_natural_number(words[1], &spec_line_limit))
    {
        MR_trace_cmd_stack_2(event_info, detailed, frame_limit,
            spec_line_limit);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_current(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    if (word_count == 1) {
        MR_trace_event_print_internal_report(event_info);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_view(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    const char      *window_cmd = NULL;
    const char      *server_cmd = NULL;
    const char      *server_name = NULL;
    int             timeout = 8;    /* seconds */
    MR_bool         force = MR_FALSE;
    MR_bool         verbose = MR_FALSE;
    MR_bool         split = MR_FALSE;
    MR_bool         close_window = MR_FALSE;
    const char      *msg;

    if (! MR_trace_options_view(&window_cmd, &server_cmd, &server_name,
        &timeout, &force, &verbose, &split, &close_window,
        &words, &word_count))
    {
        ; /* the usage message has already been printed */
    } else if (word_count != 1) {
        MR_trace_usage_cur_cmd();
    } else if (close_window) {
        MR_trace_maybe_close_source_window(verbose);
    } else {
        msg = MR_trace_new_source_window(window_cmd, server_cmd, server_name,
            timeout, force, verbose, split);
        if (msg != NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: %s.\n", msg);
        }

        MR_trace_maybe_sync_source_window(event_info, verbose);
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_hold(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    char        *event_var_name;
    char        *held_var_name;
    MR_TypeInfo type_info;
    MR_Word     value;
    const char  *ignored_name;
    const char  *problem;
    MR_bool     bad_subterm;

    if (word_count == 2) {
        event_var_name = words[1];
        held_var_name = words[1];
    } else if (word_count == 3) {
        event_var_name = words[1];
        held_var_name = words[2];
    } else {
        MR_trace_usage_cur_cmd();
        return KEEP_INTERACTING;
    }

    if (strpbrk(held_var_name, "^/") != NULL) {
        /* Don't allow path separators in variable names. */
        MR_trace_usage_cur_cmd();
        return KEEP_INTERACTING;
    }

    if (held_var_name[0] == '$') {
        /* Ignore any unneeded initial $ signs. */
        held_var_name = &held_var_name[1];
    }

    problem = MR_trace_parse_lookup_var_path(event_var_name, &type_info,
        &value, &bad_subterm);
    if (problem != NULL) {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "mdb: %s%s.\n",
            (bad_subterm? "there is no path " : ""), problem);
        return KEEP_INTERACTING;
    }

    if (! MR_add_hold_var(held_var_name, type_info, value)) {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "mdb: there is already a held variable $%s\n",
            held_var_name);
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_diff(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    int         start;
    int         max;
    char        *name1;
    char        *name2;
    MR_TypeInfo type_info1;
    MR_TypeInfo type_info2;
    MR_Word     value1;
    MR_Word     value2;
    MR_Word     univ1;
    MR_Word     univ2;
    const char  *problem1;
    const char  *problem2;
    MR_bool     bad_subterm1;
    MR_bool     bad_subterm2;

    start = 0;
    max = 20;
    if (! MR_trace_options_diff(&start, &max, &words, &word_count)) {
        /* the usage message has already been printed */
        return KEEP_INTERACTING;
    } else if (word_count != 3) {
        MR_trace_usage_cur_cmd();
        return KEEP_INTERACTING;
    }

    name1 = words[1];
    name2 = words[2];
    problem1 = MR_trace_parse_lookup_var_path(name1, &type_info1, &value1,
        &bad_subterm1);
    problem2 = MR_trace_parse_lookup_var_path(name2, &type_info2, &value2,
        &bad_subterm2);
    if (problem1 != NULL) {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "mdb: %s%s.\n",
            (bad_subterm1? "arg1: there is no path " : ""), problem1);
        return KEEP_INTERACTING;
    }
    if (problem2 != NULL) {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "mdb: %s%s.\n",
            (bad_subterm2? "arg2: there is no path " : ""), problem2);
        return KEEP_INTERACTING;
    }

    MR_TRACE_CALL_MERCURY(
        MR_new_univ_on_hp(univ1, type_info1, value1);
        MR_new_univ_on_hp(univ2, type_info2, value2);
        ML_report_diffs(start, max, univ1, univ2);
    );

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_dump(char **words, int word_count, MR_Trace_Cmd_Info *cmd,
    MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    MR_bool         verbose = MR_FALSE;
    MR_Word         browser_term;
    const char      *problem = NULL;
    MR_bool         xml = MR_FALSE;

    /*
    ** Set this to NULL to avoid uninitialization warnings.
    */
    browser_term = (MR_Word) NULL;

    if (! MR_trace_options_dump(&xml, &words, &word_count)) {
        ; /* the usage message has already been printed */
    } else if (word_count != 3) {
        MR_trace_usage_cur_cmd();
    } else {
        if (MR_streq(words[1], "goal")) {
            const char  *name;
            MR_Word     arg_list;
            MR_bool     is_func;

            problem = NULL;
            MR_convert_goal_to_synthetic_term(&name, &arg_list, &is_func);
            browser_term = MR_synthetic_to_browser_term(name, arg_list,
                is_func);
        } else if (MR_streq(words[1], "exception")) {
            MR_Word exception;

            exception = MR_trace_get_exception_value();
            if (exception == (MR_Word) NULL) {
                problem = "missing exception value";
            } else {
                browser_term = MR_univ_to_browser_term(exception);
            }
        } else if (MR_streq(words[1], "proc_body")) {
            const MR_Proc_Layout    *entry;
            MR_Word                 rep;

            entry = event_info->MR_event_sll->MR_sll_entry;

            if (entry->MR_sle_body_bytes == NULL) {
                problem = "current procedure has no body bytecodes";
            } else {
                MR_TRACE_CALL_MERCURY(
                    MR_DD_trace_read_rep(entry->MR_sle_body_bytes,
                        event_info->MR_event_sll, &rep);
                );

                browser_term = MR_type_value_to_browser_term(
                    (MR_TypeInfo) ML_proc_rep_type(), rep);
            }
        } else {
            MR_Var_Spec var_spec;
            MR_TypeInfo type_info;
            MR_Word     value;
            const char  *name;

            MR_convert_arg_to_var_spec(words[1], &var_spec);
            problem = MR_lookup_unambiguous_var_spec(var_spec,
                &type_info, &value, &name);
            if (problem == NULL) {
                browser_term = MR_type_value_to_browser_term(type_info, value);
            }
        }

        if (problem != NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: %s.\n", problem);
        } else {
            if (xml) {
                MR_trace_save_term_xml(words[2], browser_term);
            } else {
                MR_trace_save_term(words[2], browser_term);
            }
        }
    }

    return KEEP_INTERACTING;
}

/*
** list [num]
**  List num lines of context around the line number of the context of the
**  current point (i.e., level in the call stack).  If num is not given,
**  the number of context lines defaults to the value of the context_lines
**  setting.
**
** TODO: add the following (use MR_parse_source_locn()):
** list filename:num[-num]
**  List a range of lines from a given file.  If only one number is
**  given, the default number of lines of context is used.
*/

MR_Next
MR_trace_cmd_list(char **words, int word_count,
    MR_Trace_Cmd_Info *cmd, MR_Event_Info *event_info, MR_Code **jumpaddr)
{
    const MR_Proc_Layout    *entry_ptr;
    const char              *filename;
    int                     lineno;
    MR_Word                 *base_sp_ptr;
    MR_Word                 *base_curfr_ptr;
    MR_bool                 num = MR_num_context_lines;
    MR_String               aligned_filename;

    if (word_count > 2) {
        MR_trace_usage_cur_cmd();
        return KEEP_INTERACTING;
    }

    if (word_count == 2 && !MR_trace_is_natural_number(words[1], &num)) {
        MR_trace_usage_cur_cmd();
        return KEEP_INTERACTING;
    }

    MR_trace_current_level_details(&entry_ptr, &filename, &lineno,
        &base_sp_ptr, &base_curfr_ptr);

    MR_TRACE_USE_HP(
        MR_make_aligned_string(aligned_filename, (MR_String) filename);
    );

    MR_TRACE_CALL_MERCURY(
        ML_LISTING_list_file(MR_mdb_out, MR_mdb_err, (char *) aligned_filename,
            lineno - num, lineno + num, lineno, MR_listing_path);
    );

    return KEEP_INTERACTING;
}

/****************************************************************************/

static void
MR_trace_set_level_and_report(int ancestor_level, MR_bool detailed,
    MR_bool print_optionals)
{
    const char              *problem;
    const MR_Proc_Layout    *entry;
    MR_Word                 *base_sp;
    MR_Word                 *base_curfr;
    const char              *filename;
    int                     lineno;
    int                     indent;

    problem = MR_trace_set_level(ancestor_level, print_optionals);
    if (problem == NULL) {
        fprintf(MR_mdb_out, "Ancestor level set to %d:\n",
            ancestor_level);
        MR_trace_current_level_details(&entry, &filename, &lineno,
            &base_sp, &base_curfr);
        fprintf(MR_mdb_out, "%4d ", ancestor_level);
        if (detailed) {
            /*
            ** We want to print the trace info first regardless
            ** of the value of MR_context_position.
            */

            MR_print_call_trace_info(MR_mdb_out, entry, base_sp, base_curfr);
            indent = 26;
        } else {
            indent = 5;
        }

        MR_print_proc_id_trace_and_context(MR_mdb_out, MR_FALSE,
            MR_context_position, entry, base_sp, base_curfr, "",
            filename, lineno, MR_FALSE, "", 0, indent);
    } else {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "%s.\n", problem);
    }
}

void
MR_trace_browse_internal(MR_Word type_info, MR_Word value,
    MR_Browse_Caller_Type caller, MR_Browse_Format format)
{
    switch (caller) {

        case MR_BROWSE_CALLER_BROWSE:
            MR_trace_browse(type_info, value, format);
            break;

        case MR_BROWSE_CALLER_PRINT:
        case MR_BROWSE_CALLER_PRINT_ALL:
            fprintf(MR_mdb_out, "\t");
            fflush(MR_mdb_out);
            MR_trace_print(type_info, value, caller, format);
            break;

        default:
            MR_fatal_error("MR_trace_browse_internal: unknown caller type");
    }
}

void
MR_trace_browse_goal_internal(MR_ConstString name, MR_Word arg_list,
    MR_Word is_func, MR_Browse_Caller_Type caller, MR_Browse_Format format)
{
    switch (caller) {

        case MR_BROWSE_CALLER_BROWSE:
            MR_trace_browse_goal(name, arg_list, is_func, format);
            break;

        case MR_BROWSE_CALLER_PRINT:
            MR_trace_print_goal(name, arg_list, is_func, caller, format);
            break;

        case MR_BROWSE_CALLER_PRINT_ALL:
            MR_fatal_error("MR_trace_browse_goal_internal: bad caller type");

        default:
            MR_fatal_error("MR_trace_browse_goal_internal:"
                " unknown caller type");
    }
}

static const char *
MR_trace_browse_exception(MR_Event_Info *event_info, MR_Browser browser,
    MR_Browse_Caller_Type caller, MR_Browse_Format format)
{
    MR_TypeInfo type_info;
    MR_Word     value;
    MR_Word     exception;

    if (event_info->MR_trace_port != MR_PORT_EXCEPTION) {
        return "command only available from EXCP ports";
    }

    exception = MR_trace_get_exception_value();
    if (exception == (MR_Word) NULL) {
        return "missing exception value";
    }

    MR_unravel_univ(exception, type_info, value);

    (*browser)((MR_Word) type_info, value, caller, format);

    return (const char *) NULL;
}

static const char *
MR_trace_browse_proc_body(MR_Event_Info *event_info, MR_Browser browser,
    MR_Browse_Caller_Type caller, MR_Browse_Format format)
{
    const MR_Proc_Layout    *entry;
    MR_Word                 rep;

    entry = event_info->MR_event_sll->MR_sll_entry;

    if (entry->MR_sle_body_bytes == NULL) {
        return "current procedure has no body info";
    }

    MR_TRACE_CALL_MERCURY(
        MR_DD_trace_read_rep(entry->MR_sle_body_bytes,
            event_info->MR_event_sll, &rep);
    );

    (*browser)(ML_proc_rep_type(), rep, caller, format);
    return (const char *) NULL;
}

static void
MR_trace_browse_xml(MR_Word type_info, MR_Word value,
    MR_Browse_Caller_Type caller, MR_Browse_Format format)
{
    MR_Word     browser_term;

    browser_term = MR_type_value_to_browser_term((MR_TypeInfo) type_info,
        value);

    MR_trace_save_and_invoke_xml_browser(browser_term);
}

static void
MR_trace_browse_goal_xml(MR_ConstString name, MR_Word arg_list,
    MR_Word is_func, MR_Browse_Caller_Type caller, MR_Browse_Format format)
{
    MR_Word     browser_term;

    browser_term = MR_synthetic_to_browser_term(name, arg_list, is_func);
    MR_trace_save_and_invoke_xml_browser(browser_term);
}

static void
MR_trace_cmd_stack_2(MR_Event_Info *event_info, MR_bool detailed,
    int frame_limit, int line_limit)
{
    const MR_Label_Layout   *layout;
    MR_Word                 *saved_regs;
    const char              *msg;

    layout = event_info->MR_event_sll;
    saved_regs = event_info->MR_saved_regs;

    MR_trace_init_modules();
    msg = MR_dump_stack_from_layout(MR_mdb_out, layout,
        MR_saved_sp(saved_regs), MR_saved_curfr(saved_regs),
        detailed, MR_context_position != MR_CONTEXT_NOWHERE,
        frame_limit, line_limit, &MR_dump_stack_record_print);

    if (msg != NULL) {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "%s.\n", msg);
    }
}

/*
** Implement the `view' command. First, check if there is a server attached.
** If so, either stop it or abort the command, depending on whether '-f'
** was given. Then, if a server name was not supplied, start a new server
** with a unique name (which has been MR_malloc'd), otherwise attach to the
** server with the supplied name (and make a MR_malloc'd copy of the name).
*/

static const char *
MR_trace_new_source_window(const char *window_cmd, const char *server_cmd,
    const char *server_name, int timeout, MR_bool force,
    MR_bool verbose, MR_bool split)
{
    const char  *msg;

    if (MR_trace_source_server.server_name != NULL) {
        /*
        ** We are already attached to a server.
        */
        if (force) {
            MR_trace_maybe_close_source_window(verbose);
        } else {
            return "error: server already open (use '-f' to force)";
        }
    }

    MR_trace_source_server.split = split;
    if (server_cmd != NULL) {
        MR_trace_source_server.server_cmd = MR_copy_string(server_cmd);
    } else {
        MR_trace_source_server.server_cmd = NULL;
    }

    if (server_name == NULL) {
        msg = MR_trace_source_open_server(&MR_trace_source_server,
                window_cmd, timeout, verbose);
    } else {
        MR_trace_source_server.server_name = MR_copy_string(server_name);
        msg = MR_trace_source_attach(&MR_trace_source_server, timeout,
            verbose);
        if (msg != NULL) {
            /*
            ** Something went wrong, so we should free the
            ** strings we allocated just above.
            */
            MR_free(MR_trace_source_server.server_name);
            MR_trace_source_server.server_name = NULL;
            MR_free(MR_trace_source_server.server_cmd);
            MR_trace_source_server.server_cmd = NULL;
        }
    }

    return msg;
}

void
MR_trace_maybe_sync_source_window(MR_Event_Info *event_info, MR_bool verbose)
{
    const MR_Label_Layout   *parent;
    const char              *filename;
    int                     lineno;
    const char              *parent_filename;
    int                     parent_lineno;
    const char              *problem; /* not used */
    MR_Word                 *base_sp, *base_curfr;
    const char              *msg;

    if (MR_trace_source_server.server_name != NULL) {
        lineno = 0;
        filename = "";
        parent_lineno = 0;
        parent_filename = "";

        /*
        ** At interface ports we send both the parent context and
        ** the current context.  Otherwise, we just send the current
        ** context.
        */
        if (MR_port_is_interface(event_info->MR_trace_port)) {
            base_sp = MR_saved_sp(event_info->MR_saved_regs);
            base_curfr = MR_saved_curfr(event_info->MR_saved_regs);
            parent = MR_find_nth_ancestor(event_info->MR_event_sll, 1,
                &base_sp, &base_curfr, &problem);
            if (parent != NULL) {
                (void) MR_find_context(parent, &parent_filename,
                   &parent_lineno);
            }
        }

        if (filename[0] == '\0') {
            (void) MR_find_context(event_info->MR_event_sll,
                    &filename, &lineno);
        }

        msg = MR_trace_source_sync(&MR_trace_source_server, filename, lineno,
            parent_filename, parent_lineno, verbose);
        if (msg != NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: %s.\n", msg);
        }
    }
}

void
MR_trace_maybe_close_source_window(MR_bool verbose)
{
    const char  *msg;

    if (MR_trace_source_server.server_name != NULL) {
        msg = MR_trace_source_close(&MR_trace_source_server, verbose);
        if (msg != NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: %s.\n", msg);
        }

        MR_free(MR_trace_source_server.server_name);
        MR_trace_source_server.server_name = NULL;
        MR_free(MR_trace_source_server.server_cmd);
        MR_trace_source_server.server_cmd = NULL;
    }
}

/****************************************************************************/

const char *const    MR_trace_print_cmd_args[] =
    { "-f", "-p", "-v", "--flat", "--pretty", "--verbose",
    "exception", "goal", "*", NULL };
    
/*
** It is better to have a single completion where possible,
** so don't include `-d' here.
*/  

const char *const    MR_trace_stack_cmd_args[] =
    { "--detailed", NULL };
    
const char *const    MR_trace_view_cmd_args[] =
    { "-c", "-f", "-n", "-s", "-t", "-v", "-w", "-2",
    "--close", "--verbose", "--force", "--split-screen", 
    "--window-command", "--server-command", "--server-name",
    "--timeout", NULL };

/****************************************************************************/

static struct MR_option MR_trace_detailed_opts[] =
{
    { "detailed",   MR_no_argument, NULL,   'd' },
    { NULL,         MR_no_argument, NULL,   0   }
};

static MR_bool
MR_trace_options_detailed(MR_bool *detailed, char ***words, int *word_count)
{
    int c;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "d",
        MR_trace_detailed_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'd':
                *detailed = MR_TRUE;
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

static MR_bool
MR_trace_options_stack_trace(MR_bool *detailed, int *frame_limit,
    char ***words, int *word_count)
{
    int c;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "df:",
        MR_trace_detailed_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'd':
                *detailed = MR_TRUE;
                break;

            case 'f':
                if (! MR_trace_is_natural_number(MR_optarg, frame_limit)) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }
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

static struct MR_option MR_trace_format_opts[] =
{
    { "flat",       MR_no_argument, NULL,   'f' },
    { "raw_pretty", MR_no_argument, NULL,   'r' },
    { "verbose",    MR_no_argument, NULL,   'v' },
    { "pretty",     MR_no_argument, NULL,   'p' },
    { "xml",        MR_no_argument, NULL,   'x' },
    { NULL,         MR_no_argument, NULL,   0   }
};

static MR_bool
MR_trace_options_format(MR_Browse_Format *format, MR_bool *xml, char ***words,
    int *word_count)
{
    int c;

    *format = MR_BROWSE_DEFAULT_FORMAT;
    *xml = MR_FALSE;
    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "frvpx",
        MR_trace_format_opts, NULL)) != EOF)
    {
        switch (c) {

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

            case 'x':
                *xml = MR_TRUE;
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

static struct MR_option MR_trace_view_opts[] =
{
    { "close",          MR_no_argument,         NULL,   'c' },
    { "window-command", MR_required_argument,   NULL,   'w' },
    { "server-command", MR_required_argument,   NULL,   's' },
    { "server-name",    MR_required_argument,   NULL,   'n' },
    { "timeout",        MR_required_argument,   NULL,   't' },
    { "force",          MR_no_argument,         NULL,   'f' },
    { "verbose",        MR_no_argument,         NULL,   'v' },
    { "split-screen",   MR_no_argument,         NULL,   '2' },
    { NULL,             MR_no_argument,         NULL,   0   }
};

static MR_bool
MR_trace_options_view(const char **window_cmd, const char **server_cmd,
    const char **server_name, int *timeout, MR_bool *force,
    MR_bool *verbose, MR_bool *split, MR_bool *close_window,
    char ***words, int *word_count)
{
    int c;
    MR_bool no_close = MR_FALSE;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "cw:s:n:t:fv2",
        MR_trace_view_opts, NULL)) != EOF)
    {
        /*
        ** Option '-c' is mutually incompatible with '-f', '-t',
        ** '-s', '-n', '-w' and '-2'.
        */
        switch (c) {

            case 'c':
                if (no_close) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }

                *close_window = MR_TRUE;
                break;

            case 'w':
                if (*close_window) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }

                *window_cmd = MR_optarg;
                no_close = MR_TRUE;
                break;

            case 's':
                if (*close_window) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }

                *server_cmd = MR_optarg;
                no_close = MR_TRUE;
                break;

            case 'n':
                if (*close_window) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }

                *server_name = MR_optarg;
                no_close = MR_TRUE;
                break;

            case 't':
                if (*close_window ||
                    ! MR_trace_is_natural_number(MR_optarg, timeout))
                {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }

                no_close = MR_TRUE;
                break;

            case 'f':
                if (*close_window) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }

                *force = MR_TRUE;
                no_close = MR_TRUE;
                break;

            case 'v':
                *verbose = MR_TRUE;
                break;

            case '2':
                if (*close_window) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }

                *split = MR_TRUE;
                no_close = MR_TRUE;
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

static struct MR_option MR_trace_diff_opts[] =
{
    { "start",      MR_required_argument,   NULL,   's' },
    { "max",        MR_required_argument,   NULL,   'm' },
    { NULL,         MR_no_argument,         NULL,   0   }
};

static MR_bool
MR_trace_options_diff(int *start, int *max, char ***words, int *word_count)
{
    int c;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "m:s:",
        MR_trace_diff_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'm':
                if (! MR_trace_is_natural_number(MR_optarg, max)) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }
                break;

            case 's':
                if (! MR_trace_is_natural_number(MR_optarg, start)) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }
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

static struct MR_option MR_trace_dump_opts[] =
{
    { "xml",        MR_no_argument,     NULL,   'x' },
    { NULL,         MR_no_argument,     NULL,   0   }
};

static MR_bool
MR_trace_options_dump(MR_bool *xml, char ***words, int *word_count)
{
    int c;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "x",
        MR_trace_dump_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'x':
                *xml = MR_TRUE;
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

/****************************************************************************/
