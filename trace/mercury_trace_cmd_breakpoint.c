// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2007 The University of Melbourne.
// Copyright (C) 2014-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module implements the mdb commands in the "breakpoint" category.
//
// The structure of these files is:
//
// - all the #includes
// - local macros and declarations of local static functions
// - one function for each command in the category
// - any auxiliary functions
// - any command argument strings
// - option processing functions.

#include "mercury_std.h"
#include "mercury_getopt.h"

#include "mercury_trace_internal.h"
#include "mercury_trace_cmds.h"
#include "mercury_trace_cmd_breakpoint.h"
#include "mercury_trace_cmd_parameter.h"
#include "mercury_trace_spy.h"
#include "mercury_trace_tables.h"
#include "mercury_trace_util.h"

////////////////////////////////////////////////////////////////////////////

typedef enum {
        MR_MULTIMATCH_ASK, MR_MULTIMATCH_ALL, MR_MULTIMATCH_ONE
} MR_MultiMatch;

static  MR_bool     MR_matches_port_name(const char *word);
static  const char  *MR_parse_spy_print(MR_BrowseFormat format, MR_bool warn,
                        char *word, MR_SpyPrint *sp_ptr);
static  MR_SpyPrintList
                    MR_add_to_print_list_end(MR_SpyPrint sp,
                        MR_SpyPrintList print_list);
static  void        MR_maybe_print_spy_point(int slot, const char *problem);
static  MR_bool     MR_parse_source_locn(char *word, const char **file,
                        int *line);

static  MR_bool     MR_trace_options_when_action_multi_ignore(MR_SpyWhen *when,
                        MR_SpyAction *action, MR_MultiMatch *multi_match,
                        MR_SpyIgnore_When *ignore_when,
                        MR_IgnoreCount *ignore_count,
                        MR_SpyPrintList *print_list,
                        char ***words, int *word_count);
static  MR_bool     MR_trace_options_condition(int *break_num,
                        MR_bool *require_var, MR_bool *require_path,
                        char ***words, int *word_count);
static  MR_bool     MR_trace_options_ignore_count(
                        MR_SpyIgnore_When *ignore_when,
                        MR_IgnoreCount *ignore_count, char ***words,
                        int *word_count);
static  MR_bool     MR_trace_options_break_print(int *break_num,
                        MR_BrowseFormat *format, MR_bool *at_start,
                        MR_bool *warn, char ***words, int *word_count);
static  MR_bool     MR_trace_options_register(MR_bool *verbose, char ***words,
                        int *word_count);

////////////////////////////////////////////////////////////////////////////

MR_Next
MR_trace_cmd_break(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    const MR_LabelLayout    *layout;
    MR_ProcSpec             proc_spec;
    MR_SpyWhen              when;
    MR_SpyAction            action;
    MR_MultiMatch           multi_match;
    MR_SpyIgnore_When       ignore_when;
    MR_IgnoreCount          ignore_count;
    MR_SpyPrintList         print_list;
    const char              *file;
    int                     line;
    MR_Unsigned             breakline;
    const char              *problem;

    layout = event_info->MR_event_sll;

    if (word_count == 2 && MR_streq(words[1], "info")) {
        int i;
        int count;

        count = 0;
        for (i = 0; i < MR_spy_point_next; i++) {
            if (MR_spy_points[i]->MR_spy_exists) {
                MR_print_spy_point(MR_mdb_out, i, MR_TRUE);
                count++;
            }
        }

        if (count == 0) {
            fprintf(MR_mdb_out, "There are no break points.\n");
        }

        return KEEP_INTERACTING;
    }

    MR_register_all_modules_and_procs(MR_mdb_out, MR_TRUE);

    when = MR_default_breakpoint_scope;
    action = MR_SPY_STOP;
    multi_match = MR_MULTIMATCH_ASK;
    // The value of ignore_when doesn't matter while ignore_count == 0.
    ignore_when = MR_SPY_DONT_IGNORE;
    ignore_count = 0;
    print_list = NULL;
    if (! MR_trace_options_when_action_multi_ignore(&when, &action,
        &multi_match, &ignore_when, &ignore_count, &print_list,
        &words, &word_count))
    {
        // The usage message has already been printed.
        ;
    } else if (word_count >= 2 && MR_streq(words[1], "user_event")) {
        const MR_UserEventSpec  *user_event_spec;
        const char              *user_event_set;
        const char              *user_event_name;
        int                     slot;
        int                     set;
        int                     spec;
        MR_bool                 found_event_set;
        MR_bool                 found_event_name;

        if (word_count == 2) {
            user_event_set = NULL;
            user_event_name = NULL;
        } else if (word_count == 3) {
            user_event_set = NULL;
            user_event_name = words[2];
        } else if (word_count == 4) {
            user_event_set = words[2];
            user_event_name = words[3];
        } else {
            MR_trace_usage_cur_cmd();
            return KEEP_INTERACTING;
        }

        if (user_event_name != NULL) {
            found_event_set = MR_FALSE;
            found_event_name = MR_FALSE;
            for (set = 0; set < MR_trace_event_set_next; set++) {
                if (user_event_set == NULL ||
                    MR_streq(user_event_set,
                        MR_trace_event_sets[set].MR_tes_name))
                {
                    if (user_event_set != NULL) {
                        found_event_set = MR_TRUE;
                    }

                    for (spec = 0;
                        spec < MR_trace_event_sets[set].MR_tes_num_specs;
                        spec++)
                    {
                        if (MR_trace_event_sets[set].MR_tes_specs == NULL) {
                            // We couldn't parse the event set specification
                            // in the module. The error message has already
                            // been printed, so just ignore the event set.

                            continue;
                        }

                        user_event_spec =
                            &MR_trace_event_sets[set].MR_tes_specs[spec];
                        if (MR_streq(user_event_name,
                            user_event_spec->MR_ues_event_name))
                        {
                            found_event_name = MR_TRUE;
                        }
                    }
                }
            }

            if (user_event_set != NULL && ! found_event_set) {
                fprintf(MR_mdb_out,
                    "There is no user event set named `%s'.\n",
                    user_event_set);
                return KEEP_INTERACTING;
            }

            if (! found_event_name) {
                if (user_event_set == NULL) {
                    fprintf(MR_mdb_out,
                        "There is no user event named `%s'.\n",
                        user_event_name);
                } else {
                    fprintf(MR_mdb_out,
                        "There is no user event named `%s' "
                        "in event set `%s'.\n",
                        user_event_set, user_event_name);
                }

                return KEEP_INTERACTING;
            }
        }

        if (ignore_count > 0 && ignore_when == MR_SPY_IGNORE_ENTRY) {
            fprintf(MR_mdb_out, "That breakpoint "
                "would never become enabled.\n");
            return KEEP_INTERACTING;
        } else if (ignore_count > 0 &&
            ignore_when == MR_SPY_IGNORE_INTERFACE)
        {
            fprintf(MR_mdb_out, "That breakpoint "
                "would never become enabled.\n");
            return KEEP_INTERACTING;
        }

        slot = MR_add_user_event_spy_point(action, ignore_when, ignore_count,
            user_event_set, user_event_name, print_list, &problem);
        MR_maybe_print_spy_point(slot, problem);
    } else if (word_count >= 2 && MR_streq(words[1], "user_event_set")) {
        const char              *user_event_set;
        int                     slot;
        int                     set;
        MR_bool                 found_event_set;

        if (word_count == 2) {
            user_event_set = NULL;
        } else if (word_count == 3) {
            user_event_set = words[2];
        } else {
            MR_trace_usage_cur_cmd();
            return KEEP_INTERACTING;
        }

        if (user_event_set != NULL) {
            found_event_set = MR_FALSE;
            for (set = 0; set < MR_trace_event_set_next; set++) {
                if (MR_streq(user_event_set,
                    MR_trace_event_sets[set].MR_tes_name))
                {
                    found_event_set = MR_TRUE;
                }
            }

            if (! found_event_set) {
                fprintf(MR_mdb_out,
                    "There is no user event set named `%s'.\n",
                    user_event_set);
                return KEEP_INTERACTING;
            }
        }

        if (ignore_count > 0 && ignore_when == MR_SPY_IGNORE_ENTRY) {
            fprintf(MR_mdb_out, "That breakpoint "
                "would never become enabled.\n");
            return KEEP_INTERACTING;
        } else if (ignore_count > 0 &&
            ignore_when == MR_SPY_IGNORE_INTERFACE)
        {
            fprintf(MR_mdb_out, "That breakpoint "
                "would never become enabled.\n");
            return KEEP_INTERACTING;
        }

        slot = MR_add_user_event_spy_point(action, ignore_when, ignore_count,
            user_event_set, NULL, print_list, &problem);
        MR_maybe_print_spy_point(slot, problem);
    } else if (word_count == 2 && MR_streq(words[1], "here")) {
        int             slot;
        MR_TracePort    port;

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

        slot = MR_add_proc_spy_point(MR_SPY_SPECIFIC, action, ignore_when,
            ignore_count, layout->MR_sll_entry, layout, print_list, &problem);
        MR_maybe_print_spy_point(slot, problem);
    } else if (word_count == 2 && MR_parse_proc_spec(words[1], &proc_spec)) {
        MR_MatchesInfo  matches;
        int             slot;

        matches = MR_search_for_matching_procedures(&proc_spec);
        if (matches.match_proc_next == 0) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: there is no such procedure.\n");
        } else if (matches.match_proc_next == 1) {
            slot = MR_add_proc_spy_point(when, action, ignore_when,
                ignore_count, matches.match_procs[0], NULL,
                print_list, &problem);
            MR_maybe_print_spy_point(slot, problem);
        } else if (multi_match == MR_MULTIMATCH_ALL) {
            int i;

            for (i = 0; i < matches.match_proc_next; i++) {
                slot = MR_add_proc_spy_point(when, action, ignore_when,
                    ignore_count, matches.match_procs[i], NULL,
                    print_list, &problem);
                MR_maybe_print_spy_point(slot, problem);
            }
        } else {
            char        buf[80];
            MR_Unsigned i;
            char        *line2;

            fprintf(MR_mdb_out,
                "Ambiguous procedure specification. The matches are:\n");

            for (i = 0; i < matches.match_proc_next; i++) {
                fprintf(MR_mdb_out, "%" MR_INTEGER_LENGTH_MODIFIER "u: ", i);
                MR_print_proc_id_and_nl(MR_mdb_out, matches.match_procs[i]);
            }

            if (multi_match == MR_MULTIMATCH_ONE) {
                return KEEP_INTERACTING;
            }

            sprintf(buf, "\nWhich do you want to put "
                "a breakpoint on (0-%" MR_INTEGER_LENGTH_MODIFIER "u or *)? ",
                matches.match_proc_next - 1);
            line2 = MR_trace_getline(buf, MR_mdb_in, MR_mdb_out);
            if (line2 == NULL) {
                // This means the user input EOF.
                fprintf(MR_mdb_out, "none of them\n");
            } else if (MR_streq(line2, "*")) {
                for (i = 0; i < matches.match_proc_next; i++) {
                    slot = MR_add_proc_spy_point(when, action, ignore_when,
                        ignore_count, matches.match_procs[i], NULL,
                        print_list, &problem);
                    MR_maybe_print_spy_point(slot, problem);
                }

                MR_free(line2);
            } else if (MR_trace_is_natural_number(line2, &i)) {
                if (0 <= i && i < matches.match_proc_next) {
                    slot = MR_add_proc_spy_point(when, action, ignore_when,
                        ignore_count, matches.match_procs[i], NULL,
                        print_list, &problem);
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
    } else if (word_count == 3 && MR_parse_proc_spec(words[1], &proc_spec) &&
        MR_matches_port_name(words[2]) && MR_strdiff(words[2], "EXCP"))
    {
        const MR_ProcLayout     *selected_proc;
        const MR_LabelLayout    *selected_label;
        MR_MatchesInfo          matches;
        MR_TracePort            port;
        const MR_LabelLayout    **matching_labels;
        MR_Unsigned             matching_port_count;
        int                     slot;
        MR_Unsigned             i;

        if (multi_match == MR_MULTIMATCH_ALL) {
            fprintf(MR_mdb_err, "Warning: "
                "the -A option is ignored when a port is specified.\n");
            multi_match = MR_MULTIMATCH_ASK;
        }

        matches = MR_search_for_matching_procedures(&proc_spec);
        if (matches.match_proc_next == 0) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: there is no such procedure.\n");
            return KEEP_INTERACTING;
        } else if (matches.match_proc_next == 1) {
            selected_proc = matches.match_procs[0];
        } else {
            char    buf[80];
            char    *line2;

            fprintf(MR_mdb_out,
                "Ambiguous procedure specification. The matches are:\n");

            for (i = 0; i < matches.match_proc_next; i++) {
                fprintf(MR_mdb_out, "%" MR_INTEGER_LENGTH_MODIFIER "u: ", i);
                MR_print_proc_id_and_nl(MR_mdb_out, matches.match_procs[i]);
            }

            if (multi_match == MR_MULTIMATCH_ONE) {
                return KEEP_INTERACTING;
            }

            sprintf(buf, "\nWhich do you want to put a breakpoint on (0-%"
                MR_INTEGER_LENGTH_MODIFIER "u)? ",
                matches.match_proc_next - 1);
            line2 = MR_trace_getline(buf, MR_mdb_in, MR_mdb_out);
            if (line2 == NULL) {
                // This means the user input EOF.
                fprintf(MR_mdb_out, "none of them\n");
                return KEEP_INTERACTING;
            } else if (MR_trace_is_natural_number(line2, &i)) {
                if (0 <= i && i < matches.match_proc_next) {
                    selected_proc = matches.match_procs[i];
                } else {
                    fprintf(MR_mdb_out, "no such match\n");
                    MR_free(line2);
                    return KEEP_INTERACTING;
                }
            } else {
                fprintf(MR_mdb_out, "none of them\n");
                MR_free(line2);
                return KEEP_INTERACTING;
            }
        }

        matching_port_count = 0;
        matching_labels = MR_malloc(sizeof(const MR_LabelLayout *) *
            selected_proc->MR_sle_num_labels);
        for (i = 0; i < selected_proc->MR_sle_num_labels; i++) {
            if (selected_proc->MR_sle_labels[i]->MR_sll_port < 0) {
                continue;
            }

            port = (MR_TracePort) selected_proc->MR_sle_labels[i]->MR_sll_port;
            if (MR_streq(MR_simplified_port_names[port], words[2])) {
                matching_labels[matching_port_count] =
                    selected_proc->MR_sle_labels[i];
                matching_port_count++;
            }
        }

        if (matching_port_count == 0) {
            fprintf(MR_mdb_out, "There is no %s port in ", words[2]);
            MR_print_proc_id(MR_mdb_out, selected_proc);
            fprintf(MR_mdb_out, ".\n");
            MR_free(matching_labels);
            return KEEP_INTERACTING;
        } else if (matching_port_count == 1) {
            selected_label = matching_labels[0];
        } else {
            char    buf[80];
            char    *line2;

            fprintf(MR_mdb_out,
                "Ambiguous port specification. The matches are:\n");

            for (i = 0; i < matching_port_count; i++) {
                const MR_LabelLayout    *this_label;

                this_label = matching_labels[i];
                fprintf(MR_mdb_out, "%" MR_INTEGER_LENGTH_MODIFIER
                    "u: %4s %s\n",
                    i,
                    MR_simplified_port_names[this_label->MR_sll_port],
                    MR_label_goal_path(this_label));
            }

            sprintf(buf, "\nWhich do you want to put "
                "a breakpoint on (0-%" MR_INTEGER_LENGTH_MODIFIER "u or *)? ",
                matching_port_count - 1);
            line2 = MR_trace_getline(buf, MR_mdb_in, MR_mdb_out);
            if (line2 == NULL) {
                // This means the user input EOF.
                fprintf(MR_mdb_out, "none of them\n");
                MR_free(matching_labels);
                return KEEP_INTERACTING;
            } else if (MR_streq(line2, "*")) {
                for (i = 0; i < matching_port_count; i++) {
                    slot = MR_add_proc_spy_point(MR_SPY_SPECIFIC, action,
                        ignore_when, ignore_count, selected_proc,
                        matching_labels[i], print_list, &problem);
                    MR_maybe_print_spy_point(slot, problem);
                }

                MR_free(matching_labels);
                MR_free(line2);
                return KEEP_INTERACTING;
            } else if (MR_trace_is_natural_number(line2, &i)) {
                if (0 <= i && i < matching_port_count) {
                    selected_label = matching_labels[i];
                } else {
                    fprintf(MR_mdb_out, "no such match\n");
                    MR_free(matching_labels);
                    MR_free(line2);
                    return KEEP_INTERACTING;
                }
            } else {
                fprintf(MR_mdb_out, "none of them\n");
                MR_free(matching_labels);
                MR_free(line2);
                return KEEP_INTERACTING;
            }
        }

        slot = MR_add_proc_spy_point(MR_SPY_SPECIFIC, action, ignore_when,
            ignore_count, selected_proc, selected_label, print_list, &problem);
        MR_maybe_print_spy_point(slot, problem);
        MR_free(matching_labels);
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
MR_trace_cmd_condition(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    int             break_num;
    MR_bool         require_var;
    MR_bool         require_path;
    int             i;
    const char      *problem;
    MR_CTerm        term;
    MR_SpyTest      test;
    char            *what_str;
    char            *term_str;
    size_t          len;
    char            *rest;
    MR_SpyCond      *cond;
    MR_VarSpec      var_spec;
    char            *path;
    MR_bool         mismatch;
    char            *error_point;

    break_num = MR_most_recent_spy_point;
    require_var = MR_TRUE;
    require_path = MR_TRUE;
    if (! MR_trace_options_condition(&break_num, &require_var, &require_path,
        &words, &word_count))
    {
        // The usage message has already been printed.
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

    if (! MR_spy_points[break_num]->MR_spy_exists) {
        fprintf(MR_mdb_err, "Breakpoint %d has been deleted.\n", break_num);
        return KEEP_INTERACTING;
    }

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
        if (i > 3) {
            len += 1;
        }

        len += strlen(words[i]);
    }

    term_str = MR_malloc(len + 1);
    len = 0;
    for (i = 3; i < word_count; i++) {
        if (i > 3) {
            strcpy(term_str + len, " ");
            len += 1;
        }

        strcpy(term_str + len, words[i]);
        len += strlen(words[i]);
    }

    term = MR_create_cterm(term_str, &rest, &mismatch, &error_point);
    if (term == NULL) {
        const char  *msg;
        size_t      j;

        msg = "syntax error in term: ";
        fprintf(MR_mdb_out, "%s%s\n", msg, term_str);
        if (term_str <= error_point &&
            error_point < term_str + strlen(term_str))
        {
            for (j = 0; j < strlen(msg); j++) {
                putc(' ', MR_mdb_out);
            }

            for (j = 0; term_str + j != error_point; j++) {
                putc(' ', MR_mdb_out);
            }

            if (mismatch) {
                fprintf(MR_mdb_out, "^ unmatched character\n");
            } else {
                fprintf(MR_mdb_out, "^ here\n");
            }
        }

        return KEEP_INTERACTING;
    }

    if (*rest != '\0') {
        fprintf(MR_mdb_out, "syntax error after term\n");
        return KEEP_INTERACTING;
    }

    if (MR_spy_points[break_num]->MR_spy_cond != NULL) {
        MR_delete_cterm(MR_spy_points[break_num]->MR_spy_cond->MR_cond_term);
        MR_free(MR_spy_points[break_num]->MR_spy_cond->MR_cond_what_string);
        MR_free(MR_spy_points[break_num]->MR_spy_cond);
    }

    if (MR_spy_points[break_num]->MR_spy_when == MR_SPY_USER_EVENT_SET) {
        // If the breakpoint is matched by all events in an event set (or
        // possibly all events in all event sets), then it doesn't make sense
        // to insist on any given variable name existing at the matching event.

        require_var = MR_FALSE;
        require_path = MR_FALSE;
    }

    MR_free(term_str);

    cond = MR_malloc(sizeof(MR_SpyCond));
    cond->MR_cond_var_spec = var_spec;
    cond->MR_cond_path = path;
    cond->MR_cond_test = test;
    cond->MR_cond_term = term;
    cond->MR_cond_what_string = what_str;
    cond->MR_cond_require_var = require_var;
    cond->MR_cond_require_path = require_path;

    MR_spy_points[break_num]->MR_spy_cond = cond;

    MR_print_spy_point(MR_mdb_out, break_num, MR_TRUE);
    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_ignore(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_Unsigned         n;
    MR_SpyIgnore_When   ignore_when;
    MR_Unsigned         ignore_count;
    const char          *problem;

    ignore_when = MR_SPY_IGNORE_ENTRY;
    ignore_count = 1;
    if (! MR_trace_options_ignore_count(&ignore_when, &ignore_count,
        &words, &word_count))
    {
        // The usage message has already been printed.
        ;
    } else if (word_count == 2 && MR_trace_is_natural_number(words[1], &n)) {
        if (0 <= n && n < MR_spy_point_next &&
            MR_spy_points[n]->MR_spy_exists)
        {
            problem = MR_ignore_spy_point(n, ignore_when, ignore_count);
            MR_maybe_print_spy_point(n, problem);
        } else {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err,
                "mdb: break point #%"
                MR_INTEGER_LENGTH_MODIFIER "u does not exist.\n", n);
        }
    } else if (word_count == 2 && MR_streq(words[1], "*")) {
        int i;
        int count;

        count = 0;
        for (i = 0; i < MR_spy_point_next; i++) {
            if (MR_spy_points[i]->MR_spy_exists) {
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
            && MR_spy_points[MR_most_recent_spy_point]->MR_spy_exists)
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
MR_trace_cmd_break_print(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    int                 break_num;
    int                 i;
    MR_BrowseFormat     format;
    MR_bool             at_start;
    MR_bool             warn;
    MR_SpyPrintList     print_list;
    MR_SpyPrint         sp;
    const char          *problem;
    MR_bool             any_problem;

    if (! MR_trace_options_break_print(&break_num, &format, &at_start, &warn,
        &words, &word_count))
    {
        // The usage message has already been printed.
        ;
    } else if (word_count > 1) {
        if (break_num < 0) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: no break point exists.\n");
        } else if (! (0 <= break_num && break_num < MR_spy_point_next
            && MR_spy_points[break_num]->MR_spy_exists))
        {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: break point #%d does not exist.\n",
                break_num);
        } else {
            if (word_count == 2 && MR_streq(words[1], "none")) {
                MR_clear_spy_point_print_list(break_num);
                MR_print_spy_point(MR_mdb_out, break_num, MR_TRUE);
            } else {
                print_list = NULL;
                any_problem = MR_FALSE;
                for (i = 1; i < word_count; i++) {
                    problem = MR_parse_spy_print(format, warn, words[i], &sp);
                    if (problem != NULL) {
                        fflush(MR_mdb_out);
                        fprintf(MR_mdb_err, "mdb: cannot parse `%s'\n",
                            words[i]);
                        any_problem = MR_TRUE;
                    } else {
                        print_list =
                            MR_add_to_print_list_end(sp, print_list);
                    }
                }

                if (! any_problem) {
                    if (at_start) {
                        MR_add_spy_point_print_list_start(break_num,
                            print_list);
                    } else {
                        MR_add_spy_point_print_list_end(break_num,
                            print_list);
                    }

                    MR_print_spy_point(MR_mdb_out, break_num, MR_TRUE);
                }
            }
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_enable(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_Unsigned n;

    if (word_count == 2 && MR_trace_is_natural_number(words[1], &n)) {
        if (0 <= n && n < MR_spy_point_next &&
            MR_spy_points[n]->MR_spy_exists)
        {
            MR_spy_points[n]->MR_spy_enabled = MR_TRUE;
            MR_print_spy_point(MR_mdb_out, n, MR_FALSE);
        } else {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err,
                "mdb: break point #%"
                MR_INTEGER_LENGTH_MODIFIER "u does not exist.\n", n);
        }
    } else if (word_count == 2 && MR_streq(words[1], "*")) {
        int i;
        int count;

        count = 0;
        for (i = 0; i < MR_spy_point_next; i++) {
            if (MR_spy_points[i]->MR_spy_exists) {
                MR_spy_points[i]->MR_spy_enabled = MR_TRUE;
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
            && MR_spy_points[MR_most_recent_spy_point]->MR_spy_exists)
        {
            MR_spy_points[MR_most_recent_spy_point]->MR_spy_enabled = MR_TRUE;
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
MR_trace_cmd_disable(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_Unsigned n;

    if (word_count == 2 && MR_trace_is_natural_number(words[1], &n)) {
        if (0 <= n && n < MR_spy_point_next &&
            MR_spy_points[n]->MR_spy_exists)
        {
            MR_spy_points[n]->MR_spy_enabled = MR_FALSE;
            MR_print_spy_point(MR_mdb_out, n, MR_FALSE);
        } else {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err,
                "mdb: break point #%"
                MR_INTEGER_LENGTH_MODIFIER "u does not exist.\n", n);
        }
    } else if (word_count == 2 && MR_streq(words[1], "*")) {
        int i;
        int count;

        count = 0;
        for (i = 0; i < MR_spy_point_next; i++) {
            if (MR_spy_points[i]->MR_spy_exists) {
                MR_spy_points[i]->MR_spy_enabled = MR_FALSE;
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
            && MR_spy_points[MR_most_recent_spy_point]->MR_spy_exists)
        {
            MR_spy_points[MR_most_recent_spy_point]->MR_spy_enabled = MR_FALSE;
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
MR_trace_cmd_delete(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_Unsigned n;

    if (word_count == 2 && MR_trace_is_natural_number(words[1], &n)) {
        if (0 <= n && n < MR_spy_point_next &&
            MR_spy_points[n]->MR_spy_exists)
        {
            MR_spy_points[n]->MR_spy_exists = MR_FALSE;
            MR_print_spy_point(MR_mdb_out, n, MR_FALSE);
            MR_spy_points[n]->MR_spy_exists = MR_TRUE;
            MR_delete_spy_point(n);
        } else {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err,
                "mdb: break point #%"
                MR_INTEGER_LENGTH_MODIFIER "u does not exist.\n", n);
        }
    } else if (word_count == 2 && MR_streq(words[1], "*")) {
        int i;
        int count;

        count = 0;
        for (i = 0; i < MR_spy_point_next; i++) {
            if (MR_spy_points[i]->MR_spy_exists) {
                MR_spy_points[i]->MR_spy_exists = MR_FALSE;
                MR_print_spy_point(MR_mdb_out, i, MR_FALSE);
                MR_spy_points[i]->MR_spy_exists = MR_TRUE;
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
            && MR_spy_points[MR_most_recent_spy_point]->MR_spy_exists)
        {
            int slot;

            slot = MR_most_recent_spy_point;
            MR_spy_points[slot]->MR_spy_exists = MR_FALSE;
            MR_print_spy_point(MR_mdb_out, slot, MR_FALSE);
            MR_spy_points[slot]->MR_spy_exists = MR_TRUE;
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
MR_trace_cmd_register(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_bool verbose;

    verbose = MR_TRUE;

    if (! MR_trace_options_register(&verbose, &words, &word_count)) {
        // The usage message has already been printed.
        ;
    } else if (word_count == 1) {
        MR_register_all_modules_and_procs(MR_mdb_out, verbose);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_modules(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
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
MR_trace_cmd_procedures(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 2) {
        MR_register_all_modules_and_procs(MR_mdb_out, MR_TRUE);
        MR_dump_module_procs(MR_mdb_out, words[1]);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

////////////////////////////////////////////////////////////////////////////

static MR_bool
MR_matches_port_name(const char *word)
{
    int i;

    for (i = 0; i < MR_PORT_NUM_PORTS; i++) {
        if (MR_streq(word, MR_simplified_port_names[i])) {
            return MR_TRUE;
        }
    }

    return MR_FALSE;
}

static const char *
MR_parse_spy_print(MR_BrowseFormat format, MR_bool warn, char *word,
    MR_SpyPrint *sp_ptr)
{
    MR_SpyPrint sp;
    const char  *problem;

    word = MR_copy_string(word);

    problem = NULL;
    sp = MR_malloc(sizeof(struct MR_SpyPrint_Struct));
    sp->MR_p_format = format;
    sp->MR_p_warn = warn;
    sp->MR_p_word_copy = word;

    if (MR_streq(word, "*")) {
        sp->MR_p_what = MR_SPY_PRINT_ALL;
        // The other fields are initialized to dummies.
        sp->MR_p_var_spec.MR_var_spec_kind = MR_VAR_SPEC_NAME;
        sp->MR_p_var_spec.MR_var_spec_number = -1;
        sp->MR_p_var_spec.MR_var_spec_name = NULL;
        sp->MR_p_path = NULL;
    } else if (MR_streq(word, "goal")) {
        sp->MR_p_what = MR_SPY_PRINT_GOAL;
        // The other fields are initialized to dummies.
        sp->MR_p_var_spec.MR_var_spec_kind = MR_VAR_SPEC_NAME;
        sp->MR_p_var_spec.MR_var_spec_number = -1;
        sp->MR_p_var_spec.MR_var_spec_name = NULL;
        sp->MR_p_path = NULL;
    } else {
        sp->MR_p_what = MR_SPY_PRINT_ONE;
        problem = MR_trace_parse_var_path(word,
            &sp->MR_p_var_spec, &sp->MR_p_path);
    }

    if (problem == NULL) {
        *sp_ptr = sp;
    } else {
        *sp_ptr = NULL;
    }

    return problem;
}

static MR_SpyPrintList
MR_add_to_print_list_end(MR_SpyPrint sp, MR_SpyPrintList print_list)
{
    MR_SpyPrintList     list;
    MR_SpyPrintList     new_list;

    new_list = MR_malloc(sizeof(struct MR_SpyPrintList_Struct));
    new_list->MR_pl_cur = sp;
    new_list->MR_pl_next = NULL;

    list = print_list;
    if (list == NULL) {
        return new_list;
    }

    while (list->MR_pl_next != NULL) {
        list = list->MR_pl_next;
    }

    list->MR_pl_next = new_list;
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

////////////////////////////////////////////////////////////////////////////

const char *const    MR_trace_break_cmd_args[] =
    { "-A", "-E", "-I", "-O", "-P", "-S", "-a", "-e", "-i",
    "--all", "--entry", "--ignore-entry", "--ignore-interface",
    "--interface", "--print", "--select-all", "--select-one",
    "--stop", "here", "info", "user_event", NULL };

const char *const    MR_trace_ignore_cmd_args[] =
    { "-E", "-I", "--ignore-entry", "--ignore-interface", NULL };

////////////////////////////////////////////////////////////////////////////

static struct MR_option MR_trace_when_action_multi_ignore_opts[] =
{
    { "all",                MR_no_argument,         NULL,   'a' },
    { "entry",              MR_no_argument,         NULL,   'e' },
    { "interface",          MR_no_argument,         NULL,   'i' },
    { "ignore",             MR_required_argument,   NULL,   'X' },
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
MR_trace_options_when_action_multi_ignore(MR_SpyWhen *when,
    MR_SpyAction *action, MR_MultiMatch *multi_match,
    MR_SpyIgnore_When *ignore_when, MR_Unsigned *ignore_count,
    MR_SpyPrintList *print_list, char ***words, int *word_count)
{
    int             c;
    MR_SpyPrint     sp;
    MR_bool         warn;
    const char      *problem;

    warn = MR_TRUE;
    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "AE:I:OPSX:aeinp:",
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
                problem = MR_parse_spy_print(MR_BROWSE_FORMAT_FLAT, warn,
                    MR_optarg, &sp);
                if (problem != NULL) {
                    fflush(MR_mdb_out);
                    fprintf(MR_mdb_err, "mdb: cannot parse `%s'\n", MR_optarg);
                    return MR_FALSE;
                }

                *print_list = MR_add_to_print_list_end(sp, *print_list);
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

            case 'X':
                if (! MR_trace_is_natural_number(MR_optarg, ignore_count)) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }
                *ignore_when = MR_SPY_IGNORE_ALL;
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
    { "break-num",          MR_required_argument,   NULL,   'b' },
    { "dont-require-var",   MR_no_argument,         NULL,   'v' },
    { "dont-require-path",  MR_no_argument,         NULL,   'p' },
    { NULL,                 MR_no_argument,         NULL,   0 }
};

static MR_bool
MR_trace_options_condition(int *break_num, MR_bool *require_var,
    MR_bool *require_path, char ***words, int *word_count)
{
    int         c;
    MR_Unsigned n;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "b:vp",
        MR_trace_condition_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'b':
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
                // If a variable is missing, then the path inside
                // is missing as well.

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
    { "ignore",             MR_required_argument,   NULL,   'X' },
    { "ignore-entry",       MR_required_argument,   NULL,   'E' },
    { "ignore-interface",   MR_required_argument,   NULL,   'I' },
    { NULL,                 MR_no_argument,         NULL,   0 }
};

static MR_bool
MR_trace_options_ignore_count(MR_SpyIgnore_When *ignore_when,
    MR_Unsigned *ignore_count, char ***words, int *word_count)
{
    int c;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "E:I:X:",
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

            case 'X':
                if (! MR_trace_is_natural_number(MR_optarg, ignore_count)) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }
                *ignore_when = MR_SPY_IGNORE_ALL;
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
    { "break-num",  MR_required_argument, NULL,   'b' },
    { "end",        MR_no_argument,       NULL,   'e' },
    { "no-warn",    MR_no_argument,       NULL,   'n' },
    { "flat",       MR_no_argument,       NULL,   'f' },
    { "raw-pretty", MR_no_argument,       NULL,   'r' },
    { "verbose",    MR_no_argument,       NULL,   'v' },
    { "pretty",     MR_no_argument,       NULL,   'p' },
    { NULL,         MR_no_argument,       NULL,   0 }
};

static MR_bool
MR_trace_options_break_print(int *break_num, MR_BrowseFormat *format,
    MR_bool *at_start, MR_bool *warn, char ***words, int *word_count)
{
    int         c;
    MR_Unsigned n;

    *break_num = MR_most_recent_spy_point;
    *format = MR_BROWSE_FORMAT_FLAT;
    *at_start = MR_TRUE;
    *warn = MR_TRUE;
    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "b:enfrvp",
        MR_trace_break_print_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'b':
                if (! MR_trace_is_natural_number(MR_optarg, &n)) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }
                *break_num = n;
                break;

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
