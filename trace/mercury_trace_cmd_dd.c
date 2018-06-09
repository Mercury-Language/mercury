// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2007 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module implements the mdb commands in the "dd" category.
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
#include "mercury_trace_cmd_dd.h"
#include "mercury_trace_cmd_parameter.h"
#include "mercury_trace_declarative.h"
#include "mercury_trace_tables.h"
#include "mercury_trace_util.h"

////////////////////////////////////////////////////////////////////////////

static  MR_bool     MR_trace_options_dd(MR_bool *assume_all_io_is_tabled,
                        MR_Unsigned *default_depth, MR_Unsigned *num_nodes,
                        MR_DeclSearchMode *search_mode,
                        MR_bool *search_mode_was_set,
                        MR_bool *search_mode_requires_trace_counts,
                        char **pass_trace_counts_file,
                        char **fail_trace_counts_file,
                        MR_bool *new_session, MR_bool *reset_kb,
                        MR_bool *testing, MR_bool *debug,
                        char ***words, int *word_count);

////////////////////////////////////////////////////////////////////////////

MR_Next
MR_trace_cmd_dd(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_DeclSearchMode   search_mode;
    MR_bool             search_mode_was_set = MR_FALSE;
    MR_bool             new_session = MR_TRUE;
    MR_bool             reset_kb = MR_FALSE;
    MR_bool             search_mode_requires_trace_counts = MR_FALSE;
    char                *pass_trace_counts_file;
    char                *fail_trace_counts_file;
    MR_String           problem;
    MR_bool             testing = MR_FALSE;
    const char          *filename;
    MR_DeclMode         decl_mode;

    MR_trace_decl_assume_all_io_is_tabled = MR_FALSE;
    MR_edt_default_depth_limit = MR_TRACE_DECL_INITIAL_DEPTH;
    search_mode = MR_trace_get_default_search_mode();
    pass_trace_counts_file = MR_dice_pass_trace_counts_file;
    fail_trace_counts_file = MR_dice_fail_trace_counts_file;
    MR_trace_decl_debug_debugger_mode = MR_FALSE;

    if (! MR_trace_options_dd(&MR_trace_decl_assume_all_io_is_tabled,
        &MR_edt_default_depth_limit, &MR_edt_desired_nodes_in_subtree,
        &search_mode, &search_mode_was_set,
        &search_mode_requires_trace_counts,
        &pass_trace_counts_file, &fail_trace_counts_file, &new_session,
        &reset_kb, &testing, &MR_trace_decl_debug_debugger_mode, &words,
        &word_count))
    {
        // The usage message has already been printed.
        ;
    } else if (word_count <= 2) {
        if (word_count == 2 && MR_trace_decl_debug_debugger_mode) {
            decl_mode = MR_DECL_DUMP;
            filename = (const char *) words[1];
        } else {
            decl_mode = MR_DECL_NODUMP;
            filename = (const char *) NULL;
        }

        if (MR_trace_have_unhid_events) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err,
                "mdb: dd doesn't work after `unhide_events on'.\n");
            return KEEP_INTERACTING;
        }

        if (search_mode_requires_trace_counts && (
            pass_trace_counts_file == NULL || fail_trace_counts_file == NULL))
        {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err,
                "mdb: you need to supply passing and failing trace count "
                "files\nbefore using the specified search mode.\n");
            return KEEP_INTERACTING;
        }

        if (pass_trace_counts_file != NULL && fail_trace_counts_file != NULL) {
            if (! MR_trace_decl_init_suspicion_table(pass_trace_counts_file,
                fail_trace_counts_file, &problem))
            {
                fflush(MR_mdb_out);
                fprintf(MR_mdb_err, "mdb: %s\n", problem);
                return KEEP_INTERACTING;
            }
        }

        MR_trace_decl_set_testing_flag(testing);

        if (new_session) {
            MR_trace_decl_session_init();
        }

        if (search_mode_was_set || new_session) {
            MR_trace_decl_set_fallback_search_mode(search_mode);
        }

        if (reset_kb) {
            MR_trace_decl_reset_knowledge_base();
        }

        if (MR_trace_start_decl_debug(decl_mode, filename, new_session, cmd,
            event_info, jumpaddr))
        {
            return STOP_INTERACTING;
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_trust(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_ProcSpec         spec;
    MR_MatchesInfo      matches;

    if (word_count == 2) {
        spec.MR_proc_module = NULL;
        spec.MR_proc_name   = NULL;
        spec.MR_proc_arity  = -1;
        spec.MR_proc_mode   = -1;
        spec.MR_proc_prefix = (MR_ProcPrefix) -1;

        MR_register_all_modules_and_procs(MR_mdb_out, MR_TRUE);

        // First see if the argument is a module name.
        spec.MR_proc_module = words[1];
        matches = MR_search_for_matching_procedures(&spec);
        if (matches.match_proc_next > 0) {
            MR_decl_add_trusted_module(words[1]);
            fprintf(MR_mdb_out, "Trusting module %s\n", words[1]);
        } else if (MR_parse_proc_spec(words[1], &spec)) {
            // Check to see if the argument is a pred/func.
            matches = MR_search_for_matching_procedures(&spec);
            MR_filter_user_preds(&matches);
            if (matches.match_proc_next == 0) {
                fprintf(MR_mdb_err,
                    "mdb: there is no such module, predicate or function.\n");
            } else if (matches.match_proc_next == 1) {
                MR_decl_add_trusted_pred_or_func(matches.match_procs[0]);
                fprintf(MR_mdb_out, "Trusting ");
                MR_print_pred_id_and_nl(MR_mdb_out, matches.match_procs[0]);
            } else {
                MR_Unsigned i;
                char        buf[80];
                char        *line2;

                fprintf(MR_mdb_out, "Ambiguous predicate or function"
                    " specification. The matches are:\n");
                for (i = 0; i < matches.match_proc_next; i++) {
                    fprintf(MR_mdb_out, "%" MR_INTEGER_LENGTH_MODIFIER "u: ",
                        i);
                    MR_print_pred_id_and_nl(MR_mdb_out,
                        matches.match_procs[i]);
                }
                sprintf(buf, "\nWhich predicate or function "
                    "do you want to trust (0-%" MR_INTEGER_LENGTH_MODIFIER
                    "u or *)? ",
                    matches.match_proc_next - 1);
                line2 = MR_trace_getline(buf, MR_mdb_in, MR_mdb_out);
                if (line2 == NULL) {
                    // This means the user input EOF.
                    fprintf(MR_mdb_out, "none of them\n");
                } else if (MR_streq(line2, "*")) {
                    for (i = 0; i < matches.match_proc_next; i++) {
                        MR_decl_add_trusted_pred_or_func(
                            matches.match_procs[i]);

                        fprintf(MR_mdb_out, "Trusting ");
                        MR_print_pred_id_and_nl(MR_mdb_out,
                            matches.match_procs[i]);
                    }
                    MR_free(line2);
                } else if(MR_trace_is_natural_number(line2, &i)) {
                    if (0 <= i && i < matches.match_proc_next) {
                        MR_decl_add_trusted_pred_or_func(
                            matches.match_procs[i]);

                        fprintf(MR_mdb_out, "Trusting ");
                        MR_print_pred_id_and_nl(MR_mdb_out,
                            matches.match_procs[i]);
                    } else {
                        fprintf(MR_mdb_out, "no such match\n");
                    }
                    MR_free(line2);
                } else {
                    fprintf(MR_mdb_out, "none of them\n");
                    MR_free(line2);
                }
            }
        }
    } else if (word_count == 3 &&
        ((MR_streq(words[1], "std") && MR_streq(words[2], "lib"))
        || (MR_streq(words[1], "standard") && MR_streq(words[2], "library"))))
    {
        MR_decl_trust_standard_library();
        fprintf(MR_mdb_out, "Trusting the Mercury standard library\n");
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_untrust(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_Unsigned i;

    if (word_count == 2 && MR_trace_is_natural_number(words[1], &i)) {
        if (!MR_decl_remove_trusted(i)) {
            fprintf(MR_mdb_err, "mdb: no such trusted object\n");
        }
    } else {
        MR_trace_usage_cur_cmd();
    }
    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_trusted(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 1) {
        MR_decl_print_all_trusted(MR_mdb_out, MR_FALSE);
    } else {
        MR_trace_usage_cur_cmd();
    }
    return KEEP_INTERACTING;
}

////////////////////////////////////////////////////////////////////////////

const char *const    MR_trace_dd_cmd_args[] =
    { "-a", "-d", "-n", "-r", "-R", "-s",
      "--assume-all-io-is-tabled", "--depth", "--nodes",
      "--reset-knowledge-base", "--resume", "--search-mode",
      "dq" "divide_and_query", "sdq", "suspicion_divide_and_query",
      "td", "top_down", NULL };

////////////////////////////////////////////////////////////////////////////

static struct MR_option MR_trace_dd_opts[] =
{
    { "assume-all-io-is-tabled",    MR_no_argument,         NULL,   'a' },
    { "debug",                      MR_no_argument,         NULL,   'z' },
    { "depth",                      MR_required_argument,   NULL,   'd' },
    { "fail-trace-counts",          MR_required_argument,   NULL,   'f' },
    { "fail-trace-count",           MR_required_argument,   NULL,   'f' },
    { "nodes",                      MR_required_argument,   NULL,   'n' },
    { "pass-trace-counts",          MR_required_argument,   NULL,   'p' },
    { "pass-trace-count",           MR_required_argument,   NULL,   'p' },
    { "reset-knowledge-base",       MR_no_argument,         NULL,   'R' },
    { "resume",                     MR_no_argument,         NULL,   'r' },
    { "search-mode",                MR_required_argument,   NULL,   's' },
    { "test",                       MR_no_argument,         NULL,   't' },
    { NULL,                         MR_no_argument,         NULL,   0   }
};

static MR_bool
MR_trace_options_dd(MR_bool *assume_all_io_is_tabled,
    MR_Unsigned *default_depth, MR_Unsigned *num_nodes,
    MR_DeclSearchMode *search_mode, MR_bool *search_mode_was_set,
    MR_bool *search_mode_requires_trace_counts,
    char **pass_trace_counts_file, char **fail_trace_counts_file,
    MR_bool *new_session, MR_bool *reset_kb, MR_bool *testing,
    MR_bool *debug, char ***words, int *word_count)
{
    int c;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "ad:f:n:p:rRs:tz",
        MR_trace_dd_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'a':
                *assume_all_io_is_tabled = MR_TRUE;
                break;

            case 'd':
                if (! MR_trace_is_natural_number(MR_optarg, default_depth)) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }
                break;

            case 'f':
                *fail_trace_counts_file = MR_copy_string(MR_optarg);
                break;

            case 'n':
                if (! MR_trace_is_natural_number(MR_optarg, num_nodes)) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }
                break;

            case 'p':
                *pass_trace_counts_file = MR_copy_string(MR_optarg);
                break;

            case 'r':
                *new_session = MR_FALSE;
                break;

            case 'R':
                *reset_kb = MR_TRUE;
                break;

            case 's':
                if (MR_trace_is_valid_search_mode_string(MR_optarg,
                    search_mode, search_mode_requires_trace_counts))
                {
                    *search_mode_was_set = MR_TRUE;
                } else {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }
                break;

            case 't':
                *testing = MR_TRUE;
                break;

            case 'z':
                *debug = MR_TRUE;
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
