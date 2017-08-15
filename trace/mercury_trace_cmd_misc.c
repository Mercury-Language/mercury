// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2007 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

// This module implements the mdb commands in the "misc" category.
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
#include "mercury_runtime_util.h"

#include "mercury_trace_internal.h"
#include "mercury_trace_cmds.h"
#include "mercury_trace_cmd_misc.h"
#include "mercury_trace_cmd_parameter.h"
#include "mercury_trace_alias.h"
#include "mercury_trace_declarative.h"
#include "mercury_trace_spy.h"

#include "mdb.listing.mh"

////////////////////////////////////////////////////////////////////////////

static  MR_bool     MR_trace_options_ignore(MR_bool *ignore_errors,
                        char ***words, int *word_count);
static  MR_bool     MR_trace_options_confirmed(MR_bool *confirmed,
                        char ***words, int *word_count);

////////////////////////////////////////////////////////////////////////////

MR_Next
MR_trace_cmd_source(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_bool ignore_errors;
    char    **args;

    ignore_errors = MR_FALSE;
    if (! MR_trace_options_ignore(&ignore_errors, &words, &word_count)) {
        // The usage message has already been printed.
        ;
    } else if (word_count >= 2) {
        // If the source fails, the error message
        // will have already been printed by MR_trace_source
        // (unless ignore_errors suppresses the message).

        if (word_count == 2) {
            args = NULL;
        } else {
            args = &words[2];
        }
        (void) MR_trace_source(words[1], ignore_errors, args,
            word_count - 2);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_save(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 2) {
        FILE    *fp;
        MR_bool found_error;
        MR_Word path_list;
        char    errbuf[MR_STRERROR_BUF_SIZE];

        fp = fopen(words[1], "w");
        if (fp == NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: error opening `%s': %s.\n",
                words[1], MR_strerror(errno, errbuf, sizeof(errbuf)));
            return KEEP_INTERACTING;
        }

        MR_trace_print_all_aliases(fp, MR_TRUE);
        switch (MR_default_print_level) {
            case MR_PRINT_LEVEL_NONE:
                fprintf(fp, "printlevel none\n");
                break;

            case MR_PRINT_LEVEL_SOME:
                fprintf(fp, "printlevel some\n");
                break;

            case MR_PRINT_LEVEL_ALL:
                fprintf(fp, "printlevel all\n");
                break;
        }

        if (MR_echo_commands) {
            fprintf(fp, "echo on\n");
        } else {
            fprintf(fp, "echo off\n");
        }

        if (MR_scroll_control) {
            fprintf(fp, "scroll on\n");
        } else {
            fprintf(fp, "scroll off\n");
        }

        fprintf(fp, "scroll %" MR_INTEGER_LENGTH_MODIFIER "u\n",
            MR_scroll_limit);
        fprintf(fp, "stack_default_limit %d\n", MR_stack_default_line_limit);

        switch (MR_context_position) {
            case MR_CONTEXT_NOWHERE:
                fprintf(fp, "context nowhere\n");
                break;

            case MR_CONTEXT_AFTER:
                fprintf(fp, "context after\n");
                break;

            case MR_CONTEXT_BEFORE:
                fprintf(fp, "context before\n");
                break;

            case MR_CONTEXT_PREVLINE:
                fprintf(fp, "context prevline\n");
                break;

            case MR_CONTEXT_NEXTLINE:
                fprintf(fp, "context nextline\n");
                break;
        }

        if (MR_print_goal_paths) {
            fprintf(fp, "goal_paths on\n");
        } else {
            fprintf(fp, "goal_paths off\n");
        }

        found_error = MR_save_spy_points(fp, MR_mdb_err);

        switch (MR_default_breakpoint_scope) {
            case MR_SPY_ALL:
                fprintf(fp, "scope all\n");
                break;

            case MR_SPY_INTERFACE:
                fprintf(fp, "scope interface\n");
                break;

            case MR_SPY_ENTRY:
                fprintf(fp, "scope entry\n");
                break;

            case MR_SPY_LINENO:
            case MR_SPY_SPECIFIC:
            case MR_SPY_USER_EVENT:
            case MR_SPY_USER_EVENT_SET:
                MR_fatal_error("save cmd: invalid default scope");
        }

        MR_trace_print_all_browser_params(fp);
        MR_decl_print_all_trusted(fp, MR_TRUE);

        if (MR_dice_fail_trace_counts_file != NULL) {
            fprintf(fp, "fail_trace_counts %s\n",
                MR_dice_fail_trace_counts_file);
        }
        if (MR_dice_pass_trace_counts_file != NULL) {
            fprintf(fp, "pass_trace_counts %s\n",
                MR_dice_pass_trace_counts_file);
        }

        fprintf(fp, "list_context_lines %" MR_INTEGER_LENGTH_MODIFIER "u\n",
            MR_num_context_lines);
        MR_TRACE_CALL_MERCURY(
            path_list = ML_LISTING_get_list_path(MR_listing_path);
            if (! MR_list_is_empty(path_list)) {
                fprintf(fp, "list_path");
                while (! MR_list_is_empty(path_list)) {
                    fprintf(fp, " %s", (const char *) MR_list_head(path_list));
                    path_list = MR_list_tail(path_list);
                }
                fprintf(fp, "\n");
            }
        );

        if (found_error) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: could not save debugger state to %s.\n",
                words[1]);
            (void) fclose(fp);
        } else if (fclose(fp) != 0) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: error closing `%s': %s.\n",
                words[1], MR_strerror(errno, errbuf, sizeof(errbuf)));
        } else {
            fprintf(MR_mdb_out, "Debugger state saved to %s.\n", words[1]);
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_quit(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_bool confirmed;

    confirmed = MR_FALSE;
    if (! MR_trace_options_confirmed(&confirmed, &words, &word_count)) {
        // The usage message has already been printed.
        ;
    } else if (word_count == 1) {
        if (! confirmed) {
            char    *line2;

            line2 = MR_trace_getline("mdb: are you sure you want to quit? ",
                MR_mdb_in, MR_mdb_out);
            if (line2 == NULL) {
                // This means the user input EOF.
                confirmed = MR_TRUE;
            } else {
                int i = 0;
                while (line2[i] != '\0' && MR_isspace(line2[i])) {
                    i++;
                }

                if (line2[i] == 'y' || line2[i] == 'Y') {
                    confirmed = MR_TRUE;
                }

                MR_free(line2);
            }
        }

        if (confirmed) {
            MR_trace_maybe_close_source_window(MR_FALSE);
            exit(EXIT_SUCCESS);
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

////////////////////////////////////////////////////////////////////////////

// It's better to have a single completion where possible,
// so don't include `-i' here.

const char *const    MR_trace_source_cmd_args[] =
    { "--ignore-errors", NULL };

const char *const    MR_trace_quit_cmd_args[] =
    { "-y", NULL };

////////////////////////////////////////////////////////////////////////////

static struct MR_option MR_trace_ignore_opts[] =
{
    { "ignore-errors",  MR_no_argument, NULL,   'i' },
    { NULL,             MR_no_argument, NULL,   0   }
};

static MR_bool
MR_trace_options_ignore(MR_bool *ignore_errors, char ***words, int *word_count)
{
    int c;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "i",
        MR_trace_ignore_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'i':
                *ignore_errors = MR_TRUE;
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
MR_trace_options_confirmed(MR_bool *confirmed, char ***words, int *word_count)
{
    int c;

    MR_optind = 0;
    while ((c = MR_getopt(*word_count, *words, "NYny")) != EOF) {
        switch (c) {

            case 'n':
            case 'N':
                *confirmed = MR_FALSE;
                break;

            case 'y':
            case 'Y':
                *confirmed = MR_TRUE;
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

////////////////////////////////////////////////////////////////////////////
