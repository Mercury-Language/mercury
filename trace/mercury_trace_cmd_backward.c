// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2008,2012 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module implements the mdb commands in the "backward" category.
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

#include "mercury_trace.h"
#include "mercury_trace_internal.h"
#include "mercury_trace_cmds.h"
#include "mercury_trace_cmd_backward.h"
#include "mercury_trace_cmd_parameter.h"
#include "mercury_trace_command_queue.h"
#include "mercury_trace_util.h"

////////////////////////////////////////////////////////////////////////////

// The message to print for retries through un-io-tabled areas, when
// the MR_RETRY_IO_INTERACTIVE option is given.

#define MR_UNTABLED_IO_RETRY_MESSAGE                    \
    "Retry across I/O operations is not always safe.\n" \
    "Are you sure you want to do it? "

static  MR_bool     MR_trace_options_retry(MR_RetryAcrossIo *across_io,
                        MR_bool *assume_all_io_is_tabled,
                        char ***words, int *word_count);

////////////////////////////////////////////////////////////////////////////

MR_Next
MR_trace_cmd_retry(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_Level            n;
    MR_Level            ancestor_level;
    MR_RetryAcrossIo    across_io;
    const char          *problem;
    MR_RetryResult      result;
    MR_bool             assume_all_io_is_tabled;
    MR_bool             unsafe_retry;

    ancestor_level = 0;
    across_io = MR_RETRY_IO_INTERACTIVE;
    assume_all_io_is_tabled = MR_FALSE;
    if (! MR_trace_options_retry(&across_io, &assume_all_io_is_tabled,
        &words, &word_count))
    {
        // The usage message has already been printed.
        ;
    } else if (word_count == 2 &&
        ( MR_streq(words[1], "clique") || MR_streq(words[1], "clentry")))
    {
        if (MR_find_clique_entry_mdb(event_info, MR_CLIQUE_ENTRY_FRAME,
            &ancestor_level))
        {
            // The error message has already been printed.
            return KEEP_INTERACTING;
        }
    } else if (word_count == 2 && MR_streq(words[1], "clparent")) {
        if (MR_find_clique_entry_mdb(event_info, MR_CLIQUE_ENTRY_PARENT_FRAME,
            &ancestor_level))
        {
            // The error message has already been printed.
            return KEEP_INTERACTING;
        }
    } else if (word_count == 2 && MR_trace_is_natural_number(words[1], &n)) {
        ancestor_level = n;
    } else if (word_count == 1) {
        ancestor_level = 0;
    } else {
        MR_trace_usage_cur_cmd();
        return KEEP_INTERACTING;
    }

    if (ancestor_level == 0 && MR_port_is_entry(event_info->MR_trace_port)) {
        MR_trace_do_noop();
        return KEEP_INTERACTING;
    }

    result = MR_trace_retry(event_info, ancestor_level,
        across_io, assume_all_io_is_tabled, MR_UNTABLED_IO_RETRY_MESSAGE,
        &unsafe_retry, &problem, MR_mdb_in, MR_mdb_out, jumpaddr);
    switch (result) {

    case MR_RETRY_OK_DIRECT:
        cmd->MR_trace_cmd = MR_CMD_GOTO;
        cmd->MR_trace_stop_event = MR_trace_event_number + 1;
        cmd->MR_trace_strict = MR_FALSE;
        cmd->MR_trace_print_level = MR_default_print_level;
        return STOP_INTERACTING;

    case MR_RETRY_OK_FINISH_FIRST:
        cmd->MR_trace_cmd = MR_CMD_FINISH;
        cmd->MR_trace_stop_depth = event_info->MR_call_depth - ancestor_level;
        cmd->MR_trace_strict = MR_TRUE;
        cmd->MR_trace_print_level = MR_PRINT_LEVEL_NONE;

        // Arrange to retry the call once it is finished.
        // XXX We should use the same options as the original retry.
        MR_insert_command_line_at_head("retry -o");
        return STOP_INTERACTING;

    case MR_RETRY_OK_FAIL_FIRST:
        cmd->MR_trace_cmd = MR_CMD_FAIL;
        cmd->MR_trace_stop_depth = event_info->MR_call_depth - ancestor_level;
        cmd->MR_trace_strict = MR_TRUE;
        cmd->MR_trace_print_level = MR_PRINT_LEVEL_NONE;

        // Arrange to retry the call once it is finished.
        // XXX We should use the same options as the original retry.
        MR_insert_command_line_at_head("retry -o");
        return STOP_INTERACTING;

    case MR_RETRY_ERROR:
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "%s\n", problem);
        return KEEP_INTERACTING;
    }

    MR_fatal_error("unrecognized retry result");
}

////////////////////////////////////////////////////////////////////////////

// "retry --assume-all-io-is-tabled" is deliberately not documented as
// it is for developers only.

const char *const    MR_trace_retry_cmd_args[] =
    { "--force", "--interactive", "--only-if-safe", NULL };

////////////////////////////////////////////////////////////////////////////

static struct MR_option MR_trace_retry_opts[] =
{
    { "assume-all-io-is-tabled",    MR_no_argument, NULL,   'a' },
    { "force",                      MR_no_argument, NULL,   'f' },
    { "interactive",                MR_no_argument, NULL,   'i' },
    { "only-if-safe",               MR_no_argument, NULL,   'o' },
    { NULL,                         MR_no_argument, NULL,   0 }
};

static MR_bool
MR_trace_options_retry(MR_RetryAcrossIo *across_io,
    MR_bool *assume_all_io_is_tabled, char ***words, int *word_count)
{
    int c;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "afio",
        MR_trace_retry_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'a':
                *assume_all_io_is_tabled = MR_TRUE;
                break;

            case 'f':
                *across_io = MR_RETRY_IO_FORCE;
                break;

            case 'i':
                *across_io = MR_RETRY_IO_INTERACTIVE;
                break;

            case 'o':
                *across_io = MR_RETRY_IO_ONLY_IF_SAFE;
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
