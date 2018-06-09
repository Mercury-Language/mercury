// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2007 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module implements the mdb commands in the "exp" category.
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
#include "mercury_trace_cmd_exp.h"
#include "mercury_trace_cmd_parameter.h"
#include "mercury_trace_util.h"

#include "mdbcomp.slice_and_dice.mh"

#include <stdio.h>

////////////////////////////////////////////////////////////////////////////

// The default number of lines to display for a dice.

#define MR_DEFAULT_DICE_LINES   50

static  void        MR_trace_print_dice(char *pass_trace_counts_file,
                        char *fail_trace_counts_file, char *sort_str,
                        int number_of_lines, char *out_str, char *module);

static  MR_bool     MR_trace_options_dice(char **pass_trace_counts_file,
                        char **fail_trace_count_file, char **sort_str,
                        MR_Unsigned *number_of_lines, char **out_file,
                        char **module, char ***words, int *word_count);

////////////////////////////////////////////////////////////////////////////

MR_Next
MR_trace_cmd_histogram_all(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
#ifdef  MR_TRACE_HISTOGRAM

    if (word_count == 2) {
        FILE    *fp;

        fp = fopen(words[1], "w");
        if (fp == NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: cannot open file `%s' for output: %s.\n",
                words[1], MR_strerror(errno, errbuf, sizeof(errbuf)));
        } else {
            MR_trace_print_histogram(fp, "All-inclusive",
                MR_trace_histogram_all,
                MR_trace_histogram_hwm);
            if (fclose(fp) != 0) {
                fflush(MR_mdb_out);
                fprintf(MR_mdb_err, "mdb: error closing file `%s': %s.\n",
                    words[1], MR_strerror(errno, errbuf, sizeof(errbuf)));
            }
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

#else   // MR_TRACE_HISTOGRAM

    fprintf(MR_mdb_out, "mdb: the `histogram_all' command is available "
        "only when histogram gathering is enabled.\n");

#endif  // MR_TRACE_HISTOGRAM

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_histogram_exp(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
#ifdef  MR_TRACE_HISTOGRAM

    if (word_count == 2) {
        FILE    *fp;

        fp = fopen(words[1], "w");
        if (fp == NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: cannot open file `%s' for output: %s.\n",
                words[1], MR_strerror(errno, errbuf, sizeof(errbuf)));
        } else {
            MR_trace_print_histogram(fp, "Experimental",
                MR_trace_histogram_exp,
                MR_trace_histogram_hwm);
            if (fclose(fp) != 0) {
                fflush(MR_mdb_out);
                fprintf(MR_mdb_err, "mdb: error closing file `%s': %s.\n",
                    words[1], MR_strerror(errno, errbuf, sizeof(errbuf)));
            }
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

#else   // MR_TRACE_HISTOGRAM

    fprintf(MR_mdb_out, "mdb: the `histogram_exp' command is available "
        "only when histogram gathering is enabled.\n");

#endif  // MR_TRACE_HISTOGRAM

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_clear_histogram(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
#ifdef  MR_TRACE_HISTOGRAM

    if (word_count == 1) {
        int i;

        for (i = 0; i <= MR_trace_histogram_hwm; i++) {
            MR_trace_histogram_exp[i] = 0;
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

#else   // MR_TRACE_HISTOGRAM

    fprintf(MR_mdb_out, "mdb: the `clear_histogram' command is available "
        "only when histogram gathering is enabled.\n");

#endif  // MR_TRACE_HISTOGRAM

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_dice(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    char        *pass_trace_counts_file;
    char        *fail_trace_counts_file;
    char        *sort_str;
    char        *out_file;
    char        *module;
    MR_Unsigned number_of_lines;

    sort_str = NULL;
    out_file = NULL;
    module = NULL;
    number_of_lines = MR_DEFAULT_DICE_LINES;

    pass_trace_counts_file = MR_dice_pass_trace_counts_file;
    fail_trace_counts_file = MR_dice_fail_trace_counts_file;

    if (! MR_trace_options_dice(&pass_trace_counts_file,
        &fail_trace_counts_file, &sort_str, &number_of_lines, &out_file,
        &module, &words, &word_count))
    {
        // The usage message has already been printed.
        ;
    } else if (word_count == 1) {
        if (pass_trace_counts_file == NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: No passing trace counts file specified."
                "\nmdb: Specify one with the -p option or using the `set' "
                "command.\n");
        } else if (fail_trace_counts_file == NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "mdb: No failing trace counts file specified."
                "\nmdb: Specify one with the -f option or using the `set' "
                "command.\n");
        } else {
            if (sort_str == NULL) {
                sort_str = MR_copy_string("");
            }

            if (module == NULL) {
                module = MR_copy_string("");
            }
            MR_trace_print_dice(pass_trace_counts_file, fail_trace_counts_file,
                sort_str, number_of_lines, out_file, module);
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    if (out_file != NULL) {
        free(out_file);
    }

    if (sort_str != NULL) {
        free(sort_str);
    }

    if (module != NULL) {
        free(module);
    }

    return KEEP_INTERACTING;
}

////////////////////////////////////////////////////////////////////////////

static  void
MR_trace_print_dice(char *pass_trace_counts_file,
    char *fail_trace_count_file, char *sort_str, int number_of_lines,
    char *out_file, char *module)
{
    MR_String   dice;
    MR_String   problem;
    MR_String   aligned_pass_trace_counts_file;
    MR_String   aligned_fail_trace_count_file;
    MR_String   aligned_sort_str;
    MR_String   aligned_module;
    FILE        *fp;
    char        errbuf[MR_STRERROR_BUF_SIZE];

    MR_TRACE_USE_HP(
        MR_make_aligned_string(aligned_pass_trace_counts_file,
            (MR_String) pass_trace_counts_file);
        MR_make_aligned_string(aligned_fail_trace_count_file,
            (MR_String) fail_trace_count_file);
        MR_make_aligned_string(aligned_sort_str, (MR_String) sort_str);
        if (module == NULL) {
            MR_make_aligned_string(aligned_module, (MR_String) "");
        } else {
            MR_make_aligned_string(aligned_module, (MR_String) module);
        }
    );

    MR_TRACE_CALL_MERCURY(
        MR_MDBCOMP_read_dice_to_string(aligned_pass_trace_counts_file,
            aligned_fail_trace_count_file, aligned_sort_str,
            number_of_lines, aligned_module, &dice, &problem);
    );

    // The string in dice is a sequence of complete lines.
    if (MR_streq(problem, "")) {
        if (out_file == NULL) {
            fprintf(MR_mdb_out, "%s", dice);
        } else {
            fp = fopen(out_file, "w");
            if (fp != NULL) {
                fprintf(fp, "%s", dice);
                if (fclose(fp) != 0) {
                    fflush(MR_mdb_out);
                    fprintf(MR_mdb_err, "mdb: Error closing file `%s': %s\n",
                        out_file, MR_strerror(errno, errbuf, sizeof(errbuf)));
                }
            } else {
                fflush(MR_mdb_out);
                fprintf(MR_mdb_err, "mdb: Error opening file `%s': %s\n",
                    out_file, MR_strerror(errno, errbuf, sizeof(errbuf)));
            }
        }
    } else {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "mdb: %s\n", problem);
    }
}

////////////////////////////////////////////////////////////////////////////

static struct MR_option MR_trace_dice_opts[] =
{
    { "pass-trace-counts",      MR_required_argument,   NULL,   'p' },
    { "pass-trace-count",       MR_required_argument,   NULL,   'p' },
    { "fail-trace-counts",      MR_required_argument,   NULL,   'f' },
    { "fail-trace-count",       MR_required_argument,   NULL,   'f' },
    { "sort",                   MR_required_argument,   NULL,   's' },
    { "top",                    MR_required_argument,   NULL,   'n' },
    { "output-to-file",         MR_required_argument,   NULL,   'o' },
    { "module",                 MR_required_argument,   NULL,   'm' },
    { NULL,                     MR_no_argument,         NULL,   0   }
};

static MR_bool
MR_trace_options_dice(char **pass_trace_counts_file,
    char **fail_trace_counts_file, char **sort_str, MR_Unsigned *n,
    char **out_file, char **module, char ***words, int *word_count)
{
    int c;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "p:f:s:n:o:m:",
        MR_trace_dice_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'p':
                // Don't free *pass_trace_counts_file even if non-NULL,
                // since its initial value comes from a global variable,
                // and thus will still be used after the dice command.
                // The waste of not freeing of the string allocated by
                // MR_copy_string if this option is duplicated can be
                // easily lived with.

                *pass_trace_counts_file = MR_copy_string(MR_optarg);
                break;

            case 'f':
                // Don't free *fail_trace_counts_file even if non-NULL,
                // since its initial value comes from a global variable,
                // and thus will still be used after the dice command.
                // The waste of not freeing of the string allocated by
                // MR_copy_string if this option is duplicated can be
                // easily lived with.

                *fail_trace_counts_file = MR_copy_string(MR_optarg);
                break;

            case 's':
                *sort_str = MR_copy_string(MR_optarg);
                break;

            case 'n':
                if (! MR_trace_is_natural_number(MR_optarg, n)) {
                    MR_trace_usage_cur_cmd();
                    return MR_FALSE;
                }
                break;

            case 'o':
                *out_file = MR_copy_string(MR_optarg);
                break;

            case 'm':
                *module = MR_copy_string(MR_optarg);
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
