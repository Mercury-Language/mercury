// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module implements the mdb commands in the "table_io" category.
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
#include "mercury_trace_cmd_table_io.h"

////////////////////////////////////////////////////////////////////////////

static  void        MR_print_unsigned_var(FILE *fp, const char *var,
                        MR_Unsigned value);

////////////////////////////////////////////////////////////////////////////

MR_Next
MR_trace_cmd_table_io(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 1) {
        if (! MR_io_tabling_allowed) {
            fprintf(MR_mdb_err,
                "This executable wasn't prepared for I/O tabling.\n");
            return KEEP_INTERACTING;
        }

        if (MR_io_tabling_phase == MR_IO_TABLING_BEFORE) {
            fprintf(MR_mdb_out, "I/O tabling has not yet started.\n");
        } else if (MR_io_tabling_phase == MR_IO_TABLING_DURING) {
            fprintf(MR_mdb_out, "I/O tabling has started.\n");
        } else if (MR_io_tabling_phase == MR_IO_TABLING_AFTER) {
            fprintf(MR_mdb_out, "I/O tabling has stopped.\n");
        } else {
            MR_fatal_error("I/O tabling in impossible phase.\n");
        }
    } else if (word_count == 2 &&
        (MR_streq(words[1], "start") || MR_streq(words[1], "begin")))
    {
        if (! MR_io_tabling_allowed) {
            fprintf(MR_mdb_err,
                "This executable wasn't prepared for I/O tabling.\n");
            return KEEP_INTERACTING;
        }

        if (MR_io_tabling_phase == MR_IO_TABLING_BEFORE) {
            MR_io_tabling_phase = MR_IO_TABLING_DURING;
            MR_io_tabling_start = MR_io_tabling_counter;
            MR_io_tabling_end = MR_IO_ACTION_MAX;
            MR_io_tabling_start_event_num = event_info->MR_event_number;
#ifdef  MR_DEBUG_RETRY
            MR_io_tabling_debug = MR_TRUE;
#endif
            fprintf(MR_mdb_out, "I/O tabling started.\n");
        } else if (MR_io_tabling_phase == MR_IO_TABLING_DURING) {
            fprintf(MR_mdb_out, "I/O tabling has already started.\n");
        } else if (MR_io_tabling_phase == MR_IO_TABLING_AFTER) {
            fprintf(MR_mdb_out, "I/O tabling has already stopped.\n");
        } else {
            MR_fatal_error("I/O tabling in impossible phase.\n");
        }
    } else if (word_count == 2 &&
        (MR_streq(words[1], "stop") || MR_streq(words[1], "end")))
    {
        if (! MR_io_tabling_allowed) {
            fprintf(MR_mdb_err,
                "This executable wasn't prepared for I/O tabling.\n");
            return KEEP_INTERACTING;
        }

        if (MR_io_tabling_phase == MR_IO_TABLING_BEFORE) {
            fprintf(MR_mdb_out, "I/O tabling has not yet started.\n");
        } else if (MR_io_tabling_phase == MR_IO_TABLING_DURING) {
            MR_io_tabling_phase = MR_IO_TABLING_AFTER;
            MR_io_tabling_end = MR_io_tabling_counter_hwm;
            MR_io_tabling_stop_event_num = event_info->MR_event_number;
            fprintf(MR_mdb_out, "I/O tabling stopped.\n");
        } else if (MR_io_tabling_phase == MR_IO_TABLING_AFTER) {
            fprintf(MR_mdb_out, "I/O tabling has already stopped.\n");
        } else {
            MR_fatal_error("I/O tabling in impossible phase.\n");
        }
    } else if (word_count == 2 && MR_streq(words[1], "stats")) {
        if (! MR_io_tabling_allowed) {
            fprintf(MR_mdb_err,
                "This executable wasn't prepared for I/O tabling.\n");
            return KEEP_INTERACTING;
        }

        fprintf(MR_mdb_out, "phase = %d\n", MR_io_tabling_phase);
        MR_print_unsigned_var(MR_mdb_out, "counter", MR_io_tabling_counter);
        MR_print_unsigned_var(MR_mdb_out, "hwm", MR_io_tabling_counter_hwm);
        MR_print_unsigned_var(MR_mdb_out, "start", MR_io_tabling_start);
        MR_print_unsigned_var(MR_mdb_out, "end", MR_io_tabling_end);
    } else if (word_count == 2 && MR_streq(words[1], "allow")) {
        // The "table_io allow" command allows the programmer to give
        // the command "table_io start" even in grades in which there
        // is no guarantee that all I/O primitives are tabled. It is
        // for developers only, because if it is used on programs in
        // which some but not all I/O primitives are tabled, the
        // results of turning on I/O tabling can be weird.

        MR_io_tabling_allowed = MR_TRUE;
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

////////////////////////////////////////////////////////////////////////////

static void
MR_print_unsigned_var(FILE *fp, const char *var, MR_Unsigned value)
{
    fprintf(fp, "%s = %" MR_INTEGER_LENGTH_MODIFIER "u\n", var, value);
}

////////////////////////////////////////////////////////////////////////////

// "table_io allow" is deliberately not documented as it is developer only.
// "table_io begin" and "table_io end" are deliberately not documented in an
// effort to encourage consistent use of start/stop.

const char *const    MR_trace_table_io_cmd_args[] =
    { "stats", "start", "stop", NULL };

////////////////////////////////////////////////////////////////////////////
