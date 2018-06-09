// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2008,2012 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module implements the mdb commands in the "forward" category.
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
#include "mercury_trace_cmd_forward.h"
#include "mercury_trace_cmd_parameter.h"
#include "mercury_trace_util.h"

////////////////////////////////////////////////////////////////////////////

static  MR_bool     MR_trace_options_movement_cmd(MR_TraceCmdInfo *cmd,
                        char ***words, int *word_count);

////////////////////////////////////////////////////////////////////////////

MR_Next
MR_trace_cmd_step(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_Unsigned n;

    cmd->MR_trace_strict = MR_FALSE;
    cmd->MR_trace_print_level_specified = MR_FALSE;
    cmd->MR_trace_print_level = MR_default_print_level;
    MR_init_trace_check_integrity(cmd);
    if (! MR_trace_options_movement_cmd(cmd, &words, &word_count)) {
        // The usage message has already been printed.
        ;
    } else if (word_count == 1) {
        cmd->MR_trace_cmd = MR_CMD_STEP;
        return STOP_INTERACTING;
    } else if (word_count == 2 && MR_trace_is_natural_number(words[1], &n)) {
        cmd->MR_trace_cmd = MR_CMD_GOTO;
        cmd->MR_trace_stop_event = MR_trace_event_number + n;
        return STOP_INTERACTING;
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_goto(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_Unsigned n;
    const char  *generator_name;

    cmd->MR_trace_strict = MR_TRUE;
    cmd->MR_trace_print_level_specified = MR_FALSE;
    cmd->MR_trace_print_level = MR_default_print_level;
    MR_init_trace_check_integrity(cmd);
    if (! MR_trace_options_movement_cmd(cmd, &words, &word_count)) {
        // The usage message has already been printed.
        ;
    } else if (word_count == 2 && MR_trace_is_natural_number(words[1], &n)) {
        generator_name = NULL;
        if (MR_trace_event_number < n
            || !MR_cur_generator_is_named(generator_name))
        {
            cmd->MR_trace_cmd = MR_CMD_GOTO;
            cmd->MR_trace_stop_event = n;
            cmd->MR_trace_stop_generator = generator_name;
            return STOP_INTERACTING;
        } else {
            fflush(MR_mdb_out);
            // XXX This message is misleading.
            fprintf(MR_mdb_err, "The debugger cannot go to a past event.\n");
        }
#ifdef  MR_USE_MINIMAL_MODEL_OWN_STACKS
    } else if (word_count == 3 && MR_trace_is_natural_number(words[1], &n)) {
        generator_name = words[2];
        if (MR_trace_event_number < n
            || !MR_cur_generator_is_named(generator_name))
        {
            cmd->MR_trace_cmd = MR_CMD_GOTO;
            cmd->MR_trace_stop_event = n;
            // We don't ever deallocate the memory allocated here,
            // but this memory leak leaks only negligible amounts of memory.

            cmd->MR_trace_stop_generator = strdup(generator_name);
            return STOP_INTERACTING;
        } else {
            fflush(MR_mdb_out);
            // XXX This message is misleading.
            fprintf(MR_mdb_err, "The debugger cannot go to a past event.\n");
        }
#endif
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_next(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    const MR_ProcLayout     *proc_layout;
    const MR_LabelLayout    *ancestor_layout;
    MR_Unsigned             depth;
    MR_Unsigned             stop_depth;
    MR_Unsigned             n;
    MR_TracePort            port;
    MR_Word                 *base_sp;
    MR_Word                 *base_curfr;
    MR_Unsigned             reused_frames;
    MR_Level                actual_level;
    const char              *problem;       // Not used.

    depth = event_info->MR_call_depth;
    cmd->MR_trace_strict = MR_TRUE;
    cmd->MR_trace_print_level_specified = MR_FALSE;
    cmd->MR_trace_print_level = MR_default_print_level;
    MR_init_trace_check_integrity(cmd);
    if (! MR_trace_options_movement_cmd(cmd, &words, &word_count)) {
        // The usage message has already been printed.
        ;
        return KEEP_INTERACTING;
    } else if (word_count == 2 && MR_trace_is_natural_number(words[1], &n)) {
        stop_depth = depth - n;
    } else if (word_count == 1) {
        n = 0;
        stop_depth = depth;
    } else {
        MR_trace_usage_cur_cmd();
        return KEEP_INTERACTING;
    }

    base_sp = MR_saved_sp(event_info->MR_saved_regs);
    base_curfr = MR_saved_curfr(event_info->MR_saved_regs);
    proc_layout = event_info->MR_event_sll->MR_sll_entry;
    MR_trace_find_reused_frames(proc_layout, base_sp, reused_frames);
    port = event_info->MR_trace_port;

    if (depth == stop_depth &&
        (MR_port_is_final(port) || port == MR_PORT_TAILREC_CALL))
    {
        MR_trace_do_noop();
    } else if (depth - reused_frames <= stop_depth && stop_depth < depth) {
        MR_trace_do_noop_tail_rec();
    } else {
        ancestor_layout = MR_find_nth_ancestor(event_info->MR_event_sll,
            n, &base_sp, &base_curfr, &actual_level, &problem);
        if (ancestor_layout == NULL) {
            fflush(MR_mdb_out);
            if (problem != NULL) {
                fprintf(MR_mdb_err, "mdb: %s\n", problem);
            } else {
                fprintf(MR_mdb_err, "mdb: not that many ancestors.\n");
            }
            return KEEP_INTERACTING;
        } else if (actual_level != n) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err,
                "mdb: that stack frame has been reused, "
                "will stop in reusing call.\n");
        } else {
            cmd->MR_trace_cmd = MR_CMD_NEXT;
            cmd->MR_trace_stop_depth = stop_depth;
            return STOP_INTERACTING;
        }
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_finish(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    const MR_ProcLayout     *proc_layout;
    const MR_LabelLayout    *ancestor_layout;
    MR_Unsigned             depth;
    MR_Unsigned             stop_depth;
    MR_Unsigned             n;
    MR_Level                ancestor_level;
    MR_TracePort            port;
    MR_Word                 *base_sp;
    MR_Word                 *base_curfr;
    MR_Unsigned             reused_frames;
    MR_Level                actual_level;
    const char              *problem;       // Not used.

    depth = event_info->MR_call_depth;
    cmd->MR_trace_strict = MR_TRUE;
    cmd->MR_trace_print_level_specified = MR_FALSE;
    cmd->MR_trace_print_level = MR_default_print_level;
    MR_init_trace_check_integrity(cmd);
    if (! MR_trace_options_movement_cmd(cmd, &words, &word_count)) {
        // The usage message has already been printed.
        ;
        return KEEP_INTERACTING;
    } else if (word_count == 2 &&
        ( MR_streq(words[1], "entry") || MR_streq(words[1], "clentry")))
    {
        if (MR_find_clique_entry_mdb(event_info, MR_CLIQUE_ENTRY_FRAME,
            &ancestor_level))
        {
            // The error message has already been printed.
            return KEEP_INTERACTING;
        }
    } else if (word_count == 2 && MR_streq(words[1], "clparent"))
    {
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

    base_sp = MR_saved_sp(event_info->MR_saved_regs);
    base_curfr = MR_saved_curfr(event_info->MR_saved_regs);
    proc_layout = event_info->MR_event_sll->MR_sll_entry;
    MR_trace_find_reused_frames(proc_layout, base_sp, reused_frames);
    port = event_info->MR_trace_port;

    stop_depth = depth - ancestor_level;
    if (MR_port_is_final(port) && depth == stop_depth) {
        MR_trace_do_noop();
    } else if (MR_port_is_final(port) &&
        depth - reused_frames <= stop_depth && stop_depth < depth)
    {
        MR_trace_do_noop_tail_rec();
    } else {
        ancestor_layout = MR_find_nth_ancestor(event_info->MR_event_sll,
            ancestor_level, &base_sp, &base_curfr, &actual_level, &problem);
        if (ancestor_layout == NULL) {
            fflush(MR_mdb_out);
            if (problem != NULL) {
                fprintf(MR_mdb_err, "mdb: %s\n", problem);
            } else {
                fprintf(MR_mdb_err, "mdb: not that many ancestors.\n");
            }
            return KEEP_INTERACTING;
        } else if (actual_level != ancestor_level) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err, "%d %d\n",
                (int) ancestor_level, (int) actual_level);
            fprintf(MR_mdb_err,
                "mdb: that stack frame has been reused, "
                "will stop at finish of reusing call.\n");
        } else {
            cmd->MR_trace_cmd = MR_CMD_FINISH;
            cmd->MR_trace_stop_depth = stop_depth;
            return STOP_INTERACTING;
        }
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_fail(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_Determinism      detism;
    MR_Unsigned         depth;
    MR_Unsigned         stop_depth;
    MR_Unsigned         n;

    detism = event_info->MR_event_sll->MR_sll_entry->MR_sle_detism;
    depth = event_info->MR_call_depth;

    cmd->MR_trace_strict = MR_TRUE;
    cmd->MR_trace_print_level_specified = MR_FALSE;
    cmd->MR_trace_print_level = MR_default_print_level;
    MR_init_trace_check_integrity(cmd);
    if (! MR_trace_options_movement_cmd(cmd, &words, &word_count)) {
        // The usage message has already been printed.
        return KEEP_INTERACTING;
    } else if (word_count == 2 && MR_trace_is_natural_number(words[1], &n)) {
        stop_depth = depth - n;
    } else if (word_count == 1) {
        stop_depth = depth;
    } else {
        MR_trace_usage_cur_cmd();
        return KEEP_INTERACTING;
    }

    if (MR_DETISM_DET_STACK(detism)) {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "mdb: cannot continue until failure: "
            "selected procedure has determinism %s.\n",
            MR_detism_names[detism]);
        return KEEP_INTERACTING;
    }

    // A procedure that lives on the nondet stack cannot have its stack frame
    // reused by tail recursive calls (at least not when any kind of debugging
    // is enabled).

    if (depth == stop_depth && event_info->MR_trace_port == MR_PORT_FAIL) {
        MR_trace_do_noop();
    } else if (depth == stop_depth &&
        event_info->MR_trace_port == MR_PORT_EXCEPTION)
    {
        fflush(MR_mdb_out);
        fprintf(MR_mdb_err, "mdb: cannot continue until failure: "
            "the call has raised an exception.\n");
    } else {
        cmd->MR_trace_cmd = MR_CMD_FAIL;
        cmd->MR_trace_stop_depth = stop_depth;
        return STOP_INTERACTING;
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_exception(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    cmd->MR_trace_strict = MR_TRUE;
    cmd->MR_trace_print_level_specified = MR_FALSE;
    cmd->MR_trace_print_level = MR_default_print_level;
    MR_init_trace_check_integrity(cmd);
    if (! MR_trace_options_movement_cmd(cmd, &words, &word_count)) {
        // The usage message has already been printed.
        ;
    } else if (word_count == 1) {
        if (event_info->MR_trace_port != MR_PORT_EXCEPTION) {
            cmd->MR_trace_cmd = MR_CMD_EXCP;
            return STOP_INTERACTING;
        } else {
            MR_trace_do_noop();
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_return(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    cmd->MR_trace_strict = MR_TRUE;
    cmd->MR_trace_print_level_specified = MR_FALSE;
    cmd->MR_trace_print_level = MR_default_print_level;
    MR_init_trace_check_integrity(cmd);
    if (! MR_trace_options_movement_cmd(cmd, &words, &word_count)) {
        // The usage message has already been printed.
        ;
    } else if (word_count == 1) {
        if (event_info->MR_trace_port == MR_PORT_EXIT) {
            cmd->MR_trace_cmd = MR_CMD_RETURN;
            return STOP_INTERACTING;
        } else {
            MR_trace_do_noop();
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_user(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    cmd->MR_trace_strict = MR_TRUE;
    cmd->MR_trace_print_level_specified = MR_FALSE;
    cmd->MR_trace_print_level = MR_default_print_level;
    MR_init_trace_check_integrity(cmd);
    if (! MR_trace_options_movement_cmd(cmd, &words, &word_count)) {
        // The usage message has already been printed.
        ;
    } else if (word_count == 1) {
        cmd->MR_trace_cmd = MR_CMD_USER;
        return STOP_INTERACTING;
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_forward(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    cmd->MR_trace_strict = MR_TRUE;
    cmd->MR_trace_print_level_specified = MR_FALSE;
    cmd->MR_trace_print_level = MR_default_print_level;
    MR_init_trace_check_integrity(cmd);
    if (! MR_trace_options_movement_cmd(cmd, &words, &word_count)) {
        // The usage message has already been printed.
        ;
    } else if (word_count == 1) {
        MR_TracePort    port;

        port = event_info->MR_trace_port;
        if (port == MR_PORT_FAIL ||
            port == MR_PORT_REDO ||
            port == MR_PORT_EXCEPTION)
        {
            cmd->MR_trace_cmd = MR_CMD_RESUME_FORWARD;
            return STOP_INTERACTING;
        } else {
            MR_trace_do_noop();
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_mindepth(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_Unsigned newdepth;

    cmd->MR_trace_strict = MR_TRUE;
    cmd->MR_trace_print_level_specified = MR_FALSE;
    cmd->MR_trace_print_level = MR_default_print_level;
    MR_init_trace_check_integrity(cmd);
    if (! MR_trace_options_movement_cmd(cmd, &words, &word_count)) {
        // The usage message has already been printed.
        ;
    } else if (word_count == 2 &&
        MR_trace_is_natural_number(words[1], &newdepth))
    {
        cmd->MR_trace_cmd = MR_CMD_MIN_DEPTH;
        cmd->MR_trace_stop_depth = newdepth;
        return STOP_INTERACTING;
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_maxdepth(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_Unsigned newdepth;

    cmd->MR_trace_strict = MR_TRUE;
    cmd->MR_trace_print_level_specified = MR_FALSE;
    cmd->MR_trace_print_level = MR_default_print_level;
    MR_init_trace_check_integrity(cmd);
    if (! MR_trace_options_movement_cmd(cmd, &words, &word_count)) {
        // The usage message has already been printed.
        ;
    } else if (word_count == 2 &&
        MR_trace_is_natural_number(words[1], &newdepth))
    {
        cmd->MR_trace_cmd = MR_CMD_MAX_DEPTH;
        cmd->MR_trace_stop_depth = newdepth;
        return STOP_INTERACTING;
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_continue(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    cmd->MR_trace_strict = MR_FALSE;
    cmd->MR_trace_print_level_specified = MR_FALSE;
    cmd->MR_trace_print_level = MR_default_print_level;
    MR_init_trace_check_integrity(cmd);
    if (! MR_trace_options_movement_cmd(cmd, &words, &word_count)) {
        // The usage message has already been printed.
        ;
    } else if (word_count == 1) {
        cmd->MR_trace_cmd = MR_CMD_TO_END;
        if (! cmd->MR_trace_print_level_specified) {
            // The user did not specify the print level;
            // select the intelligent default.

            if (cmd->MR_trace_strict) {
                cmd->MR_trace_print_level = MR_PRINT_LEVEL_NONE;
            } else {
                cmd->MR_trace_print_level = MR_PRINT_LEVEL_SOME;
            }
        }
        return STOP_INTERACTING;
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

////////////////////////////////////////////////////////////////////////////

const char *const    MR_trace_movement_cmd_args[] =
    { "-N", "-S", "-a", "-i", "-n", "-s",
    "--none", "--some", "--all", "--integrity",
    "--strict", "--no-strict", NULL };

////////////////////////////////////////////////////////////////////////////

static struct MR_option MR_trace_movement_cmd_opts[] =
{
    { "all",        MR_no_argument, NULL,   'a' },
    { "none",       MR_no_argument, NULL,   'n' },
    { "some",       MR_no_argument, NULL,   's' },
    { "nostrict",   MR_no_argument, NULL,   'N' },
    { "strict",     MR_no_argument, NULL,   'S' },
#ifdef  MR_TRACE_CHECK_INTEGRITY
    { "integrity",  MR_no_argument, NULL,   'i' },
#endif
    { NULL,         MR_no_argument, NULL,   0 }
};

static MR_bool
MR_trace_options_movement_cmd(MR_TraceCmdInfo *cmd,
    char ***words, int *word_count)
{
    int c;

#ifdef  MR_TRACE_CHECK_INTEGRITY
  #define   MR_TRACE_MOVEMENT_OPTS  "NSains"
#else
  #define   MR_TRACE_MOVEMENT_OPTS  "NSans"
#endif

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, MR_TRACE_MOVEMENT_OPTS,
        MR_trace_movement_cmd_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'N':
                cmd->MR_trace_strict = MR_FALSE;
                break;

            case 'S':
                cmd->MR_trace_strict = MR_TRUE;
                break;

            case 'a':
                cmd->MR_trace_print_level_specified = MR_TRUE;
                cmd->MR_trace_print_level = MR_PRINT_LEVEL_ALL;
                break;

            case 'n':
                cmd->MR_trace_print_level_specified = MR_TRUE;
                cmd->MR_trace_print_level = MR_PRINT_LEVEL_NONE;
                break;

            case 's':
                cmd->MR_trace_print_level_specified = MR_TRUE;
                cmd->MR_trace_print_level = MR_PRINT_LEVEL_SOME;
                break;

#ifdef  MR_TRACE_CHECK_INTEGRITY
            case 'i':
                cmd->MR_trace_check_integrity = MR_TRUE;
                break;
#endif

            default:
                MR_trace_usage_cur_cmd();
                return MR_FALSE;
        }
    }

    *words = *words + MR_optind - 1;
    *word_count = *word_count - MR_optind + 1;
    return MR_TRUE;
}
