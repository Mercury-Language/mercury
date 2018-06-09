// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2007 The University of Melbourne.
// Copyright (C) 2017-2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module implements the mdb commands in the "parameter" category.
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
#include "mercury_stack_trace.h"    // for MR_ContextPosition
#include "mercury_string.h"

#include "mercury_trace.h"
#include "mercury_trace_browse.h"
#include "mercury_trace_internal.h"
#include "mercury_trace_cmds.h"
#include "mercury_trace_cmd_parameter.h"
#include "mercury_trace_spy.h"
#include "mercury_trace_alias.h"
#include "mercury_trace_util.h"

#include "mdb.browser_info.mh"      // for ML_BROWSE_get_num_io_actions etc
#include "mdb.listing.mh"           // for ML_LISTING_get_list_path etc

////////////////////////////////////////////////////////////////////////////

char                    *MR_mmc_options = NULL;

MR_TracePrintLevel      MR_default_print_level = MR_PRINT_LEVEL_SOME;

MR_bool                 MR_scroll_control = MR_TRUE;
MR_Unsigned             MR_scroll_limit = 24;
MR_Unsigned             MR_scroll_next = 0;

int                     MR_stack_default_line_limit = 0;

MR_bool                 MR_echo_commands = MR_FALSE;

MR_bool                 MR_print_optionals = MR_FALSE;

char                    *MR_dice_pass_trace_counts_file = NULL;
char                    *MR_dice_fail_trace_counts_file = NULL;

MR_ContextPosition      MR_context_position = MR_CONTEXT_AFTER;

MR_UserEventContext     MR_user_event_context = MR_USER_EVENT_CONTEXT_FULL;

MR_bool                 MR_print_goal_paths = MR_TRUE;

MR_Word                 MR_listing_path;

MR_Unsigned             MR_num_context_lines = 2;

MR_SpyWhen              MR_default_breakpoint_scope = MR_SPY_INTERFACE;

////////////////////////////////////////////////////////////////////////////

static  MR_bool     MR_trace_options_cmd_format(MR_Word *print_set,
                        MR_Word *browse_set, MR_Word *print_all_set,
                        char ***words, int *word_count);
static  MR_bool     MR_trace_options_cmd_format_param(MR_Word *print_set,
                        MR_Word *browse_set, MR_Word *print_all_set,
                        MR_Word *flat_format, MR_Word *raw_pretty_format,
                        MR_Word *verbose_format, MR_Word *pretty_format,
                        char ***words, int *word_count);

////////////////////////////////////////////////////////////////////////////

MR_Next
MR_trace_cmd_mmc_options(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    size_t len;
    size_t i;

    // Allocate the right amount of space.
    len = 0;
    for (i = 1; i < word_count; i++) {
        len += strlen(words[i]) + 1;
    }
    len++;
    MR_mmc_options = MR_realloc(MR_mmc_options, len);

    // Copy the arguments to MR_mmc_options.
    MR_mmc_options[0] = '\0';
    for (i = 1; i < word_count; i++) {
        strcat(MR_mmc_options, words[i]);
        strcat(MR_mmc_options, " ");
    }
    MR_mmc_options[len - 1] = '\0';

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_printlevel(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 2) {
        if (MR_streq(words[1], "none")) {
            MR_default_print_level = MR_PRINT_LEVEL_NONE;
            if (MR_trace_internal_interacting) {
                fprintf(MR_mdb_out, "Default print level set to `none'.\n");
            }
        } else if (MR_streq(words[1], "some")) {
            MR_default_print_level = MR_PRINT_LEVEL_SOME;
            if (MR_trace_internal_interacting) {
                fprintf(MR_mdb_out, "Default print level set to `some'.\n");
            }
        } else if (MR_streq(words[1], "all")) {
            MR_default_print_level = MR_PRINT_LEVEL_ALL;
            if (MR_trace_internal_interacting) {
                fprintf(MR_mdb_out, "Default print level set to `all'.\n");
            }
        } else {
            MR_trace_usage_cur_cmd();
        }
    } else if (word_count == 1) {
        fprintf(MR_mdb_out, "The default print level is ");
        switch (MR_default_print_level) {
            case MR_PRINT_LEVEL_NONE:
                fprintf(MR_mdb_out, "`none'.\n");
                break;
            case MR_PRINT_LEVEL_SOME:
                fprintf(MR_mdb_out, "`some'.\n");
                break;
            case MR_PRINT_LEVEL_ALL:
                fprintf(MR_mdb_out, "`all'.\n");
                break;
            default:
                MR_default_print_level = MR_PRINT_LEVEL_SOME;
                fprintf(MR_mdb_out, "invalid (now set to `some').\n");
                break;
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_scroll(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_Unsigned n;

    if (word_count == 2) {
        if (MR_streq(words[1], "off")) {
            MR_scroll_control = MR_FALSE;
            if (MR_trace_internal_interacting) {
                fprintf(MR_mdb_out, "Scroll control disabled.\n");
            }
        } else if (MR_streq(words[1], "on")) {
            MR_scroll_control = MR_TRUE;
            if (MR_trace_internal_interacting) {
                fprintf(MR_mdb_out, "Scroll control enabled.\n");
            }
        } else if (MR_trace_is_natural_number(words[1], &n)) {
            MR_scroll_limit = n;
            if (MR_trace_internal_interacting) {
                fprintf(MR_mdb_out,
                    "Scroll window size set to %" MR_INTEGER_LENGTH_MODIFIER
                    "u.\n", MR_scroll_limit);
            }
        } else {
            MR_trace_usage_cur_cmd();
        }
    } else if (word_count == 1) {
        fprintf(MR_mdb_out, "Scroll control is ");
        if (MR_scroll_control) {
            fprintf(MR_mdb_out, "on");
        } else {
            fprintf(MR_mdb_out, "off");
        }
        fprintf(MR_mdb_out,
            ", scroll window size is %" MR_INTEGER_LENGTH_MODIFIER "u.\n",
            MR_scroll_limit);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_stack_default_limit(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_Unsigned n;

    if (word_count == 2) {
        if (MR_trace_is_natural_number(words[1], &n)) {
            MR_stack_default_line_limit = n;
            if (! MR_trace_internal_interacting) {
                return KEEP_INTERACTING;
            }

            if (MR_stack_default_line_limit > 0) {
                fprintf(MR_mdb_out,
                    "Default stack dump size limit set to %d.\n",
                    MR_stack_default_line_limit);
            } else {
                fprintf(MR_mdb_out,
                    "Default stack dump size limit set to none.\n");
            }
        } else {
            MR_trace_usage_cur_cmd();
        }
    } else if (word_count == 1) {
        if (MR_stack_default_line_limit > 0) {
            fprintf(MR_mdb_out, "Default stack dump size limit is %d.\n",
                MR_stack_default_line_limit);
        } else {
            fprintf(MR_mdb_out,
                "There is no default stack dump size limit.\n");
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

static const char   *MR_context_set_msg[] = {
    "Contexts will not be printed.",
    "Contexts will be printed before, on the same line.",
    "Contexts will be printed after, on the same line.",
    "Contexts will be printed on the previous line.",
    "Contexts will be printed on the next line.",
};

static const char   *MR_context_report_msg[] = {
    "Contexts are not printed.",
    "Contexts are printed before, on the same line.",
    "Contexts are printed after, on the same line.",
    "Contexts are printed on the previous line.",
    "Contexts are printed on the next line.",
};

MR_Next
MR_trace_cmd_context(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 2) {
        if (MR_streq(words[1], "none")) {
            MR_context_position = MR_CONTEXT_NOWHERE;
        } else if (MR_streq(words[1], "before")) {
            MR_context_position = MR_CONTEXT_BEFORE;
        } else if (MR_streq(words[1], "after")) {
            MR_context_position = MR_CONTEXT_AFTER;
        } else if (MR_streq(words[1], "prevline")) {
            MR_context_position = MR_CONTEXT_PREVLINE;
        } else if (MR_streq(words[1], "nextline")) {
            MR_context_position = MR_CONTEXT_NEXTLINE;
        } else {
            MR_trace_usage_cur_cmd();
            return KEEP_INTERACTING;
        }

        if (MR_trace_internal_interacting) {
            fprintf(MR_mdb_out, "%s\n",
                MR_context_set_msg[MR_context_position]);
        }
    } else if (word_count == 1) {
        switch (MR_context_position) {
            case MR_CONTEXT_NOWHERE:
            case MR_CONTEXT_BEFORE:
            case MR_CONTEXT_AFTER:
            case MR_CONTEXT_PREVLINE:
            case MR_CONTEXT_NEXTLINE:
                fprintf(MR_mdb_out, "%s\n",
                    MR_context_report_msg[MR_context_position]);
                break;

            default:
                MR_fatal_error("invalid MR_context_position");
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

static const char   *MR_user_event_context_set_msg[] = {
    "User events will get no contexts printed.",
    "User events will get only file contexts printed.",
    "User events will get only procedure contexts printed.",
    "User events will get full contexts printed.",
};

static const char   *MR_user_event_context_report_msg[] = {
    "User events get no contexts printed.",
    "User events get only file contexts printed.",
    "User events get only procedure contexts printed.",
    "User events get full contexts printed.",
};

MR_Next
MR_trace_cmd_user_event_context(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 2) {
        if (MR_streq(words[1], "none")) {
            MR_user_event_context = MR_USER_EVENT_CONTEXT_NONE;
        } else if (MR_streq(words[1], "file")) {
            MR_user_event_context = MR_USER_EVENT_CONTEXT_FILE;
        } else if (MR_streq(words[1], "proc")) {
            MR_user_event_context = MR_USER_EVENT_CONTEXT_PROC;
        } else if (MR_streq(words[1], "full")) {
            MR_user_event_context = MR_USER_EVENT_CONTEXT_FULL;
        } else {
            MR_trace_usage_cur_cmd();
            return KEEP_INTERACTING;
        }

        if (MR_trace_internal_interacting) {
            fprintf(MR_mdb_out, "%s\n",
                MR_user_event_context_set_msg[MR_user_event_context]);
        }
    } else if (word_count == 1) {
        switch (MR_user_event_context) {
            case MR_USER_EVENT_CONTEXT_NONE:
            case MR_USER_EVENT_CONTEXT_FILE:
            case MR_USER_EVENT_CONTEXT_PROC:
            case MR_USER_EVENT_CONTEXT_FULL:

                fprintf(MR_mdb_out, "%s\n",
                    MR_user_event_context_report_msg[MR_user_event_context]);
                break;

            default:
                MR_fatal_error("invalid MR_user_event_context");
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_goal_paths(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 2) {
        if (MR_streq(words[1], "off")) {
            MR_print_goal_paths = MR_FALSE;
            fprintf(MR_mdb_out, "Goal path printing is now off.\n");
        } else if (MR_streq(words[1], "on")) {
            MR_print_goal_paths = MR_TRUE;
            fprintf(MR_mdb_out, "Goal path printing is now on.\n");
        } else {
            MR_trace_usage_cur_cmd();
            return KEEP_INTERACTING;
        }
    } else if (word_count == 1) {
        if (MR_print_goal_paths) {
            fprintf(MR_mdb_out, "Goal path printing is on.\n");
        } else {
            fprintf(MR_mdb_out, "Goal path printing is off.\n");
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

static const char   *MR_scope_set_msg[] = {
    "The default scope of `break' commands is now all matching events.",
    "The default scope of `break' commands is now all matching interface events.",
    "The default scope of `break' commands is now all matching entry events.",
    "MDB INTERNAL ERROR: scope set to MR_SPY_SPECIFIC",
    "MDB INTERNAL ERROR: scope set to MR_SPY_LINENO",
};

static const char   *MR_scope_report_msg[] = {
    "The default scope of `break' commands is all matching events.",
    "The default scope of `break' commands is all matching interface events.",
    "The default scope of `break' commands is all matching entry events.",
    "MDB INTERNAL ERROR: scope set to MR_SPY_SPECIFIC",
    "MDB INTERNAL ERROR: scope set to MR_SPY_LINENO",
};

MR_Next
MR_trace_cmd_scope(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 2) {
        if (MR_streq(words[1], "all")) {
            MR_default_breakpoint_scope = MR_SPY_ALL;
        } else if (MR_streq(words[1], "interface")) {
            MR_default_breakpoint_scope = MR_SPY_INTERFACE;
        } else if (MR_streq(words[1], "entry")) {
            MR_default_breakpoint_scope = MR_SPY_ENTRY;
        } else {
            MR_trace_usage_cur_cmd();
            return KEEP_INTERACTING;
        }

        if (MR_trace_internal_interacting) {
            fprintf(MR_mdb_out, "%s\n",
                MR_scope_set_msg[MR_default_breakpoint_scope]);
        }
    } else if (word_count == 1) {
        switch (MR_default_breakpoint_scope) {
        case MR_SPY_ALL:
        case MR_SPY_INTERFACE:
        case MR_SPY_ENTRY:
            fprintf(MR_mdb_out, "%s\n",
                MR_scope_report_msg[MR_default_breakpoint_scope]);
            break;

        default:
            MR_fatal_error(
                "invalid MR_default_breakpoint_scope");
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_echo(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 2) {
        if (MR_streq(words[1], "off")) {
            MR_echo_commands = MR_FALSE;
            if (MR_trace_internal_interacting) {
                fprintf(MR_mdb_out, "Command echo disabled.\n");
            }
        } else if (MR_streq(words[1], "on")) {
            if (!MR_echo_commands) {
                // Echo the `echo on' command. This is needed for historical
                // reasons (compatibility with our existing test suite).

                fprintf(MR_mdb_out, "echo on\n");
                MR_echo_commands = MR_TRUE;
            }
            if (MR_trace_internal_interacting) {
                fprintf(MR_mdb_out, "Command echo enabled.\n");
            }
        } else {
            MR_trace_usage_cur_cmd();
        }
    } else if (word_count == 1) {
        fprintf(MR_mdb_out, "Command echo is ");
        if (MR_echo_commands) {
            fprintf(MR_mdb_out, "on.\n");
        } else {
            fprintf(MR_mdb_out, "off.\n");
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_list_context_lines(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_Unsigned n;

    if (word_count == 2 && MR_trace_is_natural_number(words[1], &n)) {
        MR_num_context_lines = n;
    } else if (word_count == 1) {
        fprintf(MR_mdb_out,
            "Printing %" MR_INTEGER_LENGTH_MODIFIER
            "u lines around each context listing\n",
            MR_num_context_lines);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_list_path(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count < 2) {
        MR_Word list;

        list = ML_LISTING_get_list_path(MR_listing_path);
        if (MR_list_is_empty(list)) {
            fprintf(MR_mdb_out, "Context search path is empty\n");
        } else {
            fprintf(MR_mdb_out, "Context search path:");
            while (! MR_list_is_empty(list)) {
                fprintf(MR_mdb_out, " %s", (const char *) MR_list_head(list));
                list = MR_list_tail(list);
            }
            fprintf(MR_mdb_out, "\n");
        }
    } else {
        int       i;
        MR_String aligned_word;

        MR_TRACE_CALL_MERCURY(
            ML_LISTING_clear_list_path(MR_listing_path, &MR_listing_path);
            for(i = word_count - 1; i >= 1; i--) {
                MR_TRACE_USE_HP(
                    MR_make_aligned_string(aligned_word, (MR_String) words[i]);
                );
                ML_LISTING_push_list_path(aligned_word,
                    MR_listing_path, &MR_listing_path);
            }
        );

        MR_listing_path =
            MR_make_permanent(MR_listing_path,
                (MR_TypeInfo) ML_LISTING_listing_type());
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_push_list_dir(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    int       i;
    MR_String aligned_word;

    if (word_count < 2) {
        MR_trace_usage_cur_cmd();
        return KEEP_INTERACTING;
    }

    MR_TRACE_CALL_MERCURY(
        for(i = word_count - 1; i >= 1; i--) {
            MR_TRACE_USE_HP(
                MR_make_aligned_string(aligned_word, (MR_String) words[i]);
            );
            ML_LISTING_push_list_path(aligned_word,
                MR_listing_path, &MR_listing_path);
        }
    );

    MR_listing_path =
        MR_make_permanent(MR_listing_path,
            (MR_TypeInfo) ML_LISTING_listing_type());

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_pop_list_dir(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count > 1) {
        MR_trace_usage_cur_cmd();
        return KEEP_INTERACTING;
    }

    MR_TRACE_CALL_MERCURY(
        ML_LISTING_pop_list_path(MR_listing_path, &MR_listing_path);
    );

    MR_listing_path =
        MR_make_permanent(MR_listing_path,
            (MR_TypeInfo) ML_LISTING_listing_type());

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_fail_trace_counts(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 2) {
        if (MR_dice_fail_trace_counts_file != NULL) {
            free(MR_dice_fail_trace_counts_file);
        }

        MR_dice_fail_trace_counts_file = MR_copy_string(words[1]);
    } else if (word_count == 1) {
        if (MR_dice_fail_trace_counts_file == NULL) {
            fprintf(MR_mdb_out,
                "The failing tests trace counts file has not been set.\n");
        } else {
            fprintf(MR_mdb_out,
                "The failing tests trace counts file is %s\n",
                MR_dice_fail_trace_counts_file);
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_pass_trace_counts(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 2) {
        if (MR_dice_pass_trace_counts_file != NULL) {
            free(MR_dice_pass_trace_counts_file);
        }

        MR_dice_pass_trace_counts_file = MR_copy_string(words[1]);
    } else if (word_count == 1) {
        if (MR_dice_pass_trace_counts_file == NULL) {
            fprintf(MR_mdb_out,
                "The passing tests trace counts file has not been set.\n");
        } else {
            fprintf(MR_mdb_out,
                "The passing tests trace counts file is %s\n",
                MR_dice_pass_trace_counts_file);
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_max_io_actions(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_Unsigned num_io_actions;

    if (word_count == 2 &&
            MR_trace_is_natural_number(words[1], &num_io_actions)) {
        MR_TRACE_CALL_MERCURY(
            ML_BROWSE_set_num_io_actions(num_io_actions,
                MR_trace_browser_persistent_state,
                &MR_trace_browser_persistent_state);
        );
    } else if (word_count == 1) {

        MR_Integer n;

        MR_TRACE_CALL_MERCURY(
            ML_BROWSE_get_num_io_actions(
                MR_trace_browser_persistent_state, &n);
        );

        // We do this to avoid warnings about MR_Integer and int
        // having different sizes on 64-bit architectures.

        num_io_actions = (int) n;

        fprintf(MR_mdb_out,
            "The maximum number of I/O actions printed is %"
            MR_INTEGER_LENGTH_MODIFIER "u\n",
            num_io_actions);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_xml_browser_cmd(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 2) {
        char    *copied_value;
        char    *aligned_value;

        copied_value = (char *) MR_GC_malloc(strlen(words[1]) + 1);
        strcpy(copied_value, words[1]);
        MR_TRACE_USE_HP(
            MR_make_aligned_string(aligned_value, copied_value);
        );
        MR_TRACE_CALL_MERCURY(
            ML_BROWSE_set_xml_browser_cmd_from_mdb(aligned_value,
                MR_trace_browser_persistent_state,
                &MR_trace_browser_persistent_state);
        );
    } else if (word_count == 1) {
        MR_String   command;

        MR_TRACE_CALL_MERCURY(
            ML_BROWSE_get_xml_browser_cmd_from_mdb(
                MR_trace_browser_persistent_state, &command);
        );

        if (command != NULL && strlen(command) > 0) {
            fprintf(MR_mdb_out, "The XML browser command is %s\n", command);
        } else {
            fprintf(MR_mdb_out, "The XML browser command has not been set.\n");
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_xml_tmp_filename(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 2) {
        char    *copied_value;
        char    *aligned_value;

        copied_value = (char *) MR_GC_malloc(strlen(words[1]) + 1);
        strcpy(copied_value, words[1]);
        MR_TRACE_USE_HP(
            MR_make_aligned_string(aligned_value, copied_value);
        );
        MR_TRACE_CALL_MERCURY(
            ML_BROWSE_set_xml_tmp_filename_from_mdb(aligned_value,
                MR_trace_browser_persistent_state,
                &MR_trace_browser_persistent_state);
        );
    } else if (word_count == 1) {
        MR_String   file;

        MR_TRACE_CALL_MERCURY(
            ML_BROWSE_get_xml_browser_cmd_from_mdb(
                MR_trace_browser_persistent_state, &file);
        );

        if (file != NULL && strlen(file) > 0) {
            fprintf(MR_mdb_out, "The XML tmp filename is %s\n", file);
        } else {
            fprintf(MR_mdb_out, "The XML tmp filename has not been set.\n");
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_web_browser_cmd(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 2) {
        char    *copied_value;
        char    *aligned_value;

        copied_value = (char *) MR_GC_malloc(strlen(words[1]) + 1);
        strcpy(copied_value, words[1]);
        MR_TRACE_USE_HP(
            MR_make_aligned_string(aligned_value, copied_value);
        );
        MR_TRACE_CALL_MERCURY(
            ML_BROWSE_set_web_browser_cmd_from_mdb(aligned_value,
                MR_trace_browser_persistent_state,
                &MR_trace_browser_persistent_state);
        );
    } else if (word_count == 1) {
        MR_String   command;

        MR_TRACE_CALL_MERCURY(
            ML_BROWSE_get_web_browser_cmd_from_mdb(
                MR_trace_browser_persistent_state, &command);
        );

        if (command != NULL && strlen(command) > 0) {
            fprintf(MR_mdb_out, "The web browser command is %s\n", command);
        } else {
            fprintf(MR_mdb_out, "The web browser command has not been set.\n");
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_format(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_BrowseFormat    new_format;
    MR_Word             print;
    MR_Word             browse;
    MR_Word             print_all;

    if (! MR_trace_options_cmd_format(&print, &browse, &print_all,
        &words, &word_count))
    {
        // The usage message has already been printed.
        ;
    } else if (word_count == 2 &&
        MR_trace_is_portray_format(words[1], &new_format))
    {
        MR_TRACE_CALL_MERCURY(
            ML_BROWSE_set_format_from_mdb(print, browse, print_all, new_format,
                MR_trace_browser_persistent_state,
                &MR_trace_browser_persistent_state);
        );
        MR_trace_browser_persistent_state =
            MR_make_permanent(MR_trace_browser_persistent_state,
                MR_trace_browser_persistent_state_type);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_format_param(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_Word             print;
    MR_Word             browse;
    MR_Word             print_all;
    MR_Word             flat;
    MR_Word             raw_pretty;
    MR_Word             verbose;
    MR_Word             pretty;
    MR_Unsigned         n;

    if (! MR_trace_options_cmd_format_param(&print, &browse, &print_all,
        &flat, &raw_pretty, &verbose, &pretty, &words, &word_count))
    {
        // The usage message has already been printed.
        ;
    } else if (word_count != 3) {
        MR_trace_usage_cur_cmd();
    } else {
        if (MR_streq(words[1], "depth") &&
            MR_trace_is_natural_number(words[2], &n))
        {
            MR_TRACE_CALL_MERCURY(
                ML_BROWSE_set_depth_from_mdb(print, browse, print_all,
                    flat, raw_pretty, verbose, pretty, n,
                    MR_trace_browser_persistent_state,
                    &MR_trace_browser_persistent_state);
            );
        } else if (MR_streq(words[1], "size") &&
            MR_trace_is_natural_number(words[2], &n))
        {
            MR_TRACE_CALL_MERCURY(
                ML_BROWSE_set_size_from_mdb(print, browse, print_all,
                    flat, raw_pretty, verbose, pretty, n,
                    MR_trace_browser_persistent_state,
                    &MR_trace_browser_persistent_state);
            );
        } else if (MR_streq(words[1], "width") &&
            MR_trace_is_natural_number(words[2], &n))
        {
            MR_TRACE_CALL_MERCURY(
                ML_BROWSE_set_width_from_mdb(print, browse, print_all,
                    flat, raw_pretty, verbose, pretty, n,
                    MR_trace_browser_persistent_state,
                    &MR_trace_browser_persistent_state);
            );
        } else if (MR_streq(words[1], "lines") &&
            MR_trace_is_natural_number(words[2], &n))
        {
            MR_TRACE_CALL_MERCURY(
                ML_BROWSE_set_lines_from_mdb(print, browse, print_all,
                    flat, raw_pretty, verbose, pretty, n,
                    MR_trace_browser_persistent_state,
                    &MR_trace_browser_persistent_state);
            );
        } else {
            MR_trace_usage_cur_cmd();
            return KEEP_INTERACTING;
        }

        MR_trace_browser_persistent_state =
            MR_make_permanent(MR_trace_browser_persistent_state,
                MR_trace_browser_persistent_state_type);
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_alias(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 1) {
        MR_trace_print_all_aliases(MR_mdb_out, MR_FALSE);
    } else if (word_count == 2) {
        MR_trace_print_alias(MR_mdb_out, words[1]);
    } else {
        if (MR_trace_valid_command(words[2])) {
            MR_trace_add_alias(words[1], words+2, word_count-2);
            if (MR_trace_internal_interacting) {
                MR_trace_print_alias(MR_mdb_out, words[1]);
            }
        } else {
            fprintf(MR_mdb_out, "`%s' is not a valid command.\n", words[2]);
        }
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_unalias(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 2) {
        if (MR_trace_remove_alias(words[1])) {
            if (MR_trace_internal_interacting) {
                fprintf(MR_mdb_out, "Alias `%s' removed.\n", words[1]);
            }
        } else {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err,
                "Alias `%s' cannot be removed, since it does not exist.\n",
                words[1]);
        }
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

////////////////////////////////////////////////////////////////////////////

void
MR_trace_listing_path_ensure_init(void)
{
    static MR_bool  MR_trace_listing_path_initialized = MR_FALSE;

    if (! MR_trace_listing_path_initialized) {
        MR_TRACE_CALL_MERCURY(
            MR_listing_path = ML_LISTING_new_list_path();
        );
        MR_trace_listing_path_initialized = MR_TRUE;
    }
}

////////////////////////////////////////////////////////////////////////////

const char *const    MR_trace_printlevel_cmd_args[] =
    { "none", "some", "all", NULL };

const char *const    MR_trace_on_off_args[] =
    { "on", "off", NULL };

const char *const    MR_trace_context_cmd_args[] =
    { "none", "before", "after", "prevline", "nextline", NULL };

const char *const    MR_trace_user_event_context_cmd_args[] =
    { "none", "file", "proc", "full", NULL };

const char *const    MR_trace_scope_cmd_args[] =
    { "all", "interface", "entry", NULL };

const char *const    MR_trace_format_cmd_args[] =
    { "-A", "-B", "-P",
    "--print-all", "--print", "--browse",
    "flat", "pretty", "verbose", NULL };

const char *const    MR_trace_format_param_cmd_args[] =
    { "-A", "-B", "-P", "-f", "-p", "-v",
    "--print-all", "--print", "--browse",
    "--flat", "--pretty", "--verbose",
    "depth", "size", "width", "lines",
    "flat", "pretty", "verbose", NULL };

////////////////////////////////////////////////////////////////////////////

static struct MR_option MR_trace_param_cmd_format_opts[] =
{
    { "print",      MR_no_argument, NULL,   'P' },
    { "browse",     MR_no_argument, NULL,   'B' },
    { "print-all",  MR_no_argument, NULL,   'A' },
    { NULL,         MR_no_argument, NULL,   0 }
};

static MR_bool
MR_trace_options_cmd_format(MR_Word *print_set, MR_Word *browse_set,
    MR_Word *print_all_set, char ***words, int *word_count)
{
    int     c;
    MR_Word mercury_bool_yes;
    MR_Word mercury_bool_no;

    MR_TRACE_CALL_MERCURY(
        mercury_bool_yes = ML_BROWSE_mercury_bool_yes();
        mercury_bool_no = ML_BROWSE_mercury_bool_no();
    );

    *print_set = mercury_bool_no;
    *browse_set = mercury_bool_no;
    *print_all_set = mercury_bool_no;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "PBA",
        MR_trace_param_cmd_format_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'P':
                *print_set = mercury_bool_yes;
                break;

            case 'B':
                *browse_set = mercury_bool_yes;
                break;

            case 'A':
                *print_all_set = mercury_bool_yes;
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

static struct MR_option MR_trace_param_cmd_format_param_opts[] =
{
    { "flat",       MR_no_argument, NULL,   'f' },
    { "raw_pretty", MR_no_argument, NULL,   'r' },
    { "verbose",    MR_no_argument, NULL,   'v' },
    { "pretty",     MR_no_argument, NULL,   'p' },
    { "print",      MR_no_argument, NULL,   'P' },
    { "browse",     MR_no_argument, NULL,   'B' },
    { "print-all",  MR_no_argument, NULL,   'A' },
    { NULL,         MR_no_argument, NULL,   0 }
};

static MR_bool
MR_trace_options_cmd_format_param(MR_Word *print_set, MR_Word *browse_set,
    MR_Word *print_all_set, MR_Word *flat_format,
    MR_Word *raw_pretty_format, MR_Word *verbose_format,
    MR_Word *pretty_format, char ***words, int *word_count)
{
    int     c;
    MR_Word mercury_bool_yes;
    MR_Word mercury_bool_no;

    MR_TRACE_CALL_MERCURY(
        mercury_bool_yes = ML_BROWSE_mercury_bool_yes();
        mercury_bool_no = ML_BROWSE_mercury_bool_no();
    );

    *print_set = mercury_bool_no;
    *browse_set = mercury_bool_no;
    *print_all_set = mercury_bool_no;
    *flat_format = mercury_bool_no;
    *raw_pretty_format = mercury_bool_no;
    *verbose_format = mercury_bool_no;
    *pretty_format = mercury_bool_no;

    MR_optind = 0;
    while ((c = MR_getopt_long(*word_count, *words, "PBAfrvp",
        MR_trace_param_cmd_format_param_opts, NULL)) != EOF)
    {
        switch (c) {

            case 'f':
                *flat_format = mercury_bool_yes;
                break;

            case 'r':
                *raw_pretty_format = mercury_bool_yes;
                break;

            case 'v':
                *verbose_format = mercury_bool_yes;
                break;

            case 'p':
                *pretty_format = mercury_bool_yes;
                break;

            case 'P':
                *print_set = mercury_bool_yes;
                break;

            case 'B':
                *browse_set = mercury_bool_yes;
                break;

            case 'A':
                *print_all_set = mercury_bool_yes;
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
