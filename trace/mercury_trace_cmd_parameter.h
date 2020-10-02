// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2007 The University of Melbourne.
// Copyright (C) 2017-2018, 2020 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#ifndef MERCURY_TRACE_CMD_PARAMETER_H
#define MERCURY_TRACE_CMD_PARAMETER_H

#include "mercury_std.h"
#include "mercury_types.h"
#include "mercury_stack_trace.h"    // for MR_Context_Position

#include "mercury_trace_cmds.h"
#include "mercury_trace_spy.h"      // for MR_SpyWhen

// Options to pass to mmc when compiling queries.

extern  char                *MR_mmc_options;

// The default print level governs whether we out reports for events at which
// we don't stop.

extern  MR_TracePrintLevel MR_default_print_level;

// These variables say (a) whether the printing of event sequences will pause
// after each screenful of events, (b) how may events constitute a screenful
// (although we count only events, not how many lines they take up), and (c)
// how many events we have printed so far in this screenful.

extern  MR_bool             MR_scroll_control;
extern  MR_Unsigned         MR_scroll_limit;
extern  MR_Unsigned         MR_scroll_next;

// This variable controls the number of stack frame lines printed by the stack
// and nondet_stack commands if the user doesn't override it.

extern  int                 MR_stack_default_line_limit;

// We echo each command just as it is executed iff this variable is MR_TRUE.

extern  MR_bool             MR_echo_commands;

// We include values of sometimes-useful types such as typeinfos in the set of
// variables whose values we collect at events for possible later printing
// only if MR_print_optionals is true.

extern  MR_bool             MR_print_optionals;

// This variable holds either the name of a file which contains a list of
// the file names of passing test case trace counts, or the name of a single
// file of passing test case trace counts.

extern  char                *MR_dice_pass_trace_counts_file;

// This variable holds either the name of a file which contains a list of
// the file names of failing test case trace counts, or the name of a single
// file of failing test case trace counts.

extern  char                *MR_dice_fail_trace_counts_file;

// MR_context_position specifies whether we print context at events,
// and if so, where.

extern  MR_ContextPosition  MR_context_position;

// MR_user_event_context specifies what context to print at user events.

extern  MR_UserEventContext MR_user_event_context;

// MR_print_goal_paths specifies whether we print goal paths at events.

extern  MR_bool             MR_print_goal_paths;

// MR_listing_path holds the current value of the listings structure
// as defined in browser/listing.m. You need to ensure that it is initialized
// before accessing it.

extern  MR_Word             MR_listing_path;
extern  void                MR_trace_listing_path_ensure_init(void);

// MR_num_context_lines holds the current number of context lines to be
// printed before and after the current callee/caller's file context.

extern  MR_Unsigned         MR_num_context_lines;

extern  char *              MR_listing_cmd;

extern  MR_SpyWhen          MR_default_breakpoint_scope;

extern  MR_TraceCmdFunc     MR_trace_cmd_mmc_options;
extern  MR_TraceCmdFunc     MR_trace_cmd_printlevel;
extern  MR_TraceCmdFunc     MR_trace_cmd_scroll;
extern  MR_TraceCmdFunc     MR_trace_cmd_stack_default_limit;
extern  MR_TraceCmdFunc     MR_trace_cmd_context;
extern  MR_TraceCmdFunc     MR_trace_cmd_user_event_context;
extern  MR_TraceCmdFunc     MR_trace_cmd_goal_paths;
extern  MR_TraceCmdFunc     MR_trace_cmd_scope;
extern  MR_TraceCmdFunc     MR_trace_cmd_echo;
extern  MR_TraceCmdFunc     MR_trace_cmd_list_context_lines;
extern  MR_TraceCmdFunc     MR_trace_cmd_list_path;
extern  MR_TraceCmdFunc     MR_trace_cmd_push_list_dir;
extern  MR_TraceCmdFunc     MR_trace_cmd_pop_list_dir;
extern  MR_TraceCmdFunc     MR_trace_cmd_list_cmd;
extern  MR_TraceCmdFunc     MR_trace_cmd_fail_trace_counts;
extern  MR_TraceCmdFunc     MR_trace_cmd_pass_trace_counts;
extern  MR_TraceCmdFunc     MR_trace_cmd_max_io_actions;
extern  MR_TraceCmdFunc     MR_trace_cmd_xml_browser_cmd;
extern  MR_TraceCmdFunc     MR_trace_cmd_xml_tmp_filename;
extern  MR_TraceCmdFunc     MR_trace_cmd_web_browser_cmd;
extern  MR_TraceCmdFunc     MR_trace_cmd_format;
extern  MR_TraceCmdFunc     MR_trace_cmd_format_param;
extern  MR_TraceCmdFunc     MR_trace_cmd_alias;
extern  MR_TraceCmdFunc     MR_trace_cmd_unalias;

extern  const char *const   MR_trace_printlevel_cmd_args[];
extern  const char *const   MR_trace_on_off_args[];
extern  const char *const   MR_trace_context_cmd_args[];
extern  const char *const   MR_trace_user_event_context_cmd_args[];
extern  const char *const   MR_trace_scope_cmd_args[];
extern  const char *const   MR_trace_format_cmd_args[];
extern  const char *const   MR_trace_format_param_cmd_args[];

#endif  // MERCURY_TRACE_CMD_PARAMETER_H
