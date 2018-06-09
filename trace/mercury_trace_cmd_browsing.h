// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#ifndef MERCURY_TRACE_CMD_BROWSING_H
#define MERCURY_TRACE_CMD_BROWSING_H

#include "mercury_std.h"
#include "mercury_types.h"

#include "mercury_trace.h"
#include "mercury_trace_cmds.h"
#include "mercury_trace_browse.h"

extern  MR_TraceCmdFunc     MR_trace_cmd_level;
extern  MR_TraceCmdFunc     MR_trace_cmd_up;
extern  MR_TraceCmdFunc     MR_trace_cmd_down;
extern  MR_TraceCmdFunc     MR_trace_cmd_vars;
extern  MR_TraceCmdFunc     MR_trace_cmd_held_vars;
extern  MR_TraceCmdFunc     MR_trace_cmd_print;
extern  MR_TraceCmdFunc     MR_trace_cmd_browse;
extern  MR_TraceCmdFunc     MR_trace_cmd_stack;
extern  MR_TraceCmdFunc     MR_trace_cmd_current;
extern  MR_TraceCmdFunc     MR_trace_cmd_view;
extern  MR_TraceCmdFunc     MR_trace_cmd_hold;
extern  MR_TraceCmdFunc     MR_trace_cmd_diff;
extern  MR_TraceCmdFunc     MR_trace_cmd_dump;
extern  MR_TraceCmdFunc     MR_trace_cmd_list;

// If we are attached to a source server, then find the appropriate
// context and ask the server to point to it; otherwise, do nothing.

extern  void                MR_trace_maybe_sync_source_window(
                                MR_EventInfo *event_info, MR_bool verbose);

extern  void                MR_trace_browse_internal(MR_Word type_info,
                                MR_Word value, MR_BrowseCallerType caller,
                                MR_BrowseFormat format);
extern  void                MR_trace_browse_goal_internal(MR_ConstString name,
                                MR_Word arg_list, MR_Word is_func,
                                MR_BrowseCallerType caller,
                                MR_BrowseFormat format);

extern  const char *const   MR_trace_print_cmd_args[];
extern  const char *const   MR_trace_stack_cmd_args[];
extern  const char *const   MR_trace_view_cmd_args[];

#endif  // MERCURY_TRACE_CMD_BROWSING_H
