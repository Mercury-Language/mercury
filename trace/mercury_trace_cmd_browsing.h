/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 1998-2006 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

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

/*
** If we are attached to a source server, then find the appropriate
** context and ask the server to point to it, otherwise do nothing.
*/

extern  void                MR_trace_maybe_sync_source_window(
                                MR_Event_Info *event_info, MR_bool verbose);

extern  void                MR_trace_browse_internal(MR_Word type_info,
                                MR_Word value, MR_Browse_Caller_Type caller,
                                MR_Browse_Format format);
extern  void                MR_trace_browse_goal_internal(MR_ConstString name,
                                MR_Word arg_list, MR_Word is_func,
                                MR_Browse_Caller_Type caller,
                                MR_Browse_Format format);

extern  const char *const   MR_trace_print_cmd_args[];
extern  const char *const   MR_trace_stack_cmd_args[];
extern  const char *const   MR_trace_view_cmd_args[];
