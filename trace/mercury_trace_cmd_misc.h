// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2006 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

#ifndef MERCURY_TRACE_CMD_MISC_H
#define MERCURY_TRACE_CMD_MISC_H

#include "mercury_trace_cmds.h"

extern  MR_TraceCmdFunc     MR_trace_cmd_source;
extern  MR_TraceCmdFunc     MR_trace_cmd_save;
extern  MR_TraceCmdFunc     MR_trace_cmd_shell;
extern  MR_TraceCmdFunc     MR_trace_cmd_quit;

extern  const char *const   MR_trace_source_cmd_args[];
extern  const char *const   MR_trace_quit_cmd_args[];

#endif  // MERCURY_TRACE_CMD_MISC_H
