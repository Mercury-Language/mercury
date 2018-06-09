// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

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
