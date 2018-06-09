// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#ifndef MERCURY_TRACE_CMD_BREAKPOINT_H
#define MERCURY_TRACE_CMD_BREAKPOINT_H

#include "mercury_imp.h"

#include "mercury_trace_cmds.h"

extern  MR_TraceCmdFunc     MR_trace_cmd_break;
extern  MR_TraceCmdFunc     MR_trace_cmd_condition;
extern  MR_TraceCmdFunc     MR_trace_cmd_ignore;
extern  MR_TraceCmdFunc     MR_trace_cmd_break_print;
extern  MR_TraceCmdFunc     MR_trace_cmd_enable;
extern  MR_TraceCmdFunc     MR_trace_cmd_disable;
extern  MR_TraceCmdFunc     MR_trace_cmd_delete;
extern  MR_TraceCmdFunc     MR_trace_cmd_register;
extern  MR_TraceCmdFunc     MR_trace_cmd_modules;
extern  MR_TraceCmdFunc     MR_trace_cmd_procedures;

extern  const char *const   MR_trace_break_cmd_args[];
extern  const char *const   MR_trace_ignore_cmd_args[];

#endif  // MERCURY_TRACE_CMD_BREAKPOINT_H
