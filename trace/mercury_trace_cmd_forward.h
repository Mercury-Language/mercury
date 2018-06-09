// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#ifndef MERCURY_TRACE_CMD_FORWARD_H
#define MERCURY_TRACE_CMD_FORWARD_H

#include "mercury_imp.h"

#include "mercury_trace.h"

extern  MR_TraceCmdFunc     MR_trace_cmd_step;
extern  MR_TraceCmdFunc     MR_trace_cmd_goto;
extern  MR_TraceCmdFunc     MR_trace_cmd_next;
extern  MR_TraceCmdFunc     MR_trace_cmd_finish;
extern  MR_TraceCmdFunc     MR_trace_cmd_fail;
extern  MR_TraceCmdFunc     MR_trace_cmd_exception;
extern  MR_TraceCmdFunc     MR_trace_cmd_return;
extern  MR_TraceCmdFunc     MR_trace_cmd_user;
extern  MR_TraceCmdFunc     MR_trace_cmd_forward;
extern  MR_TraceCmdFunc     MR_trace_cmd_mindepth;
extern  MR_TraceCmdFunc     MR_trace_cmd_maxdepth;
extern  MR_TraceCmdFunc     MR_trace_cmd_continue;

extern  const char *const   MR_trace_movement_cmd_args[];

#endif  // MERCURY_TRACE_CMD_FORWARD_H
