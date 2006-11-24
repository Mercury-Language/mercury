/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 1998-2006 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

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
