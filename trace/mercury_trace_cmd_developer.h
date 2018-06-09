// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2006, 2008 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#ifndef MERCURY_TRACE_CMD_DEVELOPER_H
#define MERCURY_TRACE_CMD_DEVELOPER_H

#include "mercury_imp.h"

#include "mercury_trace_cmds.h"

extern  MR_TraceCmdFunc     MR_trace_cmd_var_details;
extern  MR_TraceCmdFunc     MR_trace_cmd_term_size;
extern  MR_TraceCmdFunc     MR_trace_cmd_flag;
extern  MR_TraceCmdFunc     MR_trace_cmd_subgoal;
extern  MR_TraceCmdFunc     MR_trace_cmd_consumer;
extern  MR_TraceCmdFunc     MR_trace_cmd_gen_stack;
extern  MR_TraceCmdFunc     MR_trace_cmd_cut_stack;
extern  MR_TraceCmdFunc     MR_trace_cmd_pneg_stack;
extern  MR_TraceCmdFunc     MR_trace_cmd_mm_stacks;
extern  MR_TraceCmdFunc     MR_trace_cmd_nondet_stack;
extern  MR_TraceCmdFunc     MR_trace_cmd_stack_regs;
extern  MR_TraceCmdFunc     MR_trace_cmd_all_regs;
extern  MR_TraceCmdFunc     MR_trace_cmd_debug_vars;
extern  MR_TraceCmdFunc     MR_trace_cmd_stats;
extern  MR_TraceCmdFunc     MR_trace_cmd_print_optionals;
extern  MR_TraceCmdFunc     MR_trace_cmd_unhide_events;
extern  MR_TraceCmdFunc     MR_trace_cmd_table;
extern  MR_TraceCmdFunc     MR_trace_cmd_type_ctor;
extern  MR_TraceCmdFunc     MR_trace_cmd_class_decl;
extern  MR_TraceCmdFunc     MR_trace_cmd_all_type_ctors;
extern  MR_TraceCmdFunc     MR_trace_cmd_all_class_decls;
extern  MR_TraceCmdFunc     MR_trace_cmd_all_procedures;
extern  MR_TraceCmdFunc     MR_trace_cmd_ambiguity;
extern  MR_TraceCmdFunc     MR_trace_cmd_trail_details;

extern  const char *const   MR_trace_nondet_stack_cmd_args[];
extern  const char *const   MR_trace_stats_cmd_args[];

#endif  // MERCURY_TRACE_CMD_DEVELOPER_H
