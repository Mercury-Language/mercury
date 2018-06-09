// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module implements the mdb commands in the "queries" category.
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

#include "mercury_trace_internal.h"
#include "mercury_trace_cmds.h"
#include "mercury_trace_cmd_queries.h"
#include "mercury_trace_cmd_parameter.h"
#include "mercury_trace_browse.h"

////////////////////////////////////////////////////////////////////////////

MR_Next
MR_trace_cmd_query(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_trace_query(MR_NORMAL_QUERY, MR_mmc_options, word_count - 1, words + 1);
    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_cc_query(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_trace_query(MR_CC_QUERY, MR_mmc_options, word_count - 1, words + 1);
    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_io_query(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    MR_trace_query(MR_IO_QUERY, MR_mmc_options, word_count - 1, words + 1);
    return KEEP_INTERACTING;
}

////////////////////////////////////////////////////////////////////////////
