// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2006 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#include "mercury_std.h"
#include "mercury_types.h"

#include "mercury_trace.h"
#include "mercury_trace_completion.h"

#ifndef MERCURY_TRACE_CMDS_H
#define MERCURY_TRACE_CMDS_H

typedef enum {
    KEEP_INTERACTING,
    STOP_INTERACTING
} MR_Next;

typedef MR_Next MR_TraceCmdFunc(char **words, int word_count,
                    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info,
                    MR_Code **jumpaddr);

// We keep a table of the available commands. The information we have about
// each command is stored in a value of type MR_TraceCmdTableEntry.
//
// The name of the command itself is stored in the name field; the category
// field contains name of the category to which the command belongs,
// e.g. "browsing".
//
// The code that the command loop should execute to handle a command of a given
// type is the function pointed to by the function field.
//
// Some commands take fixed strings as arguments. The arg_strings field
// is a NULL terminated array of those strings, or NULL if there are
// no fixed strings.
//
// The arg_completer field contains the address of a function for more
// arbitrary completion, e.g. on predicate names. This field should not be
// null; if the command cannot use a completion function, the field should
// contain MR_trace_null_completer.

typedef struct
{
    const char                  *MR_cmd_category;
    const char                  *MR_cmd_name;
    MR_TraceCmdFunc             *MR_cmd_function;
    const char *const           *MR_cmd_arg_strings;
    const MR_MakeCompleter      MR_cmd_arg_completer;
} MR_TraceCmdTableEntry;

#endif  // MERCURY_TRACE_CMDS_H
