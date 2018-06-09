// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2002, 2005-2006, 2008 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#ifndef MERCURY_TRACE_INTERNAL_H
#define MERCURY_TRACE_INTERNAL_H

#include "mercury_types.h"              // for MR_Code
#include "mercury_trace_base.h"         // for MR_SavedDebugState

#include "mercury_trace.h"              // for MR_EventInfo, etc.
#include "mercury_std.h"                // for MR_bool
#include "mercury_trace_cmds.h"         // for MR_SpyPrintList
#include "mercury_trace_completion.h"   // for MR_MakeCompleter
#include "mercury_trace_spy.h"          // for MR_SpyPrintList
#include "mercury_trace_source.h"       // for MR_TraceSourceServer

#include <stdio.h>                      // for FILE

extern  MR_Code     *MR_trace_event_internal(MR_TraceCmdInfo *cmd,
                        MR_bool interactive, MR_SpyPrintList print_list,
                        MR_EventInfo *event_info, const char *msg);

extern  void        MR_trace_event_print_internal_report(
                        MR_EventInfo *event_info);

// Debugger I/O streams.
// Replacements for stdin/stdout/stderr respectively.
//
// The distinction between MR_mdb_out and MR_mdb_err is analagous to
// the distinction between stdout and stderr: ordinary output, including
// information messages about conditions which are not errors, should
// go to MR_mdb_out, but error messages should go to MR_mdb_err.

extern  FILE    *MR_mdb_in;
extern  FILE    *MR_mdb_out;
extern  FILE    *MR_mdb_err;

// We print confirmation of commands (e.g. new aliases) if this is MR_TRUE.

extern  MR_bool         MR_trace_internal_interacting;

// If this true, we print every command we get from the command queue
// just before it is executed. This is intended for debugging the debugger.

extern  MR_bool         MR_trace_echo_queue_commands;

// The details of the source server, if any.

extern  MR_TraceSourceServer  MR_trace_source_server;

// Source commands from the given file. Print an error message if the file
// cannot be read, unless ignore_errors is true.

extern  MR_bool     MR_trace_source(const char *filename,
                        MR_bool ignore_errors, char **args, int num_args);

// Source commands from the given file.

extern  void        MR_trace_source_from_open_file(FILE *fp,
                        char **args, int num_args);

// Print a usage message for the currently executing command.

extern  void    MR_trace_usage_cur_cmd(void);

// Print a message about this command being a no-op from this port.
// The second variant is when the command is a no-op only because of
// the reuse of stack frames by tail recursive procedures.

extern  void    MR_trace_do_noop(void);
extern  void    MR_trace_do_noop_tail_rec(void);

// If the given word is the name of a valid command, return its info.

extern  const MR_TraceCmdTableEntry
                *MR_trace_valid_command(const char *word);

// Close a source server, if there is one attached.

extern  void    MR_trace_maybe_close_source_window(MR_bool verbose);

// This just prints to MR_mdb_out a message telling the user
// that the debugger caught an interrupt.

extern  void    MR_trace_interrupt_message(void);

extern  char    *MR_trace_getline(const char *prompt, FILE *mdb_in,
                    FILE *mdb_out);
extern  char    *MR_trace_get_command(const char *prompt, FILE *mdb_in,
                    FILE *mdb_out);

extern  MR_SavedDebugState
                MR_saved_debug_state;

// If word is a valid command, return information about the
// completer for the command.

extern  MR_bool MR_trace_command_completion_info(const char *word,
                    MR_MakeCompleter *completer,
                    const char *const **fixed_args);

// A Readline completer for command names.

extern  MR_CompleterList
                *MR_trace_command_completer(const char *word, size_t word_len);

#endif  // MERCURY_TRACE_INTERNAL_H
