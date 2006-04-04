/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 1998-2002, 2005-2006 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef MERCURY_TRACE_INTERNAL_H
#define MERCURY_TRACE_INTERNAL_H

#include "mercury_types.h"              /* for MR_Code */
#include "mercury_trace_base.h"         /* for MR_SavedDebugState */

#include "mercury_trace.h"              /* for MR_Event_Info, etc. */
#include "mercury_std.h"                /* for MR_bool */
#include "mercury_trace_cmds.h"         /* for MR_Spy_Print_List */
#include "mercury_trace_completion.h"   /* for MR_Make_Completer */
#include "mercury_trace_spy.h"          /* for MR_Spy_Print_List */
#include "mercury_trace_source.h"       /* for MR_Trace_Source_Server */

#include <stdio.h>                      /* for FILE */

extern  MR_Code     *MR_trace_event_internal(MR_Trace_Cmd_Info *cmd,
                        MR_bool interactive, MR_Spy_Print_List print_list,
                        MR_Event_Info *event_info);

extern  void        MR_trace_event_print_internal_report(
                        MR_Event_Info *event_info);


/*
** Debugger I/O streams.
** Replacements for stdin/stdout/stderr respectively.
**
** The distinction between MR_mdb_out and MR_mdb_err is analagous to
** the distinction between stdout and stderr: ordinary output, including
** information messages about conditions which are not errors, should
** go to MR_mdb_out, but error messages should go to MR_mdb_err.
*/

extern  FILE    *MR_mdb_in;
extern  FILE    *MR_mdb_out;
extern  FILE    *MR_mdb_err;

/* 
** We print confirmation of commands (e.g. new aliases) if this is MR_TRUE.
*/ 

MR_bool         MR_trace_internal_interacting;

/*
** The structure of the input queue, and the operations that work on it.
*/

typedef struct MR_Line_Struct {
    char                    *MR_line_contents;
    struct MR_Line_Struct   *MR_line_next;
} MR_Line;

extern  void        MR_insert_line_at_head(const char *contents);
extern  void        MR_insert_line_at_tail(const char *contents);

/*
** If there any lines waiting in the queue, return the first of these.
** The memory for the line will have been allocated with MR_malloc(),
** and it is the caller's resposibility to MR_free() it when appropriate.
** If there are no lines in the queue, this function returns NULL.
*/
        
extern  char        *MR_trace_getline_queue(void);

/*  
** The details of the source server, if any.
*/

extern  MR_Trace_Source_Server  MR_trace_source_server;

/*
** Source commands from the given file. Print an error message if the file
** cannot be read, unless ignore_errors is true.
*/

extern  MR_bool     MR_trace_source(const char *filename,
                        MR_bool ignore_errors);

/*
** Source commands from the given file.
*/

extern  void        MR_trace_source_from_open_file(FILE *fp);

/*
** Print a usage message for the currently executing command.
*/

extern  void    MR_trace_usage_cur_cmd(void);

/*
** Print a message about this command being a no-op from this port.
*/

extern  void    MR_trace_do_noop(void);

/*
** If the given word is the name of a valid command, return its info.
*/

extern  const MR_Trace_Command_Info
                *MR_trace_valid_command(const char *word);

/*
** Close a source server, if there is one attached.
*/

extern  void    MR_trace_maybe_close_source_window(MR_bool verbose);

/*
** This just prints to MR_mdb_out a message telling the user
** that the debugger caught an interrupt.
*/

extern  void    MR_trace_interrupt_message(void);

extern  char    *MR_trace_getline(const char *prompt, FILE *mdb_in,
                    FILE *mdb_out);
extern  char    *MR_trace_get_command(const char *prompt, FILE *mdb_in,
                    FILE *mdb_out);

extern  MR_SavedDebugState
                MR_saved_debug_state;

/*
** If word is a valid command, return information about the
** completer for the command.
*/

extern  MR_bool MR_trace_command_completion_info(const char *word,
                    MR_Make_Completer *completer,
                    const char *const **fixed_args);

/*
** A Readline completer for command names.
*/

extern  MR_Completer_List
                *MR_trace_command_completer(const char *word, size_t word_len);

#endif  /* MERCURY_TRACE_INTERNAL_H */
