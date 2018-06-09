// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2008 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#ifndef MERCURY_TRACE_COMMAND_QUEUE_H
#define MERCURY_TRACE_COMMAND_QUEUE_H

// The structure of the queue of command lines, and the operations
// that work on it.
//
// The contents of each command line should be allocated with MR_malloc().

typedef struct MR_CmdLines_Struct   MR_CmdLines;

struct MR_CmdLines_Struct {
    char            *MR_cmd_line_contents;
    MR_CmdLines     *MR_cmd_line_next;
};

extern  void        MR_insert_command_line_at_head(const char *line);
extern  void        MR_insert_command_line_at_tail(const char *line);

extern  void        MR_insert_command_lines_at_head(MR_CmdLines *lines);
extern  void        MR_insert_command_lines_at_tail(MR_CmdLines *lines);

// If there any lines waiting in the queue, return the first of these.
// The memory for the line will have been allocated with MR_malloc(),
// and it is the caller's responsibility to MR_free() it when appropriate.
// If there are no lines in the queue, this function returns NULL.

extern  char        *MR_trace_getline_command_queue(void);

#endif  // MERCURY_TRACE_COMMAND_QUEUE_H
