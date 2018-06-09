// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2008 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module looks after a queue of lines containing commands for mdb.
// Its main use is during initialization.
//
// Main author: Zoltan Somogyi.

#include "mercury_imp.h"
#include "mercury_trace_command_queue.h"
#include "mercury_trace_internal.h" // for MR_trace_echo_queue_commands

// MR_cmd_queue_head points to the first node in the list, while
// MR_cmd_queue_tail points to the last. It is an invariant that
// if one of these two variables is NULL, the other is NULL as well.

static  MR_CmdLines *MR_cmd_queue_head = NULL;
static  MR_CmdLines *MR_cmd_queue_tail = NULL;

void
MR_insert_command_line_at_head(const char *line_contents)
{
    MR_CmdLines *cmd_line;

    cmd_line = MR_NEW(MR_CmdLines);
    cmd_line->MR_cmd_line_contents = MR_copy_string(line_contents);
    cmd_line->MR_cmd_line_next = MR_cmd_queue_head;

    MR_cmd_queue_head = cmd_line;
    if (MR_cmd_queue_tail == NULL) {
        MR_cmd_queue_tail = MR_cmd_queue_head;
    }
}

void
MR_insert_command_line_at_tail(const char *line_contents)
{
    MR_CmdLines *cmd_line;

    cmd_line = MR_NEW(MR_CmdLines);
    cmd_line->MR_cmd_line_contents = MR_copy_string(line_contents);
    cmd_line->MR_cmd_line_next = NULL;

    if (MR_cmd_queue_tail == NULL) {
        MR_cmd_queue_head = cmd_line;
        MR_cmd_queue_tail = cmd_line;
    } else {
        MR_cmd_queue_tail->MR_cmd_line_next = cmd_line;
        MR_cmd_queue_tail = cmd_line;
    }
}

void
MR_insert_command_lines_at_head(MR_CmdLines *new_lines)
{
    MR_CmdLines *last_new_node;

    if (new_lines == NULL) {
        return;
    }

    for (last_new_node = new_lines;
        last_new_node->MR_cmd_line_next != NULL;
        last_new_node = last_new_node->MR_cmd_line_next)
    {
        // Do nothing.
    }

    MR_assert(last_new_node->MR_cmd_line_next == NULL);
    last_new_node->MR_cmd_line_next = MR_cmd_queue_head;

    MR_cmd_queue_head = new_lines;
    if (MR_cmd_queue_tail == NULL) {
        MR_cmd_queue_tail = last_new_node;
    }
}

void
MR_insert_command_lines_at_tail(MR_CmdLines *new_lines)
{
    MR_CmdLines *last_new_node;

    if (new_lines == NULL) {
        return;
    }

    for (last_new_node = new_lines;
        last_new_node->MR_cmd_line_next != NULL;
        last_new_node = last_new_node->MR_cmd_line_next)
    {
        // Do nothing.
    }

    MR_assert(last_new_node->MR_cmd_line_next == NULL);

    if (MR_cmd_queue_tail == NULL) {
        MR_cmd_queue_head = new_lines;
        MR_cmd_queue_tail = last_new_node;
    } else {
        MR_cmd_queue_tail->MR_cmd_line_next = new_lines;
        MR_cmd_queue_tail = last_new_node;
    }
}

char *
MR_trace_getline_command_queue(void)
{
    if (MR_cmd_queue_head != NULL) {
        MR_CmdLines *old;
        char        *line_contents;

        old = MR_cmd_queue_head;
        line_contents = MR_cmd_queue_head->MR_cmd_line_contents;
        MR_cmd_queue_head = MR_cmd_queue_head->MR_cmd_line_next;

        if (MR_cmd_queue_head == NULL) {
            MR_cmd_queue_tail = NULL;
        }

        if (MR_trace_echo_queue_commands) {
            printf("queue command <%s>\n", line_contents);
            fflush(stdout);
        }

        MR_free(old);
        return line_contents;
    } else {
        return NULL;
    }
}
