// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-2007 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module implements the mdb commands in the "help" category.
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

#include "mercury_array_macros.h"
#include "mercury_trace_internal.h"
#include "mercury_trace_cmds.h"
#include "mercury_trace_cmd_help.h"
#include "mercury_trace_help.h"
#include "mercury_trace_util.h"

////////////////////////////////////////////////////////////////////////////

// The initial number of lines in documentation entries.
#define MR_INIT_DOC_CHARS   800

static  const char  *MR_trace_read_help_text(void);

////////////////////////////////////////////////////////////////////////////

MR_Next
MR_trace_cmd_document_category(char **words, int word_count,
    MR_TraceCmdInfo *cmd, MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    int         slot;
    const char  *msg;
    const char  *help_text;

    help_text = MR_trace_read_help_text();
    if (word_count != 3) {
        MR_trace_usage_cur_cmd();
    } else if (! MR_trace_is_nonneg_int(words[1], &slot)) {
        MR_trace_usage_cur_cmd();
    } else {
        msg = MR_trace_add_cat(words[2], slot, help_text);
        if (msg != NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err,
                "Document category `%s' not added: %s.\n", words[2], msg);
        }
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_document(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    int         slot;
    const char  *msg;
    const char  *help_text;

    help_text = MR_trace_read_help_text();
    if (word_count != 4) {
        MR_trace_usage_cur_cmd();
    } else if (! MR_trace_is_nonneg_int(words[2], &slot)) {
        MR_trace_usage_cur_cmd();
    } else {
        msg = MR_trace_add_item(words[1], words[3], slot, help_text);
        if (msg != NULL) {
            fflush(MR_mdb_out);
            fprintf(MR_mdb_err,
                "Document item `%s' in category `%s' not added: %s.\n",
                words[3], words[1], msg);
        }
    }

    return KEEP_INTERACTING;
}

MR_Next
MR_trace_cmd_help(char **words, int word_count, MR_TraceCmdInfo *cmd,
    MR_EventInfo *event_info, MR_Code **jumpaddr)
{
    if (word_count == 1) {
        MR_trace_help();
    } else if (word_count == 2) {
        MR_trace_help_word(words[1]);
    } else if (word_count == 3) {
        MR_trace_help_cat_item(words[1], words[2]);
    } else {
        MR_trace_usage_cur_cmd();
    }

    return KEEP_INTERACTING;
}

////////////////////////////////////////////////////////////////////////////

// Read lines until we find one that contains only "end".
// Return the lines concatenated together.
// The memory returned is allocated with MR_malloc();
// it is the caller's responsibility to MR_free() it when appropriate.

static const char *
MR_trace_read_help_text(void)
{
    char    *text;
    char    *doc_chars = NULL;
    int     doc_char_max = 0;
    int     next_char_slot;
    size_t  line_len;
    int     i;

    next_char_slot = 0;
    while ((text = MR_trace_getline("cat> ", MR_mdb_in, MR_mdb_out)) != NULL) {
        if (MR_streq(text, "end")) {
            MR_free(text);
            break;
        }

        line_len = strlen(text);
        MR_ensure_big_enough(next_char_slot + line_len + 2, doc_char, char,
            MR_INIT_DOC_CHARS);
        for (i = 0; i < line_len; i++) {
            doc_chars[next_char_slot + i] = text[i];
        }

        next_char_slot += line_len;
        doc_chars[next_char_slot] = '\n';
        next_char_slot += 1;
        MR_free(text);
    }

    MR_ensure_big_enough(next_char_slot, doc_char, char, MR_INIT_DOC_CHARS);
    doc_chars[next_char_slot] = '\0';
    return doc_chars;
}
