// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998,2000-2002, 2006 The University of Melbourne.
// Copyright (C) 2015-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_trace_alias.h
//
// Defines the interface of the alias system for the internal debugger.

#ifndef MERCURY_TRACE_ALIAS_H
#define MERCURY_TRACE_ALIAS_H

#include "mercury_std.h"    // for MR_bool
#include <stdio.h>

#include "mercury_trace_completion.h"

typedef struct {
    char        *MR_alias_name;
    char        **MR_alias_words;
    int         MR_alias_word_count;
} MR_Alias;

// Add an alias with the given name and expansion to the list.
// The name, the words in the expansion and the array of pointers to the
// expansion will all be copied, so their storage can be released
// when MR_trace_add_alias returns.
//
// Overwrites any previous alias with the same name.

extern  void        MR_trace_add_alias(char *name, char **words,
                        int word_count);

// Remove the given alias from the list. Returns MR_FALSE if there is no
// such alias, and MR_TRUE if there was such an alias and the removal was
// successful.

extern  MR_bool     MR_trace_remove_alias(const char *name);

// Looks up whether the given alias exists. If yes, returns MR_TRUE, and
// sets *words_ptr to point to a vector of words forming the alias expansion,
// and *word_count_ptr to the number of words in the expansion. If no,
// returns MR_FALSE.

extern  MR_bool     MR_trace_lookup_alias(const char *name,
                        char ***words_ptr, int *word_count_ptr);

// Print the alias of the given name, if it exists, and an error message
// if it does not.

extern  void        MR_trace_print_alias(FILE *fp, const char *name);

// Print all the aliases to the given file. If mdb_command_format is MR_TRUE,
// print them in a form that, when sourced from mdb , recreate the aliases.
// Otherwise, print the aliases in a format that is nice for humans to read.

extern  void        MR_trace_print_all_aliases(FILE *fp,
                        MR_bool mdb_command_format);

// If the main command in *words[0] MR_trace_expand_aliases is an alias, then
// expand the alias. Words, word_max and word_count should form a resizeable
// array of words, in the sense of mercury_array_macros.h.

extern  void        MR_trace_expand_aliases(char ***words, int *word_max,
                        int *word_count);

// A Readline completer for aliases.
extern  MR_CompleterList
                    *MR_trace_alias_completer(const char *word,
                        size_t word_length);

#endif  // MERCURY_TRACE_ALIAS_H
