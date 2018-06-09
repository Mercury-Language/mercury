// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998,2002, 2005-2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_trace_help.h
//
// Defines the interface of the help system for the internal debugger.

#ifndef MERCURY_TRACE_HELP_H
#define MERCURY_TRACE_HELP_H

#include "mercury_trace_completion.h"

// This global keeps a reference to the help system.

extern  MR_Word             MR_trace_help_system;

// These function add a help node, which must a category or an item
// within a category. It returns NULL if the addition was successful,
// and a pointer to an error message otherwise.

extern  const char          *MR_trace_add_cat(const char *category, int slot,
                                const char *text);

extern  const char          *MR_trace_add_item(const char *category,
                                const char *item, int slot, const char *text);

// These functions print help to standard output.
//
// MR_trace_help prints a list of the top-level help nodes.
// MR_trace_help_word prints the text of all the help nodes with the given
//  name. If there are none, it prints a list of the top-level help nodes.
// MR_trace_help_cat_item prints the text of the node at path cat/item.

extern  void                MR_trace_help(void);
extern  void                MR_trace_help_word(const char *word);

extern  void                MR_trace_help_cat_item(const char *cat,
                                const char *item);

// A Readline completer for help topics.
extern  MR_CompleterList    *MR_trace_help_completer(const char *word,
                                size_t word_len);

#endif  // MERCURY_TRACE_HELP_H
