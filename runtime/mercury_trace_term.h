// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2005-2007 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_trace_term.h declares the runtime's facilities to manipulate terms.
// We use this capability to implement conditional breakpoints.

#include <stdio.h>

#ifndef MERCURY_TRACE_TERM_H
#define MERCURY_TRACE_TERM_H

#include "mercury_std.h"

typedef struct MR_CTerm_Struct      *MR_CTerm;
typedef struct MR_CArgs_Struct      *MR_CArgs;

typedef struct MR_FlatTerm_Struct   *MR_FlatTerm;
typedef struct MR_FlatArgs_Struct   *MR_FlatArgs;

struct MR_CTerm_Struct {
    char        *MR_term_functor;
    MR_CArgs    MR_term_args;
};

struct MR_CArgs_Struct {
    MR_CTerm    MR_args_head;
    MR_CArgs    MR_args_tail;
};

struct MR_FlatTerm_Struct {
    char        *MR_flat_term_functor;
    MR_FlatArgs MR_flat_term_args;
};

struct MR_FlatArgs_Struct {
    char        *MR_flat_args_head;
    MR_FlatArgs MR_flat_args_tail;
};

// Read a term from the string starting at str.
//
// If successful,
//
// - the return value will be non-NULL,
// - *rest will point to the first character after the term,
// - *mismatch and *error will not be meaningful.
//
// If unsuccessful (due to a syntax error),
//
// - the return value will be NULL,
// - *rest will not be meaningful,
// - *mismatch will be MR_TRUE if the syntax error is due a missing
//   close bracket at the end of a list, a missing double quote character
//   at the end of a string, or a missing single quote character at the end
//   of a quoted function symbol name, and MR_FALSE otherwise;
// - *error will point to the unmatched character if *mismatch is MR_TRUE,
//   and to the site of the syntax error otherwise.

extern  MR_CTerm    MR_create_cterm(char *str, char **rest,
                        MR_bool *mismatch, char **error);

// Print this term to the given file.

extern  void        MR_print_cterm(FILE *fp, MR_CTerm term);

// Release the memory taken by the structures inside term. The memory taken
// by the strings representing function symbols are not released; their freeing
// is up to the caller.

extern  void        MR_delete_cterm(MR_CTerm term);

#endif // MERCURY_TRACE_TERM_H
