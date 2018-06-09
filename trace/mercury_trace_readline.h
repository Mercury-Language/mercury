// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998-1999, 2006-2007 The University of Melbourne.
// Copyright (C) 2015-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_trace_readline.h: simple interface functions to read a line.
//
// Main authors: fjh, zs.

#ifndef MERCURY_TRACE_READLINE_H
#define MERCURY_TRACE_READLINE_H

#include <stdio.h>  // for FILE

// Print the prompt to the `out' file, read a line from the `in' file,
// possibly using GNU readline for command-line editing, and return it
// in a MR_malloc'd buffer holding the line (without the final newline).
// If EOF occurs on a nonempty line, treat the EOF as a newline; if EOF
// occurs on an empty line, return NULL.

extern  char    *MR_trace_readline(const char *prompt, FILE *in, FILE *out);

// Read a line from a file, and return a pointer to a MR_malloc'd buffer
// holding the line (without the final newline). If EOF occurs on a
// nonempty line, treat the EOF as a newline; if EOF occurs on an empty
// line, return NULL. Don't use GNU readline.

extern  char    *MR_trace_readline_raw(FILE *in);

// Read a line from an mdb script.
// Occurences of the strings "$1" to
// "$9" are replaced with the corresponding values in the args array.
// If there is no value in the args array, then the "$n" string is replaced
// by the empty string.
// Lines beginning with '#' and empty lines are ignored.
// Return a pointer to a MR_malloc'd buffer holding the new string (without
// the final newline). If EOF occurs on a nonempty line, treat the EOF as a
// newline; if EOF occurs on an empty line, return NULL.
// Don't use GNU readline.

extern  char    *MR_trace_readline_from_script(FILE *fp, char **args,
                    int num_args);

#endif  // MERCURY_TRACE_READLINE_H
