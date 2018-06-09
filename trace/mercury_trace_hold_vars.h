// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2005-2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module looks after the debugger's information about the variables
// that have been held the user. Held variables start out as variables
// live at a trace event, and then the act of holding onto them extends their
// lifetimes to the end of the debugger session.

#ifndef MERCURY_TRACE_HOLD_VARS_H
#define MERCURY_TRACE_HOLD_VARS_H

#include "mercury_std.h"        // for MR_bool
#include "mercury_types.h"      // for MR_Word etc

#include <stdio.h>              // for FILE

// Add a new variable with the given name, type and value to the set
// of held variables. Returns true if successful; returning false indicates
// that a held variable with that name already exists.

extern  MR_bool     MR_add_hold_var(const char *name,
                        const MR_TypeInfo typeinfo, MR_Word value);

// Search for a held variable with the given name. If successful, return true
// and fill in *typeinfo and *name, otherwise return false.

extern  MR_bool     MR_lookup_hold_var(const char *name,
                        MR_TypeInfo *typeinfo, MR_Word *value);

// Print a list of the extant held variables to the specified file.

extern  void        MR_trace_list_held_vars(FILE *fp);

#endif  // MERCURY_TRACE_HOLD_VARS_H
