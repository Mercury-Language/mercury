// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2002, 2004 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

// The internals of hand-written unification and comparison routines.
//
// The code that includes this file should define the following macros:
//
// module
// type
// arity
// unify_code
// compare_code

#define proc_label  MR_proc_entry_uci_name(module, __Unify__,           \
                        type, arity, 0)
#define proc_layout MR_proc_layout_uci_name(module, __Unify__,          \
                        type, arity, 0)
#define body_code   do { unify_code } while (0)

#include    "mercury_hand_unify_body.h"

#undef  proc_label
#undef  proc_layout
#undef  body_code

#define proc_label  MR_proc_entry_uci_name(module, __Compare__,         \
                        type, arity, 0)
#define proc_layout MR_proc_layout_uci_name(module, __Compare__,        \
                        type, arity, 0)
#define body_code   do { compare_code } while (0)

#include    "mercury_hand_compare_body.h"

#undef  proc_label
#undef  proc_layout
#undef  body_code

#define proc_label  MR_proc_entry_uci_name(module, __CompareRep__,      \
                        type, arity, 0)
#define proc_layout MR_proc_layout_uci_name(module, __CompareRep__,     \
                        type, arity, 0)
#define body_code   do { MR_fatal_error(                                \
                "direct type-specific compare_rep call");               \
            } while (0)

#include    "mercury_hand_compare_body.h"

#undef  proc_label
#undef  proc_layout
#undef  body_code
