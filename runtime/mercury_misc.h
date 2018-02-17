// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1995-2000,2002, 2004, 2006, 2009-2011 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

// mercury_misc.h - MR_warning(),
//                  MR_fatal_error()

#ifndef MERCURY_MISC_H
#define MERCURY_MISC_H

#include "mercury_std.h"    // for `MR_NO_RETURN'
#include <stdlib.h>         // for `size_t'

extern void     MR_warning(const char *msg, ...);

// For warnings from the debugger.

extern void     MR_mdb_warning(const char *msg, ...);

extern void     MR_perror(const char *msg);

// For errors from the debugger.

extern void     MR_mdb_perror(const char *msg);

// Output a message to standard error and abort.
// This function is for fatal errors in the Mercury runtime.

MR_NO_RETURN(extern void MR_fatal_error(const char *msg, ...));

// Output a message to standard error and abort.
// Prefix the message with the location of the component that caused the
// error.
// This is intended to be called from library bindings etc.

MR_NO_RETURN(extern void MR_external_fatal_error(const char *locn,
                    const char *msg, ...));

// Register a function to be called (as func(data)) when the program is
// about to be terminated due to an uncaught exception.

extern void     MR_register_exception_cleanup(void (*func)(void *),
                    void *data);

// Call all the functions registered with MR_register_exception_cleanup.
// Should be invoked only when the program is about to be terminated
// due to an uncaught exception.

extern void     MR_perform_registered_exception_cleanups(void);

////////////////////////////////////////////////////////////////////////////

// These macros are shorthands to allow reductions in the size of compiler
// generated C source files.

#define MR_COMMON_TYPE(typenum)                                         \
    MR_PASTE2(mercury_type_, typenum)

#define MR_COMMON_NAME(typenum)                                         \
    MR_PASTE2(mercury_common_, typenum)

#define MR_COMMON(typenum, cellnum)                                     \
    ((MR_Word *) &MR_COMMON_NAME(typenum)[cellnum])

#define MR_TAG_COMMON(tag, typenum, cellnum)                            \
    (MR_mkword(MR_mktag(tag), MR_COMMON(typenum, cellnum)))

////////////////////////////////////////////////////////////////////////////

#define MR_pseudo_type_infos(m)                                         \
    MR_PASTE2(mercury_data__pseudo_type_info_array__, m)

#define MR_hlds_var_nums(m)                                             \
    MR_PASTE2(mercury_data__hlds_var_nums_array__, m)

#define MR_short_locns(m)                                               \
    MR_PASTE2(mercury_data__short_locns_array__, m)

#define MR_long_locns(m)                                                \
    MR_PASTE2(mercury_data__long_locns_array__, m)

#define MR_user_event_var_nums(m)                                       \
    MR_PASTE2(mercury_data__user_event_var_nums_array__, m)

#define MR_user_event_layouts(m)                                        \
    MR_PASTE2(mercury_data__user_event_layouts_array__, m)

#define MR_no_var_label_layouts(m)                                      \
    MR_PASTE2(mercury_data__no_var_label_layout_array__, m)

#define MR_svar_label_layouts(m)                                        \
    MR_PASTE2(mercury_data__svar_label_layout_array__, m)

#define MR_lvar_label_layouts(m)                                        \
    MR_PASTE2(mercury_data__lvar_label_layout_array__, m)

////////////////////////////////////////////////////////////////////////////

#define MR_proc_call_sites(m)                                           \
    MR_PASTE2(mercury_data__proc_call_sites_array__, m)

#define MR_proc_cp_statics(m)                                           \
    MR_PASTE2(mercury_data__proc_cp_statics_array__, m)

#define MR_proc_cp_dynamics(m)                                          \
    MR_PASTE2(mercury_data__proc_cp_dynamics_array__, m)

#define MR_proc_statics(m)                                              \
    MR_PASTE2(mercury_data__proc_statics_array__, m)

#define MR_proc_head_var_nums(m)                                        \
    MR_PASTE2(mercury_data__proc_head_var_nums_array__, m)

#define MR_proc_var_names(m)                                            \
    MR_PASTE2(mercury_data__proc_var_names_array__, m)

#define MR_proc_body_bytecodes(m)                                       \
    MR_PASTE2(mercury_data__proc_body_bytecodes_array__, m)

#define MR_proc_table_io_entries(m)                                     \
    MR_PASTE2(mercury_data__proc_table_io_entries_array__, m)

#define MR_proc_event_layouts(m)                                        \
    MR_PASTE2(mercury_data__proc_event_layouts_array__, m)

#define MR_proc_exec_traces(m)                                          \
    MR_PASTE2(mercury_data__proc_exec_traces_array__, m)

#define MR_threadscope_strings(m)                                       \
    MR_PASTE2(mercury_data__threadscope_string_table_array__, m)

#define MR_alloc_sites(m)                                               \
    MR_PASTE2(mercury_data__alloc_sites_array__, m)

////////////////////////////////////////////////////////////////////////////

#define MR_no_var_label_layout_refs1(m, s1)                             \
    &MR_no_var_label_layouts(m)[s1],

#define MR_no_var_label_layout_refs2(m, s1, s2)                         \
    MR_no_var_label_layout_refs1(m, s1)                                 \
    MR_no_var_label_layout_refs1(m, s2)

#define MR_no_var_label_layout_refs3(m, s1, s2, s3)                     \
    MR_no_var_label_layout_refs1(m, s1)                                 \
    MR_no_var_label_layout_refs1(m, s2)                                 \
    MR_no_var_label_layout_refs1(m, s3)

#define MR_no_var_label_layout_refs4(m, s1, s2, s3, s4)                 \
    MR_no_var_label_layout_refs1(m, s1)                                 \
    MR_no_var_label_layout_refs1(m, s2)                                 \
    MR_no_var_label_layout_refs1(m, s3)                                 \
    MR_no_var_label_layout_refs1(m, s4)

#define MR_no_var_label_layout_refs5(m, s1, s2, s3, s4, s5)             \
    MR_no_var_label_layout_refs1(m, s1)                                 \
    MR_no_var_label_layout_refs1(m, s2)                                 \
    MR_no_var_label_layout_refs1(m, s3)                                 \
    MR_no_var_label_layout_refs1(m, s4)                                 \
    MR_no_var_label_layout_refs1(m, s5)

#define MR_no_var_label_layout_refs6(m, s1, s2, s3, s4, s5, s6)         \
    MR_no_var_label_layout_refs1(m, s1)                                 \
    MR_no_var_label_layout_refs1(m, s2)                                 \
    MR_no_var_label_layout_refs1(m, s3)                                 \
    MR_no_var_label_layout_refs1(m, s4)                                 \
    MR_no_var_label_layout_refs1(m, s5)                                 \
    MR_no_var_label_layout_refs1(m, s6)

#define MR_no_var_label_layout_refs7(m, s1, s2, s3, s4, s5, s6, s7)     \
    MR_no_var_label_layout_refs1(m, s1)                                 \
    MR_no_var_label_layout_refs1(m, s2)                                 \
    MR_no_var_label_layout_refs1(m, s3)                                 \
    MR_no_var_label_layout_refs1(m, s4)                                 \
    MR_no_var_label_layout_refs1(m, s5)                                 \
    MR_no_var_label_layout_refs1(m, s6)                                 \
    MR_no_var_label_layout_refs1(m, s7)

#define MR_no_var_label_layout_refs8(m, s1, s2, s3, s4, s5, s6, s7, s8) \
    MR_no_var_label_layout_refs1(m, s1)                                 \
    MR_no_var_label_layout_refs1(m, s2)                                 \
    MR_no_var_label_layout_refs1(m, s3)                                 \
    MR_no_var_label_layout_refs1(m, s4)                                 \
    MR_no_var_label_layout_refs1(m, s5)                                 \
    MR_no_var_label_layout_refs1(m, s6)                                 \
    MR_no_var_label_layout_refs1(m, s7)                                 \
    MR_no_var_label_layout_refs1(m, s8)

#define MR_no_var_label_layout_refs9(m, s1, s2, s3, s4, s5, s6, s7, s8, s9) \
    MR_no_var_label_layout_refs1(m, s1)                                 \
    MR_no_var_label_layout_refs1(m, s2)                                 \
    MR_no_var_label_layout_refs1(m, s3)                                 \
    MR_no_var_label_layout_refs1(m, s4)                                 \
    MR_no_var_label_layout_refs1(m, s5)                                 \
    MR_no_var_label_layout_refs1(m, s6)                                 \
    MR_no_var_label_layout_refs1(m, s7)                                 \
    MR_no_var_label_layout_refs1(m, s8)                                 \
    MR_no_var_label_layout_refs1(m, s9)

#define MR_no_var_label_layout_refs10(m, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10) \
    MR_no_var_label_layout_refs1(m, s1)                                 \
    MR_no_var_label_layout_refs1(m, s2)                                 \
    MR_no_var_label_layout_refs1(m, s3)                                 \
    MR_no_var_label_layout_refs1(m, s4)                                 \
    MR_no_var_label_layout_refs1(m, s5)                                 \
    MR_no_var_label_layout_refs1(m, s6)                                 \
    MR_no_var_label_layout_refs1(m, s7)                                 \
    MR_no_var_label_layout_refs1(m, s8)                                 \
    MR_no_var_label_layout_refs1(m, s9)                                 \
    MR_no_var_label_layout_refs1(m, s10)

#define MR_svar_label_layout_refs1(m, s1)                               \
    (MR_LabelLayout *) &MR_svar_label_layouts(m)[s1],

#define MR_svar_label_layout_refs2(m, s1, s2)                           \
    MR_svar_label_layout_refs1(m, s1)                                   \
    MR_svar_label_layout_refs1(m, s2)

#define MR_svar_label_layout_refs3(m, s1, s2, s3)                       \
    MR_svar_label_layout_refs1(m, s1)                                   \
    MR_svar_label_layout_refs1(m, s2)                                   \
    MR_svar_label_layout_refs1(m, s3)

#define MR_svar_label_layout_refs4(m, s1, s2, s3, s4)                   \
    MR_svar_label_layout_refs1(m, s1)                                   \
    MR_svar_label_layout_refs1(m, s2)                                   \
    MR_svar_label_layout_refs1(m, s3)                                   \
    MR_svar_label_layout_refs1(m, s4)

#define MR_svar_label_layout_refs5(m, s1, s2, s3, s4, s5)               \
    MR_svar_label_layout_refs1(m, s1)                                   \
    MR_svar_label_layout_refs1(m, s2)                                   \
    MR_svar_label_layout_refs1(m, s3)                                   \
    MR_svar_label_layout_refs1(m, s4)                                   \
    MR_svar_label_layout_refs1(m, s5)

#define MR_svar_label_layout_refs6(m, s1, s2, s3, s4, s5, s6)           \
    MR_svar_label_layout_refs1(m, s1)                                   \
    MR_svar_label_layout_refs1(m, s2)                                   \
    MR_svar_label_layout_refs1(m, s3)                                   \
    MR_svar_label_layout_refs1(m, s4)                                   \
    MR_svar_label_layout_refs1(m, s5)                                   \
    MR_svar_label_layout_refs1(m, s6)

#define MR_svar_label_layout_refs7(m, s1, s2, s3, s4, s5, s6, s7)       \
    MR_svar_label_layout_refs1(m, s1)                                   \
    MR_svar_label_layout_refs1(m, s2)                                   \
    MR_svar_label_layout_refs1(m, s3)                                   \
    MR_svar_label_layout_refs1(m, s4)                                   \
    MR_svar_label_layout_refs1(m, s5)                                   \
    MR_svar_label_layout_refs1(m, s6)                                   \
    MR_svar_label_layout_refs1(m, s7)

#define MR_svar_label_layout_refs8(m, s1, s2, s3, s4, s5, s6, s7, s8)   \
    MR_svar_label_layout_refs1(m, s1)                                   \
    MR_svar_label_layout_refs1(m, s2)                                   \
    MR_svar_label_layout_refs1(m, s3)                                   \
    MR_svar_label_layout_refs1(m, s4)                                   \
    MR_svar_label_layout_refs1(m, s5)                                   \
    MR_svar_label_layout_refs1(m, s6)                                   \
    MR_svar_label_layout_refs1(m, s7)                                   \
    MR_svar_label_layout_refs1(m, s8)

#define MR_svar_label_layout_refs9(m, s1, s2, s3, s4, s5, s6, s7, s8, s9) \
    MR_svar_label_layout_refs1(m, s1)                                   \
    MR_svar_label_layout_refs1(m, s2)                                   \
    MR_svar_label_layout_refs1(m, s3)                                   \
    MR_svar_label_layout_refs1(m, s4)                                   \
    MR_svar_label_layout_refs1(m, s5)                                   \
    MR_svar_label_layout_refs1(m, s6)                                   \
    MR_svar_label_layout_refs1(m, s7)                                   \
    MR_svar_label_layout_refs1(m, s8)                                   \
    MR_svar_label_layout_refs1(m, s9)

#define MR_svar_label_layout_refs10(m, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10) \
    MR_svar_label_layout_refs1(m, s1)                                   \
    MR_svar_label_layout_refs1(m, s2)                                   \
    MR_svar_label_layout_refs1(m, s3)                                   \
    MR_svar_label_layout_refs1(m, s4)                                   \
    MR_svar_label_layout_refs1(m, s5)                                   \
    MR_svar_label_layout_refs1(m, s6)                                   \
    MR_svar_label_layout_refs1(m, s7)                                   \
    MR_svar_label_layout_refs1(m, s8)                                   \
    MR_svar_label_layout_refs1(m, s9)                                           \
    MR_svar_label_layout_refs1(m, s10)

#define MR_lvar_label_layout_refs1(m, s1)                               \
    &MR_lvar_label_layouts(m)[s1],

#define MR_lvar_label_layout_refs2(m, s1, s2)                           \
    MR_lvar_label_layout_refs1(m, s1)                                   \
    MR_lvar_label_layout_refs1(m, s2)

#define MR_lvar_label_layout_refs3(m, s1, s2, s3)                       \
    MR_lvar_label_layout_refs1(m, s1)                                   \
    MR_lvar_label_layout_refs1(m, s2)                                   \
    MR_lvar_label_layout_refs1(m, s3)

#define MR_lvar_label_layout_refs4(m, s1, s2, s3, s4)                   \
    MR_lvar_label_layout_refs1(m, s1)                                   \
    MR_lvar_label_layout_refs1(m, s2)                                   \
    MR_lvar_label_layout_refs1(m, s3)                                   \
    MR_lvar_label_layout_refs1(m, s4)

#define MR_lvar_label_layout_refs5(m, s1, s2, s3, s4, s5)               \
    MR_lvar_label_layout_refs1(m, s1)                                   \
    MR_lvar_label_layout_refs1(m, s2)                                   \
    MR_lvar_label_layout_refs1(m, s3)                                   \
    MR_lvar_label_layout_refs1(m, s4)                                   \
    MR_lvar_label_layout_refs1(m, s5)

#define MR_lvar_label_layout_refs6(m, s1, s2, s3, s4, s5, s6)           \
    MR_lvar_label_layout_refs1(m, s1)                                   \
    MR_lvar_label_layout_refs1(m, s2)                                   \
    MR_lvar_label_layout_refs1(m, s3)                                   \
    MR_lvar_label_layout_refs1(m, s4)                                   \
    MR_lvar_label_layout_refs1(m, s5)                                   \
    MR_lvar_label_layout_refs1(m, s6)

#define MR_lvar_label_layout_refs7(m, s1, s2, s3, s4, s5, s6, s7)       \
    MR_lvar_label_layout_refs1(m, s1)                                   \
    MR_lvar_label_layout_refs1(m, s2)                                   \
    MR_lvar_label_layout_refs1(m, s3)                                   \
    MR_lvar_label_layout_refs1(m, s4)                                   \
    MR_lvar_label_layout_refs1(m, s5)                                   \
    MR_lvar_label_layout_refs1(m, s6)                                   \
    MR_lvar_label_layout_refs1(m, s7)

#define MR_lvar_label_layout_refs8(m, s1, s2, s3, s4, s5, s6, s7, s8)   \
    MR_lvar_label_layout_refs1(m, s1)                                   \
    MR_lvar_label_layout_refs1(m, s2)                                   \
    MR_lvar_label_layout_refs1(m, s3)                                   \
    MR_lvar_label_layout_refs1(m, s4)                                   \
    MR_lvar_label_layout_refs1(m, s5)                                   \
    MR_lvar_label_layout_refs1(m, s6)                                   \
    MR_lvar_label_layout_refs1(m, s7)                                   \
    MR_lvar_label_layout_refs1(m, s8)

#define MR_lvar_label_layout_refs9(m, s1, s2, s3, s4, s5, s6, s7, s8, s9) \
    MR_lvar_label_layout_refs1(m, s1)                                   \
    MR_lvar_label_layout_refs1(m, s2)                                   \
    MR_lvar_label_layout_refs1(m, s3)                                   \
    MR_lvar_label_layout_refs1(m, s4)                                   \
    MR_lvar_label_layout_refs1(m, s5)                                   \
    MR_lvar_label_layout_refs1(m, s6)                                   \
    MR_lvar_label_layout_refs1(m, s7)                                   \
    MR_lvar_label_layout_refs1(m, s8)                                   \
    MR_lvar_label_layout_refs1(m, s9)

#define MR_lvar_label_layout_refs10(m, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10) \
    MR_lvar_label_layout_refs1(m, s1)                                   \
    MR_lvar_label_layout_refs1(m, s2)                                   \
    MR_lvar_label_layout_refs1(m, s3)                                   \
    MR_lvar_label_layout_refs1(m, s4)                                   \
    MR_lvar_label_layout_refs1(m, s5)                                   \
    MR_lvar_label_layout_refs1(m, s6)                                   \
    MR_lvar_label_layout_refs1(m, s7)                                   \
    MR_lvar_label_layout_refs1(m, s8)                                   \
    MR_lvar_label_layout_refs1(m, s9)                                   \
    MR_lvar_label_layout_refs1(m, s10)

#endif // not MERCURY_MISC_H
