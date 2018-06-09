// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1993-2006, 2010 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_init.h - this file declares stuff defined in the
// automatically generated *_init.c files. This is also the interface
// used by C code that wishes to interface to Mercury.
//
// It also declares some stuff that is used in the automatically
// generate *_init.c files.

#ifndef MERCURY_INIT_H
#define MERCURY_INIT_H

// The following must come before any definitions of global variables.
// This is necessary to support DLLs on Windows.

#include "mercury_conf.h" // for MR_USE_DLLS
#if MR_USE_DLLS
  #include "libmer_rt_dll.h"
#endif

////////////////////////////////////////////////////////////////////////////
// This part is the interface that should be used by C programs that wish
// to interface to Mercury.

// mercury_main() is defined in the <module>_init.c file.
// It calls mercury_init(), mercury_call_main(), and then mercury_terminate().

extern  int     mercury_main(int argc, char **argv);

// mercury_init() is defined in the <module>_init.c file.
//
// The `argc' and `argv' parameters are as for main() in C.
// The `stack_bottom' parameter should be the address of a (word-aligned)
// variable on the C stack. The conservative garbage collector treats that
// address as the start of the stack, so anything older than that
// address won't get scanned; don't store pointers to GC'ed memory
// in local variables that are older than that.
//
// mercury_init() just does some stuff to initialize the garbage
// collector, sets some global variables, and then calls
// mercury_runtime_init().

extern  void    mercury_init(int argc, char **argv, void *stack_bottom);

// mercury_call_main() is defined in the <module>_init.c file.
// It just calls mercury_runtime_main(), which calls main/2
// in the Mercury program.

extern  void    mercury_call_main(void);

// mercury_terminate() is defined in the <module>_init.c file.
// It just calls mercury_runtime_terminate(), which performs
// any necessary cleanup, and then returns the appropriate
// exit status as set by io__set_exit_status.

extern  int     mercury_terminate(void);

// mercury_init_grade_check is defined in the <module>_init.c file.
// It is used to ensure that everything gets compiled in the same grade.
// See mercury_grade.h for details.

extern const char *mercury_init_grade_check(void);

////////////////////////////////////////////////////////////////////////////

// This part defines things which are used by the automatically
// generated *_init.c file. These should not be used (directly)
// by C programs that wish to interface to Mercury.

#include "mercury_regs.h"       // must come before system headers
#include "mercury_goto.h"       // for MR_declare_entry
#include "mercury_types.h"      // for MR_Word etc
#include "mercury_wrapper.h"    // for MR_do_init_modules,
                                // mercury_runtime_init(),
                                // mercury_runtime_main(),
                                // mercury_runtime_terminate(), etc.

#include "mercury_trace_base.h" // for MR_trace_port
#include "mercury_type_info.h"  // for MR_TypeCtorInfo_Struct
#include "mercury_library_types.h"  // for MercuryFilePtr
#include "mercury_complexity.h" // for MR_ComplexityProc

#ifdef MR_CONSERVATIVE_GC
  #ifdef MR_BOEHM_GC
    #define GC_I_HIDE_POINTERS
    #include "gc.h"             // for GC_INIT(), GC_stack_bottom
  #endif
  #ifdef MR_HGC
    #include "mercury_hgc.h"    // for mercury_hgc_init() and
                                // MR_hgc_set_stack_bot()
  #endif
#endif

#ifdef MR_HIGHLEVEL_CODE
  #include "mercury.h"
#endif

// mercury_main() takes the address of the following predicates/functions,
// which are defined elsewhere.
//
// These declarations duplicate some of the contents of the automatically
// generated header files for some of the library modules, and therefore
// represent a potential double maintenance problem. At the moment we
// accept this because it avoids having the runtime rely on the library.

// in library/io.mh
extern  void    mercury_init_io(void);
extern  void    ML_std_library_init(void);
extern  void    ML_std_library_finalize(void);
extern  void    ML_io_stderr_stream(MercuryFilePtr *);
extern  void    ML_io_stdout_stream(MercuryFilePtr *);
extern  void    ML_io_stdin_stream(MercuryFilePtr *);

extern  void    ML_io_print_to_stream(MR_Word, MercuryFilePtr, MR_Word);

// in library/private_builtin.m
extern  const MR_TypeCtorInfo       ML_type_ctor_info_for_univ;
extern  const MR_TypeCtorInfo       ML_type_info_for_type_info;
extern  const MR_TypeCtorInfo       ML_type_info_for_pseudo_type_info;
extern  const MR_FA_TypeInfo_Struct1    ML_type_info_for_list_of_univ;
extern  const MR_FA_TypeInfo_Struct1    ML_type_info_for_list_of_int;
extern  const MR_FA_TypeInfo_Struct1    ML_type_info_for_list_of_char;
extern  const MR_FA_TypeInfo_Struct1    ML_type_info_for_list_of_string;
extern  const MR_FA_TypeInfo_Struct1    ML_type_info_for_list_of_type_info;
extern  const MR_FA_TypeInfo_Struct1 ML_type_info_for_list_of_pseudo_type_info;

// in trace/mercury_trace_internal.h
extern  char        *MR_trace_getline(const char *,
                        FILE *mdb_in, FILE *mdb_out);
extern  char        *MR_trace_get_command(const char *, FILE *, FILE *);

// in trace/mercury_trace_vars.h
extern  const char  *MR_trace_browse_all_on_level(FILE *,
                        const MR_LabelLayout *, MR_Word *, MR_Word *,
                        int, MR_bool);

// in trace/mercury_trace_external.h
extern  void        MR_trace_init_external(void);
extern  void        MR_trace_final_external(void);

// in library/type_desc.m
extern  MR_String   ML_type_name(MR_Word);

// in runtime/mercury_trace_base.c
extern  MR_Code     *MR_trace_fake(const MR_LabelLayout *);

// in trace/mercury_trace.c
extern  MR_Code     *MR_trace_real(const MR_LabelLayout *);
extern  void        MR_trace_interrupt_handler(void);

// in trace/mercury_trace_tables.c
extern  void        MR_register_module_layout_real(const MR_ModuleLayout *);

////////////////////////////////////////////////////////////////////////////

#endif // not MERCURY_INIT_H

////////////////////////////////////////////////////////////////////////////
