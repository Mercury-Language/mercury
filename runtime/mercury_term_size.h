// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2003, 2005 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_term_size.h
//
// This module declares functions for returning the sizes of terms.

#ifndef MR_MERCURY_TERM_SIZE_H
#define MR_MERCURY_TERM_SIZE_H

#include "mercury_std.h"        // for MR_bool
#include "mercury_types.h"      // for the typedefs of the structs we define

#include "mercury_complexity.h" // for MR_ComplexityProc, etc

#ifdef  MR_RECORD_TERM_SIZES

extern  MR_ComplexityCounter    MR_complexity_word_counter;
extern  MR_ComplexityCounter    MR_complexity_cell_counter;
extern  MR_ComplexityCounter    MR_complexity_tick_counter;

#define MR_complexity_is_active(numprocs, procnum, name, inputs, is_active) \
    do {                                                                    \
        is_active = MR_complexity_is_active_func((numprocs), (procnum),     \
            (name), (inputs));                                              \
    } while (0)

#define MR_complexity_call_proc(procnum, slot)                              \
    do {                                                                    \
        slot = MR_complexity_call_func(procnum);                            \
    } while (0)

#define MR_complexity_exit_proc(procnum, slot)                              \
    do {                                                                    \
        MR_complexity_leave_func(procnum, slot);                            \
    } while (0)

#define MR_complexity_fail_proc(procnum, slot)                              \
    do {                                                                    \
        MR_complexity_leave_func(procnum, slot);                            \
    } while (0)

#define MR_complexity_redo_proc(procnum, slot)                              \
    do {                                                                    \
        MR_complexity_redo_func(procnum, slot);                             \
    } while (0)

extern  MR_Unsigned MR_term_size(MR_TypeInfo type_info, MR_Word term);

extern  void    MR_init_complexity_proc(int proc_num, const char *fullname,
                    int num_profiled_args, int num_args,
                    MR_ComplexityArgInfo *arg_infos);
extern  void    MR_check_complexity_init(void);

extern  void    MR_write_complexity_procs(void);

extern  MR_ComplexityIsActive
                MR_complexity_is_active_func(int num_procs, int proc_num,
                    const char *name, int num_inputs);
extern  int     MR_complexity_call_func(int procnum);
extern  void    MR_complexity_leave_func(int procnum, int slot);
extern  void    MR_complexity_redo_func(int procnum, int slot);
extern  void    MR_complexity_fill_size_slot(MR_ComplexityProc *proc, int slot,
                    int num_input_args, int argnum, int size);

#else   // MR_RECORD_TERM_SIZES

// Term sizes are not meaningful if MR_RECORD_TERM_SIZES is not defined.
// This macro, and others in mercury_heap.h, allows us to write code to
// compute term sizes without worrying about whether MR_RECORD_TERM_SIZES
// is defined or not.

#define MR_term_size(type_info, term)       0

#endif  // MR_RECORD_TERM_SIZES

#endif  // MR_MERCURY_TERM_SIZE_H

#define MR_COMPLEXITY_SLOTS_PER_CHUNK   1024
