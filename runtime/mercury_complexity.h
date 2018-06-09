// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2003 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_complexity.h
//
// This module defines the structures returning the sizes of terms.

#ifndef MR_MERCURY_COMPLEXITY_H
#define MR_MERCURY_COMPLEXITY_H

#define MR_COMPLEXITY_SLOTS_PER_CHUNK   1024

// This type should correspond to the representation of the
// complexity_is_active type in library/term_size_prof_builtin.m.

typedef struct MR_ComplexityProc_Struct         MR_ComplexityProc;

typedef enum {
    MR_COMPLEXITY_INPUT_VAR_SIZE,
    MR_COMPLEXITY_INPUT_FIX_SIZE,
    MR_COMPLEXITY_OUTPUT
} MR_ComplexityArgKind;

typedef struct {
    const char              *MR_clpai_maybe_name;
    MR_ComplexityArgKind    MR_clpai_kind;
} MR_ComplexityArgInfo;

typedef enum {
    MR_COMPLEXITY_IS_INACTIVE,
    MR_COMPLEXITY_IS_ACTIVE
} MR_ComplexityIsActive;

typedef int MR_ComplexityCounter;

typedef struct {
    MR_ComplexityCounter    MR_clpm_num_words;
    MR_ComplexityCounter    MR_clpm_num_cells;
    MR_ComplexityCounter    MR_clpm_num_ticks;
} MR_ComplexityMetrics;

typedef struct MR_ComplexityPastSlots_Struct MR_ComplexityPastSlots;

struct MR_ComplexityPastSlots_Struct {
    MR_ComplexityMetrics    *MR_clpps_metrics;
    int                     *MR_clpps_sizes;
    MR_ComplexityPastSlots  *MR_clpps_previous;
};

// The MR_ComplexityProc structure contains all the information we have
// about a procedure whose complexity we are trying to determine
// experimentally.
//
// The MR_clp_full_proc_name field contains the name of the procedure. The
// format of the name is name/arity-modenum, where name is the a fully module
// qualified predicate or function name, arity is the user arity (not including
// the return value for functions), and modenum is the mode number. This field
// is initialized in the program's mkinit-generated _init.c file and never
// modified later, but is sanity-checked on every invocation of
// MR_complexity_is_active.
//
// The MR_clp_num_profiled_args field contains the number of profiled input
// arguments of the procedure.
// XXX
// It is initialized and re-initialized on every invocation of
// MR_complexity_is_active.
//
// The MR_clp_is_active field specifies whether the procedure is currently
// being executed. It is initialized in the program's _init.c file, and updated
// as necessary by the macros we invoke at the call, exit, fail and redo ports.
//
// The MR_clp_metrics field points to an array of MR_COMPLEXITY_SLOTS_PER_CHUNK
// elements, while the MR_clp_sizes field point to an array of
// MR_COMPLEXITY_SLOTS_PER_CHUNK times MR_clp_num_profiled_args elements.
//
// Each top-level invocation of the procedure allocates a "slot" for its
// measurement. The slot consists of one element of the MR_clp_metrics array
// and MR_clp_num_profiled_args consecutive elements of the MR_clp_sizes array.
// The MR_clp_next_slot_num identifies the next available slot.
//
// When we have used up all the slots in the MR_clp_metrics and MR_clp_sizes
// arrays, we allocate a MR_ComplexityPastSlots structure, move those arrays to
// the new structure, and allocate a fresh pair of arrays.
//
// The full set of measurements for a procedure is thus the initial
// MR_clp_next_slot_num elements of the MR_clp_metrics and MR_clp_sizes arrays,
// and the MR_COMPLEXITY_SLOTS_PER_CHUNK elements of each structure linked
// via MR_clp_past_slots.

struct MR_ComplexityProc_Struct {
    const char              *MR_clp_full_proc_name;
    int                     MR_clp_num_profiled_args;
    int                     MR_clp_num_args;
    MR_ComplexityArgInfo    *MR_clp_arg_infos;
    MR_ComplexityIsActive   MR_clp_is_active;
    MR_ComplexityPastSlots  *MR_clp_past_slots;
    int                     MR_clp_next_slot_num;
    MR_ComplexityMetrics    *MR_clp_metrics;
    int                     *MR_clp_sizes;
};

#endif  // MR_MERCURY_COMPLEXITY_H
