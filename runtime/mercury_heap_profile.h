// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998, 2000-2001, 2006, 2011 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// File: mercury_heap_profile.h
// Main authors: zs, fjh
//
// This module records information about the allocations of cells on the heap.
// The information recorded here is used by mercury_prof.c and
// by library/benchmarking.m.

////////////////////////////////////////////////////////////////////////////

#ifndef MERCURY_HEAP_PROFILE_H
#define MERCURY_HEAP_PROFILE_H

#include "mercury_types.h"     // for `MR_Code'
#include "mercury_dword.h"     // for `MR_Dword'
#include "mercury_type_info.h" // for `MR_STATIC_CODE_CONST'

////////////////////////////////////////////////////////////////////////////

// Type declarations.

// We count memory allocation in units of
//  - cells (i.e. individual allocations), and
//  - words
//
// We keep track of how much allocation occurs in each "period".
// A period ends (and a new period begins) at each call to
// report_stats or report_full_memory_stats in library/benchmarking.m.
// We also keep track of the total allocation over all periods.
//
// We keep track of how much memory was allocated
//  - by each procedure,
//  - for objects of each type,
//  - and an overall total
//
// The tables of counters for each procedure is represented as a binary
// search tree. Similarly for the table of counters for each type.
//
// Due to garbage collection, the total amount of memory allocated can exceed
// the amount of real or even virtual memory available. Hence the total amount
// of memory allocated by a long-running Mercury program might not fit into a
// single 32-bit `unsigned long'. This is why we use MR_Dwords, which are
// at least 64 bits in size.

typedef struct MR_memprof_counter
{
    MR_Dword    cells_at_period_start;
    MR_Dword    words_at_period_start;
    MR_Dword    cells_since_period_start;
    MR_Dword    words_since_period_start;
} MR_memprof_counter;

// Type representing a binary tree node.
typedef struct MR_memprof_record
{
    const char                      *type_name; // type name or NULL
    MR_STATIC_CODE_CONST MR_Code    *proc;      // proc address or NULL
    MR_memprof_counter              counter;
    struct MR_memprof_record        *left;      // left sub-tree
    struct MR_memprof_record        *right;     // right sub-tree
} MR_memprof_record;

// Type representing a binary tree.
typedef struct MR_memprof_table
{
    MR_memprof_record       *root;
    int                     num_entries;
} MR_memprof_table;

////////////////////////////////////////////////////////////////////////////

// global variables

extern MR_memprof_counter   MR_memprof_overall;
extern MR_memprof_table     MR_memprof_procs;
extern MR_memprof_table     MR_memprof_types;

////////////////////////////////////////////////////////////////////////////

// Function declarations.

// MR_record_allocation(size, alloc_id, type):
//
// Record heap profiling information for an allocation of one cell of `size'
// words. The heap profiling information is recorded in the three global
// variables above.

extern void     MR_record_allocation(int size,
                    const MR_AllocSiteInfoPtr alloc_id, const char *type_name);

// MR_prof_output_mem_tables():
//
// Write out the information recorded by MR_record_allocation() to a pair
// of files `Prof.MemoryMR_Words' and `Prof.MemoryCells'.

extern void     MR_prof_output_mem_tables(void);

////////////////////////////////////////////////////////////////////////////

// At runtime turn heap profiling on or off.

extern void     MR_prof_turn_on_heap_profiling(void);

extern void     MR_prof_turn_off_heap_profiling(void);

////////////////////////////////////////////////////////////////////////////

extern void     MR_register_alloc_sites(const MR_AllocSiteInfo *alloc_sites,
                    int size);

extern void     MR_report_memory_attribution(MR_ConstString label,
                    MR_bool run_collect);

extern void     MR_finish_prof_snapshots_file(void);

////////////////////////////////////////////////////////////////////////////

#endif // MERCURY_HEAP_PROFILE_H

////////////////////////////////////////////////////////////////////////////
