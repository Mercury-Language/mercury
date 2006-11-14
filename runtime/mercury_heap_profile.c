/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 1997, 1999-2001, 2006 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** File: mercury_heap_profile.c.
** Main authors: zs, fjh
**
** This module records information about the allocations of cells on the heap.
**
** The information recorded by this module is used by code in
** library/benchmarking.m.
*/

/*---------------------------------------------------------------------------*/

#include "mercury_imp.h"

#include <stdio.h>
#include <string.h>

#include "mercury_prof_mem.h"
#include "mercury_dword.h"
#include "mercury_heap_profile.h"

/* all fields of these variables are initialized to 0 */
MR_memprof_counter  MR_memprof_overall;
MR_memprof_table    MR_memprof_procs;
MR_memprof_table    MR_memprof_types;

/* private global variables */
static MR_bool      profile_heap = MR_TRUE;

/*
** Initialize a heap profiling counter.
*/
static void
MR_init_counter(MR_memprof_counter *counter)
{
    MR_zero_dword(counter->cells_at_period_start);
    MR_zero_dword(counter->words_at_period_start);
    MR_zero_dword(counter->cells_since_period_start);
    MR_zero_dword(counter->words_since_period_start);
}

/*
** Increment the fields in a heap profiling counter to record the allocation
** of a single cell of `size' words.
*/

static void
MR_increment_counter(MR_memprof_counter *counter, int size)
{
    MR_increment_dword(counter->cells_since_period_start, 1);
    MR_increment_dword(counter->words_since_period_start, size);
}

/*
** Search the specified `table' to find the entry for the given `name'
** allocating one if there isn't one already, and then increment
** the counters for that entry for an allocation of the specified `size'.
*/

static void
MR_increment_table_entry(MR_memprof_table *table, const char *name,
    MR_Code *addr, int size)
{
    MR_bool             found;
    int                 diff;
    MR_memprof_record   **node_addr;
    MR_memprof_record   *node;

    /*
    ** Search the tree to find the node with this name.
    */
    found = MR_FALSE;
    node_addr = &table->root;
    while ((node = *node_addr) != NULL) {
        diff = strcmp(name, node->name);
        if (diff < 0) {
            node_addr = &node->left;
        } else if (diff > 0) {
            node_addr = &node->right;
        } else {
            found = MR_TRUE;
            break;
        }
    }

    /*
    ** If the tree didn't already contain a node with this name,
    ** create a new node for it.
    */
    if (!found) {
        char *copy_of_name;

        node = MR_PROF_NEW(MR_memprof_record);
        /*
        ** We need to make a fresh copy of the name, rather than just copying
        ** the pointer, because our caller may deallocate its copy of the name.
        ** Normally the name will be a string literal, but even then it might
        ** be a string literal from a dlopen()'ed module which will later
        ** get dlclose()'d.
        */
        copy_of_name = MR_PROF_NEW_ARRAY(char, strlen(name) + 1);
        strcpy(copy_of_name, name);
        node->name = copy_of_name;
        node->addr = addr;
        node->left = NULL;
        node->right = NULL;
        MR_init_counter(&node->counter);

        *node_addr = node;

        table->num_entries++;
    }

    /* Now record the counts in this node */
    MR_increment_counter(&node->counter, size);
}

/*
** Record heap profiling information for an allocation of size `size'
** in procedure `proc' for an object of type `type'.
*/

void
MR_record_allocation(int size, MR_Code *proc_addr,
    const char *proc_name, const char *type)
{
    if (!profile_heap) {
        return;
    }

    /*
    ** Increment the overall totals,
    ** record the allocation in the per-procedure table, and
    ** record the allocation in the per-type table.
    */
    MR_increment_counter(&MR_memprof_overall, size);
    MR_increment_table_entry(&MR_memprof_procs, proc_name, proc_addr, size);
    MR_increment_table_entry(&MR_memprof_types, type, NULL, size);
}

void
MR_prof_turn_on_heap_profiling(void)
{
    profile_heap = MR_TRUE;
}

void
MR_prof_turn_off_heap_profiling(void)
{
    profile_heap = MR_FALSE;
}
