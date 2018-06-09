// vim: ts=4 sw=4 expandtab ft=c

// mercury_hgc.h
// Copyright (C) 2009 Ralph Becket <ralphbecket@gmail.com>
// Copyright (C) 2010 The University of Melbourne.
// Copyright (C) 2015-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// hgc.h - A history-based conservative garbage collector.

#ifndef MERCURY_HGC_H
#define MERCURY_HGC_H

#ifdef MR_HGC

#include <stdlib.h>

// Initialise hgc. This should be called from the lowest stack frame in
// your program, unless you are using hgc_set_stack_bot().

extern void     MR_hgc_init(void);

// Tell hgc about a global variable or address range that may point into the
// heap.

extern void     MR_hgc_add_root(void *p);

extern void     MR_hgc_add_roots_range(void *bot, void *top);

// An "immutable" cell in hgc parlance means one that will never hold
// a reference to a more recently allocated cell. Any other changes
// to an "immutable" cell's contents are fine.

extern void     *MR_hgc_malloc_immutable(size_t size_bytes);

// Make a cell mutable. This allows you to include in the cell references
// to more recently allocated cells. Mutable cells can cause hgc to be
// less efficient since they are garbage collected less frequently and
// delay the collection of other dead cells they reference.

extern void     MR_hgc_make_mutable(void *p);

// Allocate a cell and make it mutable.

extern void     *MR_hgc_malloc_mutable(size_t size_bytes);

// Ensure an allocated cell is never collected.

extern void     MR_hgc_make_uncollectable(void *p);

// Allocate a mutable cell which will never be collected.

extern void     *MR_hgc_malloc_uncollectable(size_t size_bytes);

// If necessary, allocate a new cell of the given size and copy as much of the
// data from the old cell as will fit. The new cell is mutable if the old cell
// is. The new cell is uncollectable if the old cell is.

extern void     *MR_hgc_realloc(void *p, size_t size_bytes);

// Free up a cell (this essentially just fills the cell with NULL pointers).

extern void     MR_hgc_free(void *p);

// Force a garbage collection. This should be unnecessary in normal practice.

extern void     MR_hgc_gc(void);

// Tell hgc what you think is the bottom stack address. This is estimated by
// hgc_init() unless already set using hgc_set_stack_bot().
// hgc_set_stack_bot() can be called before or after hgc_init().

extern void     MR_hgc_set_stack_bot(void *p);

// Definitions providing a Boehm-like interface.

typedef void    *GC_hidden_pointer;

#define GC_INIT                 MR_hgc_init
#define GC_malloc               MR_hgc_malloc_immutable
#define GC_MALLOC               MR_hgc_malloc_immutable
#define GC_MALLOC_ATOMIC        MR_hgc_malloc_immutable
#define GC_MALLOC_UNCOLLECTABLE MR_hgc_malloc_uncollectable
#define GC_REALLOC              MR_hgc_realloc
#define GC_gcollect             MR_hgc_gc
#define GC_free                 MR_hgc_free
#define GC_FREE                 MR_hgc_free
#define HIDE_POINTER(p)         (~(long long)(p))
#define REVEAL_POINTER(p)       ((void *)HIDE_POINTER(p))
#define GC_register_finalizer(p, f, d, of, od)
// GC_register_finalizer is not supported.

#endif // MR_HGC

#endif // MERCURY_HGC_H
