// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2009 Ralph Becket <ralphbecket@gmail.com>
// Copyright (C) 2010 The University of Melbourne.
// Copyright (C) 2015-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// hgc.c - A history-based conservative garbage collector.
//
// TODO: support collection of mutables.
// TODO: explore incremental collection.
//
// This collector assumes that large and/or mutable heap cells are rare and
// that otherwise all heap cells contain pointers only to less recently
// allocated cells (i.e. a cell allocated at time t will never contain a
// pointer to a cell allocated at time t' > t). This assumption is valid
// for many declarative programming languages.
//
// Cells are allocated in 4 Kbyte "pages" (allocated on 4 KByte boundaries).
// Cells are powers of two in size and are allocated on the corresponding
// address boundaries. Each page contains cells of the same size "payload"
// (i.e. the user accessible portion of the cell). The first cell in each page
// is reserved for metadata, specifically the payload size and bit mask
// used to convert a pointer into the page to a pointer to the corresponding
// cell in the page.
//
// Each cell contains an extra "next" pointer, in addition to the cell
// payload. This pointer is used to link the cell on to either the
// appropriate free list (there is one free list for each payload size) or
// the allocation chain. The least significant bit of the "next" pointer
// is used to mark cells by the mark/sweep garbage collector.
//
// The allocation chain is a singly linked list of cells that have been
// allocated. The successors of each cell on the allocation list are those
// cells which were allocated before the cell in question.
//
// Garbage collection proceeds as follows:
// (1) mark all cells referred to from roots (registers, the stack, etc.);
// (2) considering each cell in order on the allocation chain,
// (2a) if the cell is marked then preserve it and mark any cells to which
//     its payload refers;
// (2b) if the cell is not marked then sweep it on to the appropriate free
//     list.
//
// In this way marking and sweeping occur in a single pass without the need
// for a mark stack.
//
// Special handling is required for cells that contain mutable data
// (i.e. cells which may contain pointers to more recently allocated cells)
// and for "big" cells (i.e. those for which the big-bag-o-pages approach
// doesn't scale).
//
// NOTES
//
// - "Ordinary" cells (up to a set size) are referred to as "small cells".
// Other cells are referred to as "big cells". Big cells are assumed to be
// rare and are allocated using a different structure (see hgc_big.c).
//
// - Empirically, bit-masking is nearly ten times faster than using division
// and modulo arithmetic, which makes a modest amount of memory wastage
// acceptable for performance. This is why cells are allocated on power-
// of-two address boundaries.
//
// - We could consider using a spare tag bit to identify cells that contain
// only non-pointer data (and thereby avoid marking through them).
// This could be useful for allocating strings and arrays of numbers.

#include "mercury_imp.h"

#ifdef MR_HGC

// Default values that can be overridden by setting the appropriate
// environment variable (e.g., set HGC_X or get default_HGC_X).
// Sizes are in megabytes, except for the growth variables which are
// per centages.

// Amount of memory (in megabytes) to reserve for the entire heap.
#define default_HGC_HEAP_SIZE 256

// Megabytes consumed before the first GC by the small heap.
#define default_HGC_INITIAL_SMALL_HEAP_SIZE 1

// More than this per centage of live data causes the small heap to grow.
#define default_HGC_GROWTH_THRESHOLD 20

// The per centage by which the small heap is grown.
#define default_HGC_GROWTH_FACTOR 50

// Megabytes consumed before the first GC by the big heap.
#define default_HGC_INITIAL_BIG_HEAP_SIZE 1

#include <stdio.h>
#include <setjmp.h>
#include <bits/setjmp.h>

#include "mercury_hgc.h"
#include "mercury_expanding_array.h"

#ifdef TRACE_HGC    // Define this to enable tracing messages.
#define TRC
#else
#define TRC if (0)
#endif

typedef MR_Integer hgc_int;
typedef MR_Word hgc_word;
typedef hgc_word* hgc_ptr;

#define hgc_num_tag_bits (MR_LOW_TAG_BITS)

#define word_size_bytes (sizeof(hgc_word))
#define words(size_in_bytes) \
    (((size_in_bytes) + word_size_bytes - 1) / word_size_bytes)
#define bytes(size_in_words) \
    ((size_in_words) * word_size_bytes)

// The least significant bit of the cell "next" field is used as a "mark bit"
// to indicate live data during mark/sweep garbage collection.

#define tag_mask (((hgc_int)(1 << hgc_num_tag_bits)) - 1)
#define addr_mask (~tag_mask)
#define tag(p) (((hgc_int)(p)) & tag_mask)
#define untag(p) (hgc_ptr)(((hgc_int)(p)) & addr_mask)
#define set_tag(p) (hgc_ptr)(((hgc_int)(p)) | 0x1)
#define clear_tag(p) (hgc_ptr)(((hgc_int)(p)) & ~0x1)

// A page is a 4 KByte collection of small_cells of the same payload size.
// The first cell on a page is reserved for page meta-data (the bit mask
// used to find the start address of cells in the page and the payload
// size of the cells in the page). Pages are allocated upwards from the
// bottom of the heap.

#define page_size_in_bytes (1 << 12)
#define page_size words(page_size_in_bytes)
#define page_mask (~(hgc_int)(page_size_in_bytes - 1))
#define page(p) (hgc_ptr)((hgc_int)(p) & page_mask)
#define page_small_cell_mask(pg) ((pg) + 0)
#define page_payload_size(pg) (hgc_int *)((pg) + 1)
#define page_small_cell(pg, p) \
    (hgc_ptr)((hgc_int)(p) & *page_small_cell_mask(pg))

// Small cells.
// A small_cell consists of a "next" pointer (either in the allocation list
// or the corresponding free list), the payload data, and padding to round
// the cell size up to a power of two. ALWAYS remember to untag the value
// obtained from dereferencing small_cell_next.

#define small_cell_next(cl) (hgc_ptr *)((cl) + 0)
#define get_small_cell_next(cl) untag(*small_cell_next(cl))
#define set_small_cell_next(cl, p) *small_cell_next(cl) = (p)
#define small_cell_payload(cl) ((cl) + 1)
#define max_small_payload 32            // Sizes 0..max_small_payload.

// Big cells.
// The prev field is the backwards link for a big_cell on a free list.
// The is_free field is 0 iff the cell is not on a free list.
// A size_index field of k means the cell occupies 1<<k words.
// The big_cell with the lowest address is found at big_bot. ALWAYS remember
// to untag the value obtained form dereferencing big_cell_next.

#define big_cell_next(cl) (hgc_ptr *)((cl) + 0)
#define get_big_cell_next(cl) untag(*big_cell_next(cl))
#define set_big_cell_next(cl, p) *big_cell_next(cl) = (p)
#define big_cell_prev(cl) (hgc_ptr *)((cl) + 1)
#define get_big_cell_prev(cl) *big_cell_prev(cl)
#define set_big_cell_prev(cl, p) *big_cell_prev(cl) = (p)
#define big_cell_is_free(cl) (hgc_int *)((cl) + 1)
#define big_cell_size_index(cl) (hgc_int *)((cl) + 2)
#define big_cell_payload_size(cl) (hgc_int *)((cl) + 3)
#define big_cell_payload(cl) ((cl) + 4)
#define big_cell_min_size(payload_size) ((payload_size) + 4)
#define big_cell_buddy(cl) \
    (big_bot + (((cl) - big_bot) ^ (1 << *big_cell_size_index(cl))))
#define min_big_cell_size_index 5
#define max_big_cell_size_index 64

static hgc_int hgc_initialised = 0;     // 1 if hgc_init() has been called.
static hgc_ptr heap_bot = NULL;         // Start of heap memory.
static hgc_ptr heap_top;                // One past heap end.
static hgc_ptr small_bot = NULL;        // The first initialised page.
static hgc_ptr small_top;               // One past last initialised page.
static hgc_ptr small_high_water;        // Used to control heap expansion.
static hgc_int growth_threshold = 20;   // Grow small heap if more than
                                        // this percent of memory is live.

static hgc_int growth_factor = 50;      // Percent growth for small_high_water.

static hgc_ptr small_cell_free_list[max_small_payload + 1];
                                        // Array of small_cell free lists.
static hgc_ptr big_bot = NULL;          // The start of the big_cell heap.
static hgc_ptr big_top;                 // One past the end.
static hgc_int big_size_index;          // big_top =
                                        //      big_bot + 1 << big_size_index

static hgc_ptr big_low_water;           // Used to control heap expansion.
static hgc_ptr alloc_list = NULL;       // List of allocated cells.
static hgc_ptr big_cell_free_list[max_big_cell_size_index + 1];
                                        // Array of big_cell free lists.
static hgc_ptr *stack_bot = NULL;       // Base of the C stack.

// The roots structure is used to hold fixed ranges of addresses that
// may contain pointers to cells.

struct roots_range {
    hgc_ptr *roots_bot;                 // Start of range.
    hgc_ptr *roots_top;                 // One past end of range.
};

// Define the roots_array expanding array type and its operations.

MR_DEFINE_EXPANDING_ARRAY(roots_array, struct roots_range, 256);

static roots_array *roots = NULL;       // The roots ranges.

// Define the mutables_array expanding array type and its operations.

MR_DEFINE_EXPANDING_ARRAY(mutables_array, hgc_ptr, 256);

static mutables_array *mutables = NULL; // The mutables array.

// Define the uncollectables_array expanding array type and its operations.

MR_DEFINE_EXPANDING_ARRAY(uncollectables_array, hgc_ptr, 256);

static uncollectables_array *uncollectables = NULL;

////////////////////////////////////////////////////////////////////////////
// Small heap management.
////////////////////////////////////////////////////////////////////////////

// Set the mark bit on a small_cell (the mark bit lives in the
// small_cell_next field).

static void
mark_small_cell(hgc_ptr cl)
{
    set_small_cell_next(cl, set_tag(get_small_cell_next(cl)));
}

// Return non-zero iff the mark bit is set on this small_cell.

static hgc_int
small_cell_is_marked(hgc_ptr cl)
{
    return tag(*small_cell_next(cl));
}

// Find the small_cell referenced by a pointer.
// Returns 0 iff the pointer points outside the small heap.

static int
find_small_cell(hgc_ptr p, hgc_ptr *cl_ref, hgc_ptr *payload_ref,
    hgc_int *payload_size_ref)
{
    if (small_bot <= p && p < small_top) {
        hgc_ptr pg = page(p);
        *cl_ref = page_small_cell(pg, p);
        *payload_size_ref = *page_payload_size(pg);
        *payload_ref = small_cell_payload(*cl_ref);
        return 1;
    } else {
        return 0;
    }
}

// Initialise a new page and add its small_cells to the given
// small_cell_free_list.

static void
init_new_page(hgc_int payload_size)
{
    hgc_ptr pg;
    hgc_int pg_cell_size;
    hgc_ptr cl;

    TRC if (!hgc_initialised) {
        fprintf(stderr, "hgc: hgc_init() has not been called.\n");
        exit(1);
    }

    pg = small_top;
    small_top += page_size;
    if (small_top > big_low_water) {
        fprintf(stderr, "hgc: init_new_page: out of memory.\n");
        exit(1);
    }

    // Each small_cell has to be at least one word larger than its payload to
    // include the small_cell_next 'history' pointer. Cell sizes are always
    // a power of 2.

    for (pg_cell_size = 2; pg_cell_size <= payload_size; pg_cell_size <<= 1) {}
    *page_payload_size(pg) = payload_size;
    // Cells are allocated at addresses that are multiples of the small_cell
    // size.

    *page_small_cell_mask(pg) = ~((pg_cell_size * sizeof(hgc_word)) - 1);

    // Add the small_cells in this page to the appropriate free list
    // (recall that the first cell in a page is reserved for page metadata).

    for (cl = pg + pg_cell_size; cl < small_top; cl += pg_cell_size) {
        set_small_cell_next(cl, small_cell_free_list[payload_size]);
        small_cell_free_list[payload_size] = cl;
    }
}

// Allocate a new small_cell from a free list (the pointer returned is to
// the small_cell, not its payload).

static hgc_ptr
alloc_small_cell(hgc_int payload_size)
{
    hgc_ptr cl = small_cell_free_list[payload_size];

    if (cl != NULL) {

        small_cell_free_list[payload_size] = get_small_cell_next(cl);
        set_small_cell_next(cl, alloc_list);
        alloc_list = cl;
        return cl;

    }

    // There are no free cells of this size. Allocate a new page if possible.

    if (small_top + page_size <= small_high_water) {
        init_new_page(payload_size);
        return alloc_small_cell(payload_size);
    }

    // We can't allocate a new page, try garbage collecting.

    MR_hgc_gc();
    if (small_cell_free_list[payload_size] != NULL) {
        return alloc_small_cell(payload_size);
    }

    // Try growing small_high_water.

    small_high_water =
        small_bot + ((100 + growth_factor) * (small_top - small_bot)) / 100;
    if (small_high_water <= big_low_water) {
        return alloc_small_cell(payload_size);
    }

    // Nope, we are out of memory.

    fprintf(stderr, "hgc: alloc_small_cell: out of memory.\n");
    exit(1);
}

////////////////////////////////////////////////////////////////////////////
// Big heap management.
////////////////////////////////////////////////////////////////////////////

// "Big" cells don't work well in the BIBOP approach used for "small" cells
// and indeed some big cells won't even fit inside a small_cell page. Instead
// we use a buddy style memory allocator.
//
// Each big_cell is a power of two words in size. A big_cell can be split into
// two smaller, equally sized, "buddy" big_cells. If both buddies become free,
// they are merged into a single cell of the next size up. A free list exists
// for each size of big_cell. Allocations are made to the smallest sized
// big_cell that can accommodate the payload; this may mean first splitting up
// a larger big_cell if one of the right size is unavailable.
//
// To support O(1) removal of a buddy from its free list during free cell
// merging, each free list is doubly linked. We re-use the "prev" field
// to indicate whether a big_cell is allocated or not (it is NULL if it
// is allocated, non-NULL otherwise).

// Set the mark bit on a big_cell (the mark bit lives in the
// big_cell_next field).

static void
mark_big_cell(hgc_ptr cl)
{
    set_big_cell_next(cl, set_tag(get_big_cell_next(cl)));
}

// Return non-zero iff the mark bit is set on this big_cell.

static hgc_int
big_cell_is_marked(hgc_ptr cl)
{
    return tag(*big_cell_next(cl));
}

// Print out a representation of the big cell heap for debugging.

static void
print_big_cells(void)
{
    hgc_ptr cl;
    hgc_int ix;

    for (cl = big_bot; cl < big_top; cl += 1 << ix) {
        int i;
        ix = *big_cell_size_index(cl);
        for (i = big_size_index - ix; i > 0; i--) {
            printf("  ");
        }
        printf("[ %l ] %p ", ix, cl);
        if (*big_cell_is_free(cl)) {
            printf("free\n");
        } else {
            printf("allocated %l\n", *big_cell_payload_size(cl));
        }
    }
}

static void
print_big_cell_free_list(void)
{
    hgc_int i;
    hgc_ptr cl;

    for (i = 0; i <= max_big_cell_size_index; i++) {
        if (big_cell_free_list[i] != NULL) {
            printf("free %2d", i);
            for (
                cl = big_cell_free_list[i];
                cl != NULL;
                cl = get_big_cell_next(cl)
            ) {
                printf(" %p", cl);
                // Do some sanity checking.
                if (! *big_cell_is_free(cl)) {
                    printf(" is not marked as free!\n");
                    exit(1);
                }
                if (*big_cell_size_index(cl) != i) {
                    printf(" has the wrong size index!\n");
                    exit(1);
                }
            }
            printf("\n");
        }
    }
}

static void
print_big_heap(void)
{
    printf("big_top = %p\n", big_top);
    printf("big_bot = %p\n", big_bot);
    printf("big heap is %l words\n", big_top - big_bot);
    printf("big_size_index = %l\n", big_size_index);
    print_big_cells();
    print_big_cell_free_list();
}

// Check the integrity of the big heap.

static void
check_big_heap(void)
{
    hgc_ptr cl;
    hgc_ptr prev_cl;
    hgc_int ix;
    // Mark every free cell with a 0xDEADBEEF marker in its first
    // payload slot.
    for (cl = big_bot; cl < big_top; cl += 1 << *big_cell_size_index(cl)) {
        if (*big_cell_is_free(cl)) {
            *big_cell_payload(cl) = 0xDEADBEEF;
        }
    }

    // Check the sizes and links on the free lists.
    for (ix = 0; ix <= max_big_cell_size_index; ix++) {
        prev_cl = (hgc_ptr)(1);
        for (
            cl = big_cell_free_list[ix];
            cl != NULL;
            cl = get_big_cell_next(cl)
        ) {
            if (*big_cell_size_index(cl) != ix) {
                printf("hgc: check_big_heap: %p has size index %l, "
                        "but should have %l\n", cl,
                        *big_cell_size_index(cl), ix);
                print_big_heap();
                exit(1);
            }
            if (get_big_cell_prev(cl) != prev_cl) {
                printf("hgc: check_big_heap: %p has prev pointer %p, "
                        "but should have %p\n", cl,
                        get_big_cell_prev(cl), prev_cl);
                print_big_heap();
                exit(1);
            }
            if (*big_cell_payload(cl) != 0xDEADBEEF) {
                printf("hgc: check_big_heap: %p is free, but does not "
                        "contain 0xDEADBEEF\n", cl);
                print_big_heap();
                exit(1);
            }
            prev_cl = cl;
        }
    }
}

// Find the big_cell referenced by a pointer.
// Returns 0 iff the pointer points outside the big heap or the cell
// referenced is unallocated.

static int
find_big_cell(hgc_ptr p, hgc_ptr *cl_ref, hgc_ptr *payload_ref,
        hgc_int *payload_size_ref)
{
    hgc_ptr cl = big_bot;
    hgc_int ix = big_size_index;
    hgc_int size = 1 << big_size_index;
    if (p < big_bot || big_top <= p) {
        return 0;
    }
    while (1) {
        if (*big_cell_size_index(cl) == ix) {
            // We have found the cell.
            if (*big_cell_is_free(cl)) {
                return 0;
            }
            *cl_ref = cl;
            *payload_ref = big_cell_payload(cl);
            *payload_size_ref = *big_cell_payload_size(cl);
            return 1;
        }
        // It has to be a subdivision of this cell.

        ix--;
        size /= 2;
        if (cl + size <= p) {
            // It is in the upper half.
            cl += size;
        }
    }
}

// Add a big_cell to the appropriate free list (but don't attempt to merge
// with its buddy).

static void
add_big_cell_to_free_list(hgc_ptr cl)
{
    hgc_int ix = *big_cell_size_index(cl);
    hgc_ptr next_cl = big_cell_free_list[ix];
    big_cell_free_list[ix] = cl;
        // We have to mark this cell as free.
    *big_cell_is_free(cl) = 1;
    set_big_cell_next(cl, next_cl);
    if (next_cl != NULL) {
        set_big_cell_prev(next_cl, cl);
    }
}

// Unlink a big_cell from its free list.

static void
unlink_big_cell_from_free_list(hgc_ptr cl)
{
    hgc_ptr next_cl = get_big_cell_next(cl);
    hgc_ptr prev_cl = get_big_cell_prev(cl);
    hgc_int ix = *big_cell_size_index(cl);

    if (big_cell_free_list[ix] == cl) {
        big_cell_free_list[ix] = next_cl;
    } else {
        set_big_cell_next(prev_cl, next_cl);
    }
    if (next_cl != NULL) {
        set_big_cell_prev(next_cl, prev_cl);
    }
}

// Split an unlinked big_cell in two and add the upper half to the
// appropriate free list.

static void
split_big_cell(hgc_ptr cl)
{
    hgc_int new_ix = *big_cell_size_index(cl) - 1;
    hgc_int new_size = 1 << new_ix;
    hgc_ptr buddy = cl + new_size;
    *big_cell_size_index(cl) = new_ix;
    *big_cell_size_index(buddy) = new_ix;
    add_big_cell_to_free_list(buddy);
}

// Free an unlinked big_cell, making sure we recursively merge it with its
// buddy as long as its buddy is free.

static void
free_big_cell(hgc_ptr cl)
{
    hgc_int ix = *big_cell_size_index(cl);
    hgc_ptr buddy = big_cell_buddy(cl);
    while (
        ix < big_size_index &&
        *big_cell_size_index(buddy) == ix &&
        *big_cell_is_free(buddy)
    ) {
        unlink_big_cell_from_free_list(buddy);
        if (buddy < cl) {
            cl = buddy;
        }
        ix++;
        *big_cell_size_index(cl) = ix;
        buddy = big_cell_buddy(cl);
    }
    add_big_cell_to_free_list(cl);
}

// Allocate a free big_cell of the right size.

static hgc_ptr
alloc_big_cell(hgc_int payload_size)
{
    hgc_int min_cell_size = big_cell_min_size(payload_size);
    hgc_int ix;
    hgc_int size;

    // Find the smallest sufficiently large free big_cell.

    for (
        ix = min_big_cell_size_index, size = 1 << ix;
        ix <= big_size_index;
        ix++, size = 1 << ix
    ) {
        if (size >= min_cell_size && big_cell_free_list[ix] != NULL) {
            // Found one. Now we may need to split it down to size.

            hgc_ptr cl = big_cell_free_list[ix];
            unlink_big_cell_from_free_list(cl);
            while (size / 2 >= min_cell_size) {
                split_big_cell(cl);
                size /= 2;
            }

            // Add it to the alloc_list and return the big_cell.
            *big_cell_payload_size(cl) = payload_size;
            *big_cell_is_free(cl) = 0;
            set_big_cell_next(cl, alloc_list);
            alloc_list = cl;

            return cl;
        }
    }

    // We couldn't find a big enough cell. We can either grow the big heap
    // or try garbage collecting.

    size = big_top - big_bot;
    if (big_bot - size >= big_low_water) {
        // We can grow the big heap.
        big_bot -= size;
        *big_cell_size_index(big_bot) = big_size_index;
        big_size_index++;
        free_big_cell(big_bot);
        return alloc_big_cell(payload_size);
    }

    // We can't grow the big heap right now; try garbage collecting and
    // seeing what happens.

    MR_hgc_gc();
    for (ix = big_size_index; 0 <= ix && min_cell_size <= 1 << ix; ix --) {
        if (big_cell_free_list[ix] != NULL) {
            return alloc_big_cell(payload_size);
        }
    }

    // Try growing big_low_water.

    size = big_top - big_bot;
    if (big_low_water - size >= small_high_water) {
        big_low_water -= size;
        return alloc_big_cell(payload_size);
    }

    // Nope, we are out of memory.

    fprintf(stderr, "hgc: alloc_big_cell: out of memory.\n");
    exit(1);
}

////////////////////////////////////////////////////////////////////////////
// Top-level garbage collection code.
////////////////////////////////////////////////////////////////////////////

// Find a cell (small or big). Returns 0 iff the pointer does not point to
// a valid cell.

static int
find_cell(hgc_ptr p, hgc_ptr *cl_ref, hgc_ptr *payload_ref,
    hgc_int *payload_size_ref)
{
    return
        find_small_cell(p, cl_ref, payload_ref, payload_size_ref) ||
        find_big_cell(p, cl_ref, payload_ref, payload_size_ref);
}

// If p points to a cell in the heap, then mark that cell.

static void
mark_ptr_target(hgc_ptr p)
{
    hgc_ptr cl;
    hgc_ptr payload;
    hgc_int payload_size;
    p = untag(p);
    if (
        find_small_cell(p, &cl, &payload, &payload_size) &&
        payload <= p && p < payload + payload_size
    ) {
        mark_small_cell(cl);
        return;
    }
    if (
        find_big_cell(p, &cl, &payload, &payload_size) &&
        payload <= p && p < payload + payload_size
    ) {
        mark_big_cell(cl);
        return;
    }
}

// Perform a single-pass mark and sweep of the alloc_list.
// If we fail to collect enough memory, allow extra pages by
// extending small_high_water.

static void
mark_and_sweep_alloc_list(void)
{
    hgc_ptr pg;
    hgc_ptr cl;
    hgc_ptr next_cl;
    hgc_ptr payload;
    hgc_ptr p;
    hgc_ptr *alloc_list_tail;
    hgc_int payload_size;
    hgc_int small_live_mem = 0;
    hgc_int small_dead_mem = 0;
    hgc_int big_live_mem = 0;
    hgc_int big_dead_mem = 0;

    TRC printf("hgc: mark_and_sweep_alloc_list()\n");

    TRC {
        int n;
        for (
            n = 0, cl = alloc_list;
            cl != NULL;
            n++, cl = (cl < small_top) ? get_small_cell_next(cl)
                                       : get_big_cell_next(cl)
        ) {}
        printf("hgc: mark_and_sweep_alloc_list: %l cells on alloc_list\n", n);
    }

    cl = alloc_list;
    alloc_list = NULL;
    alloc_list_tail = &alloc_list;
    while (cl != NULL) {
        if (cl < small_top) {
            // This is a small_cell.

            next_cl = get_small_cell_next(cl);
            pg = page(cl);
            payload_size = *page_payload_size(pg);

            if (small_cell_is_marked(cl)) {
                payload = small_cell_payload(cl);
                for (p = payload; p < payload + payload_size; p++) {
                    mark_ptr_target((hgc_ptr)(*p));
                }
                *alloc_list_tail = cl;
                alloc_list_tail = small_cell_next(cl);
                small_live_mem += payload_size;
            } else {
                set_small_cell_next(cl, small_cell_free_list[payload_size]);
                small_cell_free_list[payload_size] = cl;
                small_dead_mem += payload_size;
            }
        } else {
            // This is a big_cell.

            next_cl = get_big_cell_next(cl);
            payload = big_cell_payload(cl);
            payload_size = *big_cell_payload_size(cl);

            if (big_cell_is_marked(cl)) {
                for (p = payload; p < payload + payload_size; p++) {
                    mark_ptr_target((hgc_ptr)(*p));
                }
                *alloc_list_tail = cl;
                alloc_list_tail = big_cell_next(cl);
                big_live_mem += payload_size;

            } else {
                free_big_cell(cl);
                big_dead_mem += payload_size;
            }
        }

        cl = next_cl;
    }

    // Important! We have to terminate the alloc_list properly.
    *alloc_list_tail = NULL;

    TRC {
        check_big_heap();
        print_big_heap();
        printf("hgc: mark_and_sweep_alloc_list: "
                "%l live words in small_cells\n",
                small_live_mem);
        printf("hgc: mark_and_sweep_alloc_list: "
                "%l dead words in small_cells\n",
                small_dead_mem);
        printf("hgc: mark_and_sweep_alloc_list: "
                "%l live words in big_cells\n",
                big_live_mem);
        printf("hgc: mark_and_sweep_alloc_list: "
                "%l dead words in big_cells\n",
                big_dead_mem);
        printf("hgc: mark_and_sweep_alloc_list: small_bot = %p\n",
                small_bot);
        printf("hgc: mark_and_sweep_alloc_list: small_top = %p\n",
                small_top);
        printf("hgc: mark_and_sweep_alloc_list: small_h/w = %p\n",
                small_high_water);
        printf("hgc: mark_and_sweep_alloc_list: big_top   = %p\n",
                big_top);
        printf("hgc: mark_and_sweep_alloc_list: big_bot   = %p\n",
                big_bot);
        printf("hgc: mark_and_sweep_alloc_list: big_l/w   = %p\n",
                big_low_water);
        printf("\n");
    }
}

// Look up an environment variable. Return a default value if the envvar
// is absent or negative.

static hgc_int
lookup_env_var(const char *name, hgc_int default_value)
{
    char *envvar = getenv(name);
    if (envvar == NULL) {
        return default_value;
    } else {
        hgc_int val = atoi(envvar);
        if (val < 0) {
            return default_value;
        } else {
            return val;
        }
    }
}

// Initialise the heap.

void
MR_hgc_init(void)
{
    hgc_int heap_size;                  // In words.
    hgc_int initial_small_heap_size;    // In words.
    hgc_int initial_big_heap_size;      // In words.
    hgc_int initial_heap_size;          // In words.
    hgc_ptr heap_midpoint;
    int i;
    char *envvar;

    // Just return if we have already been initialised.

    if (hgc_initialised) {
        return;
    }

    // Set stack_bot if it hasn't already been fixed.

    if (stack_bot == NULL) {
        stack_bot = (hgc_ptr *)(&heap_size);
    }

    // Check the environment variables.

    heap_size =
        words(lookup_env_var("HGC_HEAP_SIZE",
                    default_HGC_HEAP_SIZE) << 20);

    growth_threshold =
        lookup_env_var("HGC_GROWTH_THRESHOLD",
                default_HGC_GROWTH_THRESHOLD);

    growth_factor =
        lookup_env_var("HGC_GROWTH_FACTOR",
                default_HGC_GROWTH_FACTOR);

    initial_small_heap_size =
        words(lookup_env_var("HGC_INITIAL_SMALL_HEAP_SIZE",
                    default_HGC_INITIAL_SMALL_HEAP_SIZE) << 20);

    initial_big_heap_size =
        words(lookup_env_var("HGC_INITIAL_BIG_HEAP_SIZE",
                    default_HGC_INITIAL_BIG_HEAP_SIZE) << 20);

    initial_heap_size = initial_small_heap_size + initial_big_heap_size;

    // Ask for as much memory as malloc will give us, give or take a factor
    // of two.

    for (
        heap_bot = NULL;
        heap_bot == NULL && heap_size >= initial_heap_size;
        heap_size /= 2
    ) {
        heap_bot = (hgc_ptr)malloc(heap_size * sizeof(hgc_word));
    }
    if (heap_bot == NULL) {
        fprintf(stderr, "hgc: failed to malloc heap.\n");
        exit(1);
    }
    TRC printf("hgc: hgc_init: allocated %l MByte heap.\n",
        (heap_size * sizeof(hgc_word)) >> 20);

    // Set up the small heap (this grows upwards).

    heap_top = heap_bot + heap_size;
    heap_midpoint = heap_bot + (heap_size / 2);
    small_bot = page(heap_bot + page_size - 1);
    small_top = small_bot;
    small_high_water = small_bot + initial_small_heap_size;
    for (i = 0; i <= max_small_payload; i++) {
        small_cell_free_list[i] = NULL;
    }

    // Set up the big heap (this grows downwards). We have to create the
    // initial empty cell and add it to the right free list.

    for (i = 0; i <= max_big_cell_size_index; i++) {
        big_cell_free_list[i] = NULL;
    }
    big_size_index = min_big_cell_size_index;
    if (big_low_water < small_high_water) {
        big_low_water = small_high_water;
    }
    big_top = heap_top;
    big_bot = heap_top - (1 << min_big_cell_size_index);
    *big_cell_size_index(big_bot) = min_big_cell_size_index;
    add_big_cell_to_free_list(big_bot);
    big_low_water = big_top - initial_big_heap_size;

    // Set up the roots and mutables arrays.

    roots = init_roots_array();
    mutables = init_mutables_array();
    uncollectables = init_uncollectables_array();

    // Report that all is well.

    hgc_initialised = 1;
}

// Add a roots range.

void
MR_hgc_add_roots_range(void *bot, void *top)
{
    struct roots_range rr = {bot, top};
    TRC if (!hgc_initialised) {
        fprintf(stderr, "hgc: hgc_init() has not been called.\n");
        exit(1);
    }
    roots = roots_array_append_item(roots, rr);
}

void
MR_hgc_add_root(void *rt)
{
    MR_hgc_add_roots_range(rt, rt + 1);
}

// Mark any cells referred to on the C stack.

static void
mark_from_C_stack(void)
{
    hgc_ptr *stack_top = (hgc_ptr *)(&stack_top);
    hgc_ptr *bot = (stack_bot < stack_top) ? stack_bot : stack_top;
    hgc_ptr *top = (stack_bot < stack_top) ? stack_top : stack_bot;
    hgc_ptr *p;

    TRC printf("hgc: mark_from_C_stack()\n");

    for (p = bot; p <= top; p++) {
        mark_ptr_target(*p);
    }
}

// Mark any cells referred to in registers. This is currently very Linux
// centric.

static void
mark_from_registers(void)
{
    jmp_buf jbuf;
    hgc_ptr *bot;
    hgc_ptr *top;
    hgc_ptr *p;

    TRC printf("hgc: mark_from_registers()\n");

    (void)setjmp(jbuf);                 // Copy the register file.
    bot = (hgc_ptr *)(jbuf[0].__jmpbuf);
    top = (hgc_ptr *)(&(jbuf[0].__mask_was_saved));

    TRC printf("hgc: mark_from_registers: %l registers\n", top - bot);

    for (p = bot; p <= top; p++) {
        mark_ptr_target(*p);
    }
}

// Mark any cells referred to in the roots array ranges.

static void
mark_from_roots(void)
{
    int i;

    TRC printf("hgc: mark_from_roots()\n");

    for (i = roots_array_num_occupants(roots) - 1; i >= 0; i--) {
        struct roots_range rr = roots_array_get_item(roots, i);
        hgc_ptr *p;
        for (p = rr.roots_bot; p < rr.roots_top; p++) {
            mark_ptr_target(*p);
        }
    }
}

// Mark a cell and any cells referenced in its payload.

static void
mark_cell_and_referents(hgc_ptr p)
{
    hgc_ptr cl;
    hgc_ptr payload;
    hgc_int payload_size;
    if (find_small_cell(p, &cl, &payload, &payload_size)) {
        mark_small_cell(cl);
        for (p = payload; p < payload + payload_size; p++) {
            mark_ptr_target((hgc_ptr)*p);
        }
        return;
    }
    if (find_big_cell(p, &cl, &payload, &payload_size)) {
        mark_big_cell(cl);
        for (p = payload; p < payload + payload_size; p++) {
            mark_ptr_target((hgc_ptr)*p);
        }
        return;
    }
}

// Mark any uncollectable cells and their referents.

static void
mark_from_uncollectables(void)
{
    int i;
    int n = uncollectables_array_num_occupants(uncollectables);

    TRC printf("hgc: mark_from_uncollectables()\n");

    for (i = 0; i < n; i++) {
        mark_cell_and_referents(
            uncollectables_array_get_item(uncollectables, i)
        );
    }
}

// Mark any cells referred to in the mutables array and any cells
// referred to by those mutables.

static void
mark_from_mutables(void)
{
    int i;
    int n = mutables_array_num_occupants(mutables);

    TRC printf("hgc: mark_from_mutables()\n");

    for (i = 0; i < n; i++) {
        mark_cell_and_referents(
            mutables_array_get_item(mutables, i)
        );
    }
}

// Perform a garbage collection.

void
MR_hgc_gc(void)
{
    TRC if (!hgc_initialised) {
        fprintf(stderr, "hgc: hgc_init() has not been called.\n");
        exit(1);
    }
    TRC printf("hgc: hgc_gc()\n");
    mark_from_roots();
    mark_from_C_stack();
    mark_from_registers();
    mark_from_uncollectables();
    mark_from_mutables();
    mark_and_sweep_alloc_list();
}

// Malloc replacement.

void *
MR_hgc_malloc_immutable(size_t size_bytes)
{
    hgc_int size = words(size_bytes);

    TRC if (!hgc_initialised) {
        fprintf(stderr, "hgc: hgc_init() has not been called.\n");
        exit(1);
    }
    if (size <= max_small_payload) {
        return small_cell_payload(alloc_small_cell(size));
    } else {
        return big_cell_payload(alloc_big_cell(size));
    }
}

// Add a cell to the mutables array.

void
MR_hgc_make_mutable(void *p)
{
    mutables = mutables_array_append_item(mutables, (hgc_ptr) p);
}

// Allocate a mutable cell.

void *
MR_hgc_malloc_mutable(size_t size_bytes)
{
    void *p = MR_hgc_malloc_immutable(size_bytes);
    MR_hgc_make_mutable(p);
    return p;
}

// Make a cell uncollectable (i.e. always live).

void
MR_hgc_make_uncollectable(void *p)
{
    uncollectables = uncollectables_array_append_item(uncollectables, p);
}

// Allocate an uncollectable, mutable cell.

void *
MR_hgc_malloc_uncollectable(size_t size_bytes)
{
    void *p = MR_hgc_malloc_mutable(size_bytes);
    MR_hgc_make_uncollectable(p);
    return p;
}

// Allow the application to change stack_bot.

void
MR_hgc_set_stack_bot(void *p)
{
    stack_bot = (hgc_ptr *)untag((hgc_ptr) p);
}

// If necessary, allocate a new cell of the given size and copy as much of the
// data from the old cell as will fit. The new cell is mutable if the old cell
// is. The new cell is uncollectable if the old cell is.

void *
MR_hgc_realloc(void *p, size_t size_bytes)
{
    hgc_int new_payload_size = words(size_bytes);
    hgc_ptr old_cl;
    hgc_ptr old_payload;
    hgc_int old_payload_size;
    if (find_cell(p, &old_cl, &old_payload, &old_payload_size)) {
        if (old_payload_size < new_payload_size) {
            hgc_ptr new_payload =
                MR_hgc_malloc_immutable(new_payload_size);
            hgc_int i;
            for (i = 0; i < old_payload_size; i++) {
                new_payload[i] = old_payload[i];
            }
            for (i = 0; i < mutables_array_num_occupants(mutables); i++) {
                if (mutables_array_get_item(mutables, i) == old_payload) {
                    mutables_array_set_item(mutables, i, new_payload);
                    break;
                }
            }
            for (
                i = 0;
                i < uncollectables_array_num_occupants(uncollectables);
                i++
            ) {
                if (uncollectables_array_get_item(uncollectables, i)
                    == old_payload)
                {
                    uncollectables_array_set_item(uncollectables, i,
                        new_payload);
                    break;
                }
            }
            return new_payload;
        } else {
            return old_payload;
        }
    } else {
        fprintf(stderr, "hgc: hgc_realloc(): not an allocated cell.\n");
        exit(1);
    }
}

// "Free" a cell by either NULLing its payload (small cells) or reducing
// its payload size to zero (big cells).

void
MR_hgc_free(void *p)
{
    hgc_ptr cl;
    hgc_ptr payload;
    hgc_int payload_size;
    p = untag(p);
    if (find_small_cell(p, &cl, &payload, &payload_size)) {
        hgc_ptr payload_ptr;
        for (payload_ptr = payload;
            payload_ptr < payload + payload_size;
            payload_ptr++)
        {
            *payload_ptr = 0;
        }
        return;
    }
    if (find_big_cell(p, &cl, &payload, &payload_size)) {
        *big_cell_payload_size(cl) = 0;
        return;
    }
    fprintf(stderr, "hgc: hgc_free(): not an allocated cell.\n");
    exit(1);
}

#endif // MR_HGC
