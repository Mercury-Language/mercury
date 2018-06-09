// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2007, 2009-2011 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#ifndef MERCURY_WSDEQUE_H
#define MERCURY_WSDEQUE_H

#ifdef MR_LL_PARALLEL_CONJ

#include "mercury_atomic_ops.h"

// XXX Should experiment with this, perhaps it should be configurable.
#define MR_INITIAL_SPARK_DEQUE_SIZE   8

////////////////////////////////////////////////////////////////////////////

// See mercury_context.h for the definition of MR_SparkDeque.

struct MR_SparkArray_Struct {
    MR_Integer          MR_sa_max;          // power of two - 1
    volatile MR_Spark   MR_sa_segment[1];   // really MR_sa_max + 1
};

// MR_sa_element(Array, Pos)
// Index into Array modulo its size, i.e. treating it as a circular array.
//
// MR_sa_max is a power of two - 1 so that we can use a bitwise AND operation
// operation instead of modulo when indexing into the array, which makes a
// significant difference.

#define MR_sa_element(arr, pos)                                         \
                ((arr)->MR_sa_segment[(pos) & (arr)->MR_sa_max])

////////////////////////////////////////////////////////////////////////////

// Initialise a spark deque. A new circular array underlying the deque will
// only be allocated if deque->MR_sd_active_array is NULL, otherwise only the
// indices into the array will be reset. `size' must be a power of two.

extern  void        MR_init_wsdeque(MR_SparkDeque *dq, MR_Integer size);

// Return true if the deque is empty.

extern  MR_bool     MR_wsdeque_is_empty(const MR_SparkDeque *dq);

// Push a spark on the bottom of the deque. Must only be called by the owner
// of the deque. The deque may grow as necessary.

MR_INLINE void      MR_wsdeque_push_bottom(MR_SparkDeque *dq,
                        const MR_Spark *spark);

// Same as MR_wsdeque_push_bottom but assume that there is enough space
// in the deque. Should only be called after a successful pop.

extern  void        MR_wsdeque_putback_bottom(MR_SparkDeque *dq,
                        const MR_Spark *spark);

// Pop a spark off the bottom of the deque. Must only be called by the owner
// of the deque. The pointer returned here can be used until the next call to
// a MR_wsdeque function, at which point it's memory may have been overwritten.

MR_INLINE volatile MR_Spark *MR_wsdeque_pop_bottom(MR_SparkDeque *dq);

// Attempt to steal a spark from the top of the deque.
//
// Returns:
//   1 on success,
//   0 if the deque is empty or
//  -1 if the steal was aborted due to a concurrent steal or pop_bottom.

extern  int         MR_wsdeque_steal_top(MR_SparkDeque *dq,
                        MR_Spark *ret_spark);

// Take a spark from the top of the deque, assuming there are no concurrent
// operations on the deque. Returns true on success.

extern  int         MR_wsdeque_take_top(MR_SparkDeque *dq,
                        MR_Spark *ret_spark);

// Return a new circular array with double the capacity of the old array.
// The valid elements of the old array are copied to the new array.

extern  MR_SparkArray *MR_grow_spark_array(const MR_SparkArray *old_arr,
                        MR_Integer bot, MR_Integer top);

// Return the current length of the dequeue.
//
// This is safe from the owner's perspective.

MR_INLINE int       MR_wsdeque_length(MR_SparkDeque *dq);

////////////////////////////////////////////////////////////////////////////

MR_INLINE void
MR_wsdeque_push_bottom(MR_SparkDeque *dq, const MR_Spark *spark)
{
    MR_Integer              bot;
    MR_Integer              top;
    volatile MR_SparkArray  *arr;
    MR_Integer              size;

    bot = dq->MR_sd_bottom;
    top = dq->MR_sd_top;
    arr = dq->MR_sd_active_array;
    size = bot - top;

    if (size >= arr->MR_sa_max) {
        arr = MR_grow_spark_array((MR_SparkArray *) arr, bot, top);
        dq->MR_sd_active_array = arr;
    }

    MR_sa_element(arr, bot) = *spark;
    // Make sure the spark data is stored before we store the value of bottom.
    // We wouldn't want a thief to steal some stale data.

    MR_CPU_SFENCE;
    dq->MR_sd_bottom = bot + 1;
}

MR_INLINE volatile MR_Spark*
MR_wsdeque_pop_bottom(MR_SparkDeque *dq)
{
    MR_Integer              bot;
    MR_Integer              top;
    MR_Integer              size;
    volatile MR_SparkArray  *arr;
    MR_bool                 success;
    volatile MR_Spark       *spark;

    bot = dq->MR_sd_bottom;
    arr = dq->MR_sd_active_array;
    bot--;
    dq->MR_sd_bottom = bot;

    // bot must be written before we read top. If it is written after,
    // which may happen without the fence, then there is a race as follows.
    //
    // There are two items in the deque. (bot = 3, top = 1) This deque's
    // owner's CPU does not immediately write bottom into memory (above).
    // The owner thinks that bot=2 but memory says that bot = 3). Meanwhile
    // two other CPUs steal work, they see two items on the deque and each
    // increments top with its CAS (top = 3, the deque is empty). The original
    // engine continues, because it saw a deque with two items it does not do
    // a CAS on top and therefore takes an item from the deque.
    // The deque had 2 items but 3 have been taken!
    //
    // Thieves create a critical section between their initial read of top
    // (and bottom) and the CAS. Successful thieves are mutually excluded
    // from this section. Therefore thief 2 will not read the values for
    // top and bottom until after thief 1's CAS is a success (if it did read
    // these,  its own CAS would fail). Thief 2 is guaranteed to see the
    // update to bot or the owner is guaranteed to see thief 1's update to
    // top (possibly both). This means that either thief 2 will fail or the
    // owner will use a CAS and a race between it and the owner will decide
    // the victor. Either way, only thief 1 and one of the other two
    // engines can take an item from the deque.

    MR_CPU_MFENCE;

    top = dq->MR_sd_top;
    size = bot - top;

    if (size < 0) {
        dq->MR_sd_bottom = top;
        return NULL;
    }

    spark = &MR_sa_element(arr, bot);
    if (size > 0) {
        return spark;
    }

    // size = 0
    success = MR_compare_and_swap_int(&dq->MR_sd_top, top, top + 1);
    dq->MR_sd_bottom = top + 1;
    return success ? spark : NULL;
}

MR_INLINE int
MR_wsdeque_length(MR_SparkDeque *dq)
{
    int length;
    int top;
    int bot;

    top = dq->MR_sd_top;
    bot = dq->MR_sd_bottom;
    length = bot - top;

    return length;
}

#endif // !MR_LL_PARALLEL_CONJ

#endif // !MERCURY_WSDEQUE_H
