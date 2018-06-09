// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2007, 2010-2011 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_wsdeque.c
//
// This file implements the basic algorithm from David Chase, Yossi Lev:
// "Dynamic circular work-stealing deque". SPAA 2005: 21-28.
//
// A work-stealing deque is a double ended queue in which only one thread can
// access one end of the queue (the bottom) while other threads can only pop
// elements from the other end (the top).
//
// XXX: We need to insert memory barriers in the right places.

#include "mercury_imp.h"
#include "mercury_memory_handlers.h"
#include "mercury_context.h"
#include "mercury_wsdeque.h"

////////////////////////////////////////////////////////////////////////////

#ifdef MR_LL_PARALLEL_CONJ

static MR_SparkArray *
MR_alloc_spark_array(MR_Integer size);

void
MR_init_wsdeque(MR_SparkDeque *dq, MR_Integer size)
{
    dq->MR_sd_bottom = 0;
    dq->MR_sd_top = 0;
    dq->MR_sd_active_array = MR_alloc_spark_array(size);
}

MR_bool
MR_wsdeque_is_empty(const MR_SparkDeque *dq)
{
    return dq->MR_sd_bottom == dq->MR_sd_top;
}

void
MR_wsdeque_putback_bottom(MR_SparkDeque *dq, const MR_Spark *spark)
{
    MR_Integer              bot;
    volatile MR_SparkArray  *arr;

    bot = dq->MR_sd_bottom;
    arr = dq->MR_sd_active_array;

    MR_sa_element(arr, bot) = *spark;
    dq->MR_sd_bottom = bot + 1;
}

int
MR_wsdeque_steal_top(MR_SparkDeque *dq, MR_Spark *ret_spark)
{
    MR_Integer              top;
    MR_Integer              bot;
    volatile MR_SparkArray  *arr;
    MR_Integer              size;

    top = dq->MR_sd_top;
    bot = dq->MR_sd_bottom;
    arr = dq->MR_sd_active_array;
    size = bot - top;

    if (size <= 0) {
        return 0;   // empty
    }

    *ret_spark = MR_sa_element(arr, top);
    if (!MR_compare_and_swap_int(&dq->MR_sd_top, top, top + 1)) {
        return -1;  // abort
    }

    return 1;       // success
}

int
MR_wsdeque_take_top(MR_SparkDeque *dq, MR_Spark *ret_spark)
{
    MR_Integer              top;
    MR_Integer              bot;
    volatile MR_SparkArray  *arr;
    MR_Integer              size;

    top = dq->MR_sd_top;
    bot = dq->MR_sd_bottom;
    arr = dq->MR_sd_active_array;

    size = bot - top;
    if (size <= 0) {
        return 0;   // empty
    }

    *ret_spark = MR_sa_element(arr, top);
    dq->MR_sd_top = top + 1;
    return 1;       // success
}

static MR_SparkArray *
MR_alloc_spark_array(MR_Integer size)
{
    MR_SparkArray   *arr;
    size_t          num_bytes;

    num_bytes = sizeof(MR_SparkArray) + (size - 1) * sizeof(MR_Spark);
    arr = MR_GC_malloc_attrib(num_bytes, MR_ALLOC_SITE_RUNTIME);
    arr->MR_sa_max = size - 1;
    return arr;
}

MR_SparkArray *
MR_grow_spark_array(const MR_SparkArray *old_arr, MR_Integer bot,
        MR_Integer top)
{
    MR_Integer      new_size;
    MR_SparkArray    *new_arr;
    MR_Integer      i;

    new_size = 2 * (old_arr->MR_sa_max + 1);
    new_arr = MR_alloc_spark_array(new_size);

    for (i = top; i < bot; i++) {
        MR_sa_element(new_arr, i) = MR_sa_element(old_arr, i);
    }

    return new_arr;
}

#endif // !MR_LL_PARALLEL_CONJ
