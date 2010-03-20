/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2007, 2009-2010 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef MERCURY_WSDEQUE_H
#define MERCURY_WSDEQUE_H

#ifdef MR_LL_PARALLEL_CONJ

#include "mercury_atomic_ops.h"

/* XXX should experiment with these */
#define MR_INITIAL_GLOBAL_SPARK_QUEUE_SIZE  4
#define MR_INITIAL_LOCAL_SPARK_DEQUE_SIZE   8

/*---------------------------------------------------------------------------*/

/* See mercury_context.h for the definition of MR_SparkDeque. */

struct MR_SparkArray_Struct {
    MR_Integer          MR_sa_max;          /* power of two - 1 */
    volatile MR_Spark   MR_sa_segment[1];   /* really MR_sa_max + 1 */
};

/*
** MR_sa_element(Array, Pos)
** Index into Array modulo its size, i.e. treating it as a circular array.
**
** MR_sa_max is a power of two - 1 so that we can use a bitwise AND operation
** operation instead of modulo when indexing into the array, which makes a
** significant difference.
*/
#define MR_sa_element(arr, pos) \
                ((arr)->MR_sa_segment[(pos) & (arr)->MR_sa_max])

/*---------------------------------------------------------------------------*/

/*
** Initialise a spark deque.  A new circular array underlying the deque will
** only be allocated if deque->MR_sd_active_array is NULL, otherwise only the
** indices into the array will be reset.  `size' must be a power of two.
*/
extern  void    MR_init_wsdeque(MR_SparkDeque *dq, MR_Integer size);

/*
** Return true if the deque is empty.
*/
extern  MR_bool MR_wsdeque_is_empty(const MR_SparkDeque *dq);

/*
** Push a spark on the bottom of the deque.  Must only be called by the owner
** of the deque.  The deque may grow as necessary.
*/
MR_INLINE
void            MR_wsdeque_push_bottom(MR_SparkDeque *dq,
                    const MR_Spark *spark);

/*
** Same as MR_wsdeque_push_bottom but assume that there is enough space
** in the deque.  Should only be called after a successful pop.
*/
extern  void    MR_wsdeque_putback_bottom(MR_SparkDeque *dq,
                    const MR_Spark *spark);

/*
** Pop a spark off the bottom of the deque.  Must only be called by
** the owner of the deque.  Returns true if successful.
*/
MR_INLINE MR_bool
MR_wsdeque_pop_bottom(MR_SparkDeque *dq, MR_Code **ret_spark_resume);

/*
** Attempt to steal a spark from the top of the deque.
**
** Returns:
**   1 on success,
**   0 if the deque is empty or
**  -1 if the steal was aborted due to a concurrent steal or pop_bottom.
*/
extern  int     MR_wsdeque_steal_top(MR_SparkDeque *dq, MR_Spark *ret_spark);

/*
** Take a spark from the top of the deque, assuming there are no concurrent
** operations on the deque.  Returns true on success.
*/
extern  int     MR_wsdeque_take_top(MR_SparkDeque *dq, MR_Spark *ret_spark);

/*
** Return a new circular array with double the capacity of the old array.
** The valid elements of the old array are copied to the new array.
*/
extern  MR_SparkArray * MR_grow_spark_array(const MR_SparkArray *old_arr,
                            MR_Integer bot, MR_Integer top);

/*---------------------------------------------------------------------------*/

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
    dq->MR_sd_bottom = bot + 1;
}

MR_INLINE MR_bool
MR_wsdeque_pop_bottom(MR_SparkDeque *dq, MR_Code **ret_spark_resume)
{
    MR_Integer              bot;
    MR_Integer              top;
    MR_Integer              size;
    volatile MR_SparkArray  *arr;
    MR_bool                 success;

    bot = dq->MR_sd_bottom;
    arr = dq->MR_sd_active_array;
    bot--;
    dq->MR_sd_bottom = bot;

    top = dq->MR_sd_top;
    size = bot - top;

    if (size < 0) {
        dq->MR_sd_bottom = top;
        return MR_FALSE;
    }

    (*ret_spark_resume) = MR_sa_element(arr, bot).MR_spark_resume;
    if (size > 0) {
        return MR_TRUE;
    }

    /* size = 0 */
    success = MR_compare_and_swap_int(&dq->MR_sd_top, top, top + 1);
    dq->MR_sd_bottom = top + 1;
    return success;
}

#endif /* !MR_LL_PARALLEL_CONJ */

#endif /* !MERCURY_WSDEQUE_H */
