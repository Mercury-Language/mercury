/*
vim: ft=c ts=4 sw=4 et
*/
/*
** Copyright (C) 2009, 2011 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include "mercury_types.h"
#include "mercury_par_builtin.h"

/***************************************************************************
**
** Futures for decedent AND-parallelism..
**
***************************************************************************/

#ifdef MR_CONSERVATIVE_GC

void
MR_finalize_future(void *obj, void *cd)
{
    MR_Future *future;

    future = (MR_Future *) obj;

  #ifdef MR_THREAD_SAFE
    pthread_mutex_destroy(&(future->MR_fut_lock));
  #endif
}

#endif

/***************************************************************************
**
** Code for Loop controls..
**
***************************************************************************/

#if defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ)

MR_LoopControl *
MR_lc_create(unsigned num_workers)
{
    MR_LoopControl* lc;
    unsigned        i;

    lc = MR_GC_malloc(sizeof(MR_LoopControl) +
        (num_workers-1) * sizeof(MR_LoopControlSlot));
    for (i = 0; i < num_workers; i++) {
        /*
        ** We allocate contexts as necessary, so that we never allocate a
        ** context we don't use.  Also by delaying the allocation of contexts
        ** all but the first may execute in parallel with one-another.
        */
        lc->MR_lc_slots[i].MR_lcs_context = NULL;
        lc->MR_lc_slots[i].MR_lcs_is_free = MR_TRUE;
    }
    lc->MR_lc_num_slots = num_workers;
    lc->MR_lc_outstanding_workers = 0;
    lc->MR_lc_waiting_context = NULL;
    pthread_mutex_init(&(lc->MR_lc_lock), MR_MUTEX_ATTR);

    return lc;
}

MR_LoopControlSlot *
MR_lc_try_get_free_slot(MR_LoopControl* lc)
{
    if (lc->MR_lc_outstanding_workers == lc->MR_lc_num_slots) {
        return NULL;
    } else {
        unsigned i;

        /*
        ** Optimize this by using a hint to start the search at.
        */
        for (i = 0; i<lc->MR_lc_num_slots; i++) {
            if (lc->MR_lc_slots[i].MR_lcs_is_free) {
                lc->MR_lc_slots[i].MR_lcs_is_free = MR_FALSE;
                MR_atomic_inc_int(&(lc->MR_lc_outstanding_workers));
                return &(lc->MR_lc_slots[i]);
            }
        }

        return NULL;
    }
}

void
MR_lc_spawn_off_func(MR_LoopControlSlot* lcs, MR_Code* code_ptr)
{
    if (lcs->MR_lcs_context == NULL) {
        /*
        ** We need a new context.
        */
        lcs->MR_lcs_context = MR_create_context("Loop control",
            MR_CONTEXT_SIZE_FOR_LOOP_CONTROL_WORKER, NULL);
        lcs->MR_lcs_context->MR_ctxt_thread_local_mutables =
            MR_THREAD_LOCAL_MUTABLES;
    }

    lcs->MR_lcs_context->MR_ctxt_resume = code_ptr;
    lcs->MR_lcs_context->MR_ctxt_parent_sp = MR_sp;
    MR_schedule_context(lcs->MR_lcs_context);
}

void
MR_lc_join(MR_LoopControl* lc, MR_LoopControlSlot* lcs)
{
    MR_bool     last_worker;
    MR_Context  *wakeup_context;

    lcs->MR_lcs_is_free = MR_TRUE;
    last_worker =
        MR_atomic_dec_and_is_zero_int(&(lc->MR_lc_outstanding_workers));

    /*
    ** This barrier ensures we update MR_lc_outstanding_contexts before
    ** we read MR_lc_finished. It works together with another barrier
    ** in MR_lc_finish(). Together these barriers prevent a race whereby
    ** the original thread is not resumed because MR_lc_finished looked false
    ** in the condition below but last_worker was true, and the original
    ** thread is about to go to sleep.
    **
    ** We go through these checks to avoid taking the lock in the then branch
    ** below in cases when MR_lc_outstanding_workers is zero but the original
    ** thread has not called MR_lc_finish() yet.
    */
    MR_CPU_MFENCE;
    if (last_worker && lc->MR_lc_finished) {
        /*
        ** Wake up the first thread if it is sleeping.
        ** XXX: a spinlock would do here, or maybe a CAS;
        ** we never hold the lock for long.
        */
        MR_LOCK(&(lc->MR_lc_lock), "MC_lc_join_and_terminate");
        wakeup_context = lc->MR_lc_waiting_context;
        /*
        ** We don't need to clear the context field at this point: only one
        ** worker can ever be the last worker, and therefore there is no danger
        ** in adding this context to the run queue twice.
        */
        MR_UNLOCK(&(lc->MR_lc_lock), "MR_lc_join_and_terminate");
        if (wakeup_context != NULL) {
            /*
            ** XXX: it is faster to switch to this context ourselves
            ** since we are going to unload our own context.
            */
            MR_schedule_context(wakeup_context);
        }
    }
}

#endif /* MR_THREAD_SAFE && MR_LL_PARALLEL_CONJ */

