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
    MR_LoopControl  *lc;
    unsigned        i;

    lc = MR_GC_malloc(sizeof(MR_LoopControl) +
        (num_workers-1) * sizeof(MR_LoopControlSlot));
    lc->MR_lc_num_slots = num_workers;
    for (i = 0; i < num_workers; i++) {
        /*
        ** We allocate contexts as necessary, so that we never allocate a
        ** context we don't use. Also, by delaying the allocation of contexts,
        ** all but the first may execute in parallel with one-another.
        ** XXX I (zs) do not understand how the first half of the previous
        ** sentence implies the seconf half; I don't think it does.
        */
        lc->MR_lc_slots[i].MR_lcs_context = NULL;
        lc->MR_lc_slots[i].MR_lcs_is_free = MR_TRUE;
    }
    lc->MR_lc_outstanding_workers = 0;
    lc->MR_lc_master_context_lock = MR_US_LOCK_INITIAL_VALUE;
    lc->MR_lc_master_context = NULL;
    lc->MR_lc_finished = MR_FALSE;

    return lc;
}

/*
** Deprecated, this was part of our old loop control design.
*/
MR_LoopControlSlot *
MR_lc_try_get_free_slot(MR_LoopControl *lc)
{
    if (lc->MR_lc_outstanding_workers == lc->MR_lc_num_slots) {
        return NULL;
    } else {
        unsigned i;

        /*
        ** XXX Optimize this by using a hint to start the search at.
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
MR_lc_spawn_off_func(MR_LoopControlSlot *lcs, MR_Code *code_ptr)
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
MR_lc_join(MR_LoopControl *lc, MR_LoopControlSlot *lcs)
{
    MR_bool     last_worker;
    MR_Context  *wakeup_context;

    lcs->MR_lcs_is_free = MR_TRUE;
    /* Ensure the slot is free before we perform the decrement. */
    MR_CPU_SFENCE;
    last_worker =
        MR_atomic_dec_and_is_zero_int(&(lc->MR_lc_outstanding_workers));

    /*
    ** If the master thread is suspended, wake it up, provided that either:
    ** - the loop has finished and this is the last worker to exit, or
    ** - the loop has not finished (so the master can create more work).
    */
    if ((lc->MR_lc_master_context != NULL) &&
        ((lc->MR_lc_finished && last_worker) || (!lc->MR_lc_finished)))
    {
        /*
        ** Now take a lock and re-read the master context field.
        */
        MR_US_SPIN_LOCK(&(lc->MR_lc_master_context_lock));
        wakeup_context = lc->MR_lc_master_context;
        lc->MR_lc_master_context = NULL;
        MR_US_UNLOCK(&(lc->MR_lc_master_context_lock));
        if (wakeup_context != NULL) {
            /*
            ** XXX: it is faster to switch to this context ourselves
            ** since we are going to unload our own context.
            ** Or we should switch to another worker context if there is one.
            */
            MR_schedule_context(wakeup_context);
        }
    }
}

#endif /* MR_THREAD_SAFE && MR_LL_PARALLEL_CONJ */

