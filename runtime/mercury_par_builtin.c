// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2009, 2011 The University of Melbourne.
// Copyright (C) 2015-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#include "mercury_types.h"
#include "mercury_par_builtin.h"

////////////////////////////////////////////////////////////////////////////
//
// Futures for decedent AND-parallelism..
//
////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////
//
// Code for Loop controls..
//
////////////////////////////////////////////////////////////////////////////

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
        // We allocate contexts as necessary, so that we never allocate a
        // context we don't use. Also, by allocating the contexts in
        // MR_lc_spawn_off, already spawned off computations can run in
        // parallel with the allocation of contexts for computations that are
        // about to be spawned off.

        lc->MR_lc_slots[i].MR_lcs_context = NULL;
        lc->MR_lc_slots[i].MR_lcs_is_free = MR_TRUE;
    }
    lc->MR_lc_outstanding_workers = 0;
    lc->MR_lc_master_context_lock = MR_US_LOCK_INITIAL_VALUE;
    lc->MR_lc_master_context = NULL;
    lc->MR_lc_finished = MR_FALSE;
    lc->MR_lc_free_slot_hint = 0;

#ifdef MR_DEBUG_LOOP_CONTROL
    fprintf(stderr, "lc_create(%d) -> %p)\n", num_workers, lc);
#endif

    return lc;
}

// Deprecated, this was part of our old loop control design.

MR_Bool
MR_lc_try_get_free_slot(MR_LoopControl *lc, MR_Unsigned *lcs_idx)
{
    if (lc->MR_lc_outstanding_workers == lc->MR_lc_num_slots) {
        return MR_FALSE;
    } else {
        unsigned hint, offset, i;

        // We start indexing into the array starting at this hint, it is either
        // set to a known free slot or the next unchecked slot after finding a
        // free slot.

        hint = lc->MR_lc_free_slot_hint;

        for (offset = 0; offset < lc->MR_lc_num_slots; offset++) {
            i = (hint + offset) % lc->MR_lc_num_slots;
            if (lc->MR_lc_slots[i].MR_lcs_is_free) {
                lc->MR_lc_slots[i].MR_lcs_is_free = MR_FALSE;
                lc->MR_lc_free_slot_hint = (i+1) % lc->MR_lc_num_slots;
                MR_atomic_inc_int(&(lc->MR_lc_outstanding_workers));
                *lcs_idx = i;
                return MR_TRUE;
            }
        }

        return MR_FALSE;
    }
}

void
MR_lc_spawn_off_func(MR_LoopControl *lc, MR_Unsigned lcs_idx, MR_Code
        *code_ptr)
{
    MR_LoopControlSlot *lcs = &(lc->MR_lc_slots[lcs_idx]);

#if MR_DEBUG_LOOP_CONTROL
    fprintf(stderr, "lc_spawn_off(%p, %d, %p) sp: %p\n",
        lc, lcs_idx, code_ptr, MR_sp);
#endif

    lcs->MR_lcs_context->MR_ctxt_resume = code_ptr;
    lcs->MR_lcs_context->MR_ctxt_parent_sp = MR_sp;
    MR_schedule_context(lcs->MR_lcs_context);
}

void
MR_lc_join(MR_LoopControl *lc, MR_Unsigned lcs_idx)
{
    MR_LoopControlSlot  *lcs;
    MR_bool             last_worker;
    MR_Context          *wakeup_context;

    lcs = &(lc->MR_lc_slots[lcs_idx]);

#ifdef MR_DEBUG_LOOP_CONTROL
    fprintf(stderr, "lc_join(%p, %d)\n", lc, lcs_idx);
#endif

    lcs->MR_lcs_is_free = MR_TRUE;
    lc->MR_lc_free_slot_hint = lcs_idx;
    // Ensure the slot is free before we perform the decrement.
    MR_CPU_SFENCE;

    // We have to determine if we are either the last of first workers to
    // finish. To do this we cannot use atomic decrement since we need to do
    // more than one comparison. We therefore use a CAS.

    last_worker =
        MR_atomic_dec_and_is_zero_int(&(lc->MR_lc_outstanding_workers));

    // If this is the last worker to finish, then take the lock before checking
    // the master context field, otherwise we might race and end up never
    // resuming the master.

    if (last_worker) {
        MR_US_SPIN_LOCK(&(lc->MR_lc_master_context_lock));
        // Don't read the master field until after we have the lock.

        MR_CPU_MFENCE;
    }

    // If the master thread is suspended, wake it up, provided that either:
    // - the loop has finished and this is the last worker to exit, or
    // - the loop has not finished (so the master can create more work).

    if ((lc->MR_lc_master_context != NULL) &&
        ((lc->MR_lc_finished && last_worker) || (!lc->MR_lc_finished)))
    {
        // Now take a lock and re-read the master context field.

        if (!last_worker) {
            MR_US_SPIN_LOCK(&(lc->MR_lc_master_context_lock));
            // Don't read the master field until after we have the lock.

            MR_CPU_MFENCE;
        }
        wakeup_context = lc->MR_lc_master_context;
        lc->MR_lc_master_context = NULL;
        MR_CPU_SFENCE; // Make sure the field is set to NULL in memory.
        MR_US_UNLOCK(&(lc->MR_lc_master_context_lock));
        if (wakeup_context != NULL) {
#ifdef MR_DEBUG_LOOP_CONTROL
            fprintf(stderr, "Waking up master\n");
#endif
            // XXX: It is faster to switch to this context ourselves
            // since we are going to unload our own context.
            // Or we should switch to another worker context if there is one.

            MR_schedule_context(wakeup_context);
        }
    } else if (last_worker) {
        MR_US_UNLOCK(&(lc->MR_lc_master_context_lock));
    }
}

#endif // MR_THREAD_SAFE && MR_LL_PARALLEL_CONJ
