/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 1997-2001, 2003, 2005-2007, 2009-2010 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include "mercury_imp.h"
#include "mercury_regs.h"
#include "mercury_engine.h"
#include "mercury_memory.h"
#include "mercury_context.h"    /* for MR_do_runnext */
#include "mercury_thread.h"
#include "mercury_threadscope.h"

#include <stdio.h>
#include <errno.h>

#ifdef  MR_THREAD_SAFE
  MercuryThread     MR_primordial_thread;

  MercuryThreadKey  MR_exception_handler_key;
  #ifdef MR_THREAD_LOCAL_STORAGE
    __thread MercuryEngine *MR_thread_engine_base;
  #else
    MercuryThreadKey MR_engine_base_key;
  #endif
  MercuryLock       MR_global_lock;
#endif

volatile MR_bool    MR_exit_now;
MR_bool             MR_debug_threads = MR_FALSE;

MR_Unsigned         MR_num_thread_local_mutables = 0;

MR_Integer          MR_thread_barrier_count;
#ifdef MR_THREAD_SAFE
  MercuryLock       MR_thread_barrier_lock;
  #ifdef MR_HIGHLEVEL_CODE
    MercuryCond     MR_thread_barrier_cond;
  #endif
#endif
#ifndef MR_HIGHLEVEL_CODE
  MR_Context        *MR_thread_barrier_context;
#endif

#ifdef MR_THREAD_SAFE

static void         *MR_create_thread_2(void *goal);

MercuryThread *
MR_create_thread(MR_ThreadGoal *goal)
{
    MercuryThread   *thread;
    pthread_attr_t  attrs;
    int             err;

    assert(MR_primordial_thread != (MercuryThread) 0);

    /*
    ** Create threads in the detached state so that resources will be
    ** automatically freed when threads terminate (we don't call
    ** pthread_join() anywhere).
    */
    thread = MR_GC_NEW(MercuryThread);
    pthread_attr_init(&attrs);
    pthread_attr_setdetachstate(&attrs, PTHREAD_CREATE_DETACHED);
    err = pthread_create(thread, &attrs, MR_create_thread_2, (void *) goal);
    pthread_attr_destroy(&attrs);

#if 0
    fprintf(stderr, "pthread_create returned %d (errno = %d)\n", err, errno);
#endif

    if (err != 0) {
        MR_fatal_error("error creating thread");
    }

    return thread;
}

static void *
MR_create_thread_2(void *goal0)
{
    MR_ThreadGoal   *goal;

    goal = (MR_ThreadGoal *) goal0;
    if (goal != NULL) {
        MR_init_thread(MR_use_now);
        (goal->func)(goal->arg);
        /* XXX: We should clean up the engine here */
    } else {
        MR_pin_thread();
        MR_init_thread(MR_use_later);
    }

    return NULL;
}

#endif /* MR_THREAD_SAFE */

MR_bool
MR_init_thread(MR_when_to_use when_to_use)
{
    MercuryEngine   *eng;

#ifdef MR_THREAD_SAFE
    /*
    ** Check to see whether there is already an engine that is initialized
    ** in this thread.  If so we just return, there's nothing for us to do.
    */
    if (MR_thread_engine_base != NULL) {
        return MR_FALSE;
    }
#endif
    eng = MR_create_engine();

#ifdef MR_THREAD_SAFE
    MR_set_thread_engine_base(eng);
    MR_restore_registers();
  #ifdef MR_ENGINE_BASE_REGISTER
    MR_engine_base_word = (MR_Word) eng;
  #endif
#else
    MR_memcpy(&MR_engine_base, eng, sizeof(MercuryEngine));
    MR_restore_registers();
#endif
    MR_load_engine_regs(MR_cur_engine());

#ifdef  MR_THREAD_SAFE
    MR_ENGINE(MR_eng_owner_thread) = pthread_self();
  #ifdef MR_THREADSCOPE
    /*
    ** TSC Synchronization is not used, support is commented out.  See
    ** runtime/mercury_threadscope.h for an explanation.
    **
    if (when_to_use == MR_use_later) {
        MR_threadscope_sync_tsc_slave();
    }
    */
  #endif
#endif

    switch (when_to_use) {
        case MR_use_later :
#ifdef MR_HIGHLEVEL_CODE
            MR_fatal_error("Sorry, not implemented: "
                "--high-level-code and multiple engines");
#else
            /* This call may never return */
            (void) MR_call_engine(MR_ENTRY(MR_do_runnext), MR_FALSE);
#endif
            MR_destroy_engine(eng);
            return MR_FALSE;

        case MR_use_now :
            /*
            ** The following is documented in mercury_engine.h, so any
            ** changes here may need changes there as well.
            */

            if (MR_ENGINE(MR_eng_this_context) == NULL) {
                MR_ENGINE(MR_eng_this_context) =
                    MR_create_context("init_thread",
                        MR_CONTEXT_SIZE_REGULAR, NULL);
#ifdef MR_THREADSCOPE
                MR_threadscope_post_create_context(MR_ENGINE(MR_eng_this_context));
#endif
            }
            MR_load_context(MR_ENGINE(MR_eng_this_context));
            MR_save_registers();
            return MR_TRUE;

        default:
            MR_fatal_error("init_thread was passed a bad value");
    }
}

/*
** Release resources associated with this thread.
*/

void
MR_finalize_thread_engine(void)
{
#ifdef MR_THREAD_SAFE
    MercuryEngine   *eng;

    eng = MR_thread_engine_base;
    MR_set_thread_engine_base(NULL);
    /*
    ** XXX calling destroy_engine(eng) here appears to segfault.
    ** This should probably be investigated and fixed.
    */
    MR_finalize_engine(eng);
#endif
}

#ifdef  MR_THREAD_SAFE

void
MR_destroy_thread(void *eng0)
{
    MercuryEngine *eng = eng0;
    MR_destroy_engine(eng);
}

#endif

#if defined(MR_THREAD_SAFE)
/*
** XXX: maybe these should only be conditionally compiled when MR_DEBUG_THREADS
** is also set. - pbone 
*/

int
MR_mutex_lock(MercuryLock *lock, const char *from)
{
    int err;

    fprintf(stderr, "%ld locking on %p (%s)\n",
        (long) pthread_self(), lock, from);
    err = pthread_mutex_lock(lock);
    assert(err == 0);
    return err;
}

int
MR_mutex_unlock(MercuryLock *lock, const char *from)
{
    int err;

    fprintf(stderr, "%ld unlocking on %p (%s)\n",
        (long) pthread_self(), lock, from);
    err = pthread_mutex_unlock(lock);
    assert(err == 0);
    return err;
}

int
MR_cond_signal(MercuryCond *cond, const char *from)
{
    int err;

    fprintf(stderr, "%ld signaling %p (%s)\n", 
        (long) pthread_self(), cond, from);
    err = pthread_cond_signal(cond);
    assert(err == 0);
    return err;
}

int
MR_cond_broadcast(MercuryCond *cond, const char *from)
{
    int err;

    fprintf(stderr, "%ld broadcasting %p (%s)\n", 
        (long) pthread_self(), cond, from);
    err = pthread_cond_broadcast(cond);
    assert(err == 0);
    return err;
}

int
MR_cond_wait(MercuryCond *cond, MercuryLock *lock, const char *from)
{
    int err;

    fprintf(stderr, "%ld waiting on cond: %p lock: %p (%s)\n", 
        (long) pthread_self(), cond, lock, from);
    err = pthread_cond_wait(cond, lock);
    assert(err == 0);
    return err;
}

int
MR_cond_timed_wait(MercuryCond *cond, MercuryLock *lock, 
    const struct timespec *abstime, const char *from)
{
    int err;
    
    fprintf(stderr, "%ld timed-waiting on cond: %p lock: %p (%s)\n",
        (long)pthread_self(), cond, lock, from);
    err = pthread_cond_timedwait(cond, lock, abstime);
    fprintf(stderr, "%ld timed-wait returned %d\n", err);
    return err;
}

#endif  /* MR_THREAD_SAFE */

MR_Unsigned
MR_new_thread_local_mutable_index(void)
{
    if (MR_num_thread_local_mutables >= MR_MAX_THREAD_LOCAL_MUTABLES-1) {
        MR_fatal_error("too many thread-local mutables");
    }
    return MR_num_thread_local_mutables++;
}

MR_ThreadLocalMuts *
MR_create_thread_local_mutables(MR_Unsigned numslots)
{
    MR_ThreadLocalMuts  *muts;

    muts = MR_GC_NEW(MR_ThreadLocalMuts);
#ifdef MR_THREAD_SAFE
    pthread_mutex_init(&muts->MR_tlm_lock, MR_MUTEX_ATTR);
#endif
    muts->MR_tlm_values = MR_GC_NEW_ARRAY(MR_Word, numslots);

    return muts;
}

MR_ThreadLocalMuts *
MR_clone_thread_local_mutables(const MR_ThreadLocalMuts *old_muts)
{
    MR_ThreadLocalMuts  *new_muts;
    MR_Unsigned         i;

    new_muts = MR_create_thread_local_mutables(MR_num_thread_local_mutables);

    MR_LOCK(&new_muts->MR_tlm_lock, "MR_clone_thread_local_mutables");
    for (i = 0; i < MR_num_thread_local_mutables; i++) {
        new_muts->MR_tlm_values[i] = old_muts->MR_tlm_values[i];
    }
    MR_UNLOCK(&new_muts->MR_tlm_lock, "MR_clone_thread_local_mutables");

    return new_muts;
}
