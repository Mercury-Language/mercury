/*
vim: ft=c ts=4 sw=4 et
*/
/*
** Copyright (C) 2009-2011 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module contains the definitions of the primitive operations we use to
** implement builtins for parallel grades as C macros. Some of these macros can
** be invoked either from the predicates representing the operations in
** par_builtin.m in the library directory, or from a foreign_proc version of
** those predicates inserted directly into compiler-generated code.
*/

#ifndef MERCURY_PAR_BUILTIN_H
#define MERCURY_PAR_BUILTIN_H

#include "mercury_context.h"
#include "mercury_thread.h"
#include "mercury_threadscope.h"
#include "mercury_atomic_ops.h"

/***************************************************************************
**
** Futures for dependant AND parallelism.
**
***************************************************************************/

#ifdef MR_THREAD_SAFE

    struct MR_Future_Struct {
        /* lock preventing concurrent accesses */
        MercuryLock     MR_fut_lock;

        /* linked list of all the contexts blocked on this future */
        MR_Context      *MR_fut_suspended;

        /* whether this future has been signalled yet */
        volatile int    MR_fut_signalled;

        MR_Word         MR_fut_value;
    };

#else /* !MR_THREAD_SAFE */

    struct MR_Future_Struct {
        char dummy; /* ANSI C doesn't allow empty structs */
    };

#endif /* !MR_THREAD_SAFE */


/*
** The mutex needs to be destroyed when the future is garbage collected.
** For efficiency we might want to ignore this altogether, e.g. on Linux
** pthread_mutex_destroy() only checks that the mutex is unlocked.
**
** We initialize the value field only to prevent its previous value,
** which may point to an allocated block, keeping that block alive.
** Semantically, the value field is undefined at this point in time.
*/

#ifdef MR_CONSERVATIVE_GC
    extern  void    MR_finalize_future(void *obj, void *cd);

    #define MR_register_future_finalizer(Future)                            \
        do {                                                                \
            GC_REGISTER_FINALIZER(Future, MR_finalize_future,               \
                NULL, NULL, NULL);                                          \
            Future->MR_fut_value = 0;                                       \
        } while (0)
#else
    #define MR_register_future_finalizer(Future)                            \
        ((void) 0)
#endif

#ifdef MR_LL_PARALLEL_CONJ

    #define MR_par_builtin_new_future_2(Future)                             \
        do {                                                                \
            MR_Word fut_addr;                                               \
                                                                            \
            MR_incr_hp(fut_addr,                                            \
                MR_round_up(sizeof(MR_Future), sizeof(MR_Word)));           \
            Future = (MR_Future *) fut_addr;                                \
                                                                            \
            pthread_mutex_init(&(Future->MR_fut_lock), MR_MUTEX_ATTR);      \
            MR_register_future_finalizer(Future);                           \
                                                                            \
            Future->MR_fut_signalled = MR_FALSE;                            \
            Future->MR_fut_suspended = NULL;                                \
        } while (0)

  #ifdef MR_THREADSCOPE
    /*
    ** In threadscope grades we need to pass the name of the future to the
    ** threadscope event.
    */
    #define MR_par_builtin_new_future(Future, Name)                         \
        do {                                                                \
            MR_par_builtin_new_future_2(Future);                            \
            MR_threadscope_post_new_future(Future, Name);                   \
        } while (0)

  #else /* ! MR_THREADSCOPE */

    #define MR_par_builtin_new_future(Future)                               \
        do {                                                                \
            MR_par_builtin_new_future_2(Future);                            \
        } while (0)

  #endif /* ! MR_THREADSCOPE */

    /*
    ** If MR_fut_signalled is true then we guarantee that reading MR_fut_value
    ** is safe, even without a lock, see the corresponding code in
    ** MR_par_builtin_signal_future();
    ** If MR_fut_signalled is false then we do take a lock and re-read the
    ** this value (to ensure there was not a race).
    */

    MR_declare_entry(mercury__par_builtin__wait_resume);

    #define MR_par_builtin_wait_future(Future, Value)                       \
        do {                                                                \
            if (Future->MR_fut_signalled) {                                 \
                Value = Future->MR_fut_value;                               \
                MR_maybe_post_wait_future_nosuspend(Future);                \
            } else {                                                        \
                MR_LOCK(&(Future->MR_fut_lock), "future.wait");             \
                MR_CPU_LFENCE;                                              \
                if (Future->MR_fut_signalled) {                             \
                    MR_UNLOCK(&(Future->MR_fut_lock), "future.wait");       \
                    Value = Future->MR_fut_value;                           \
                    MR_maybe_post_wait_future_nosuspend(Future);            \
                } else {                                                    \
                    MR_Context *ctxt;                                       \
                                                                            \
                    /*                                                      \
                    ** Put the address of the future at a fixed place       \
                    ** known to mercury__par_builtin__wait_resume, to wit,  \
                    ** the top of the stack.                                \
                    */                                                      \
                    MR_incr_sp(1);                                          \
                    MR_sv(1) = (MR_Word) Future;                            \
                                                                            \
                    /*                                                      \
                    ** Save this context and put it on the list of          \
                    ** suspended contexts for this future.                  \
                    */                                                      \
                    ctxt = MR_ENGINE(MR_eng_this_context);                  \
                    MR_save_context(ctxt);                                  \
                                                                            \
                    ctxt->MR_ctxt_resume =                                  \
                        MR_ENTRY(mercury__par_builtin__wait_resume);        \
                    ctxt->MR_ctxt_resume_owner_engine =                     \
                        MR_ENGINE(MR_eng_id);                               \
                    ctxt->MR_ctxt_next = Future->MR_fut_suspended;          \
                    Future->MR_fut_suspended = ctxt;                        \
                                                                            \
                    MR_UNLOCK(&(Future->MR_fut_lock), "future.wait");       \
                    MR_maybe_post_wait_future_suspended(Future);            \
                                                                            \
                    MR_maybe_post_stop_context;                             \
                    MR_ENGINE(MR_eng_this_context) = NULL;                  \
                    /*                                                      \
                    ** MR_idle will try to run a different context as that  \
                    ** has good chance of unblocking the future.            \
                    */                                                      \
                    MR_idle();                                              \
                }                                                           \
            }                                                               \
        } while (0)

    #define MR_par_builtin_get_future(Future, Value)                        \
        do {                                                                \
            assert(Future->MR_fut_signalled);                               \
            Value = Future->MR_fut_value;                                   \
        } while (0)

    #define MR_par_builtin_signal_future(Future, Value)                     \
        do {                                                                \
            MR_Context *ctxt;                                               \
            MR_Context *next;                                               \
                                                                            \
            /*                                                              \
            ** Post the threadscope signal future message before waking any \
            ** threads (and posting those messages.                         \
            */                                                              \
            MR_maybe_post_signal_future(Future);                            \
            MR_LOCK(&(Future->MR_fut_lock), "future.signal");               \
                                                                            \
            /*                                                              \
            ** If the same future is passed twice to a procedure then it    \
            ** could be signalled twice, but the value must be the same.    \
            */                                                              \
            if (Future->MR_fut_signalled) {                                 \
                assert(Future->MR_fut_value == Value);                      \
            } else {                                                        \
                Future->MR_fut_value = Value;                               \
                /*                                                          \
                ** Ensure that the value is available before we update      \
                ** MR_fut_signalled.                                        \
                */                                                          \
                MR_CPU_SFENCE;                                              \
                Future->MR_fut_signalled = MR_TRUE;                         \
            }                                                               \
                                                                            \
            /* Schedule all the contexts blocked on this future. */         \
            ctxt = Future->MR_fut_suspended;                                \
            while (ctxt != NULL) {                                          \
                next = ctxt->MR_ctxt_next;                                  \
                MR_schedule_context(ctxt);  /* clobbers MR_ctxt_next */     \
                ctxt = next;                                                \
            }                                                               \
            Future->MR_fut_suspended = NULL;                                \
                                                                            \
            MR_UNLOCK(&(Future->MR_fut_lock), "future.signal");             \
        } while (0)

#ifdef MR_THREADSCOPE
    #define MR_maybe_post_stop_context                                      \
        do {                                                                \
            MR_threadscope_post_stop_context(MR_TS_STOP_REASON_BLOCKED);    \
        } while (0)

    #define MR_maybe_post_wait_future_nosuspend(future)                     \
        do {                                                                \
            MR_threadscope_post_wait_future_nosuspend(future);              \
        } while (0)

    #define MR_maybe_post_wait_future_suspended(future)                     \
        do {                                                                \
            MR_threadscope_post_wait_future_suspended(future);              \
        } while (0)

    #define MR_maybe_post_signal_future(future)                             \
        do {                                                                \
            MR_threadscope_post_signal_future(future);                      \
        } while (0)

#else
    #define MR_noop                                                         \
        do {                                                                \
        } while (0)
    #define MR_maybe_post_stop_context MR_noop
    #define MR_maybe_post_wait_future_nosuspend(future) MR_noop
    #define MR_maybe_post_wait_future_suspended(future) MR_noop
    #define MR_maybe_post_signal_future(future) MR_noop
#endif

#else

    #define MR_par_builtin_new_future(Future)                               \
        do {                                                                \
            MR_fatal_error("internal error: "                               \
                "new_future should only be used "                           \
                "by lowlevel parallel grades");                             \
        } while (0)

    #define MR_par_builtin_wait_future(Future, Value)                       \
        do {                                                                \
            MR_fatal_error("internal error: "                               \
                "wait_future should only be used "                          \
                "by lowlevel parallel grades");                             \
        } while (0)

    #define MR_par_builtin_get_future(Future, Value)                        \
        do {                                                                \
            MR_fatal_error("internal error: "                               \
                "get_future should only be used "                           \
                "by lowlevel parallel grades");                             \
        } while (0)

    #define MR_par_builtin_signal_future(Future, Value)                     \
        do {                                                                \
            MR_fatal_error("internal error: "                               \
                "signal_future should only be used "                        \
                "by lowlevel parallel grades");                             \
        } while (0)

#endif

/***************************************************************************
**
** Builtins for loop coordination.
**
***************************************************************************/

#if defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ)

typedef struct MR_LoopControl_Struct        MR_LoopControl;

typedef struct MR_LoopControlSlot_Struct    MR_LoopControlSlot;

struct MR_LoopControl_Struct
{
    MR_LoopControlSlot*                 MR_lc_slots;
    unsigned                            MR_lc_num_slots;
    MR_THREADSAFE_VOLATILE MR_Integer   MR_lc_outstanding_workers;
    MR_Context*                         MR_lc_waiting_context;
    MR_THREADSAFE_VOLATILE MR_bool      MR_lc_finished;
    MercuryLock                         MR_lc_lock;
};

struct MR_LoopControlSlot_Struct
{
    MR_Context*         MR_lcs_context;
    MR_bool             MR_lcs_is_free;
};

#else

/*
** We have to define these types so that par_builtin.m can use them as foreign
** types, even in grades that don't support them.
*/

typedef MR_Word MR_LoopControl;
typedef MR_Word MR_LoopControlSlot;

#endif

#if defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ)

/*
** XXX: Make these functions macros, they're functions now to make debugging
** and testing easier.
*/

/*
** Create and initialize a loop control structure.
*/
extern MR_LoopControl* MR_lc_create(unsigned num_workers);

/*
** Wait for all workers and then finalize and free the loop control structure,
** The caller must pass a module-unique (unquoted) string for resume_point_name
** that will be used in the name for a C label.
*/
#define MR_lc_finish_part1(lc, label)                                       \
    do {                                                                    \
        (lc)->MR_lc_finished = MR_TRUE;                                     \
        /*                                                                  \
        ** This barrier ensures that MR_lc_finished before we read          \
        ** MR_lc_outstanding_contexts, it works with another barrier in     \
        ** MR_lc_join_and_terminate().  See MR_lc_join_and_terminate().     \
        */                                                                  \
        MR_CPU_MFENCE;                                                      \
        if ((lc)->MR_lc_outstanding_workers > 0) {                          \
            /*                                                              \
            ** This context must wait until the workers are finished.       \
            ** This must be implemented as a macro, we cannot move the C    \
            ** stack pointer without extra work.                            \
            */                                                              \
            MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume =                \
                MR_LABEL(MR_add_prefix(label));                             \
            MR_LOCK(&((lc)->MR_lc_lock), "MR_lc_finish_part1");             \
            if ((lc)->MR_lc_outstanding_workers == 0) {                     \
                MR_UNLOCK(&((lc)->MR_lc_lock), "MR_lc_finish_part1");       \
                MR_GOTO_LOCAL(MR_add_prefix(label));                        \
            }                                                               \
            MR_save_context(MR_ENGINE(MR_eng_this_context));                \
            (lc)->MR_lc_waiting_context = MR_ENGINE(MR_eng_this_context);   \
            MR_UNLOCK(&((lc)->MR_lc_lock), "MR_lc_finish_part1");           \
            MR_ENGINE(MR_eng_this_context) = NULL;                          \
            MR_idle(); /* Release the engine to the idle loop */            \
        }                                                                   \
    } while (0);

#define MR_lc_finish_part2(lc)                                              \
    do {                                                                    \
        unsigned i;                                                         \
                                                                            \
        /*                                                                  \
        ** All the jobs have finished,                                      \
        */                                                                  \
        for (i = 0; i < (lc)->MR_lc_num_slots; i++) {                       \
            if ((lc)->MR_lc_slots[i].MR_lcs_context != NULL) {              \
                MR_destroy_context((lc)->MR_lc_slots[i].MR_lcs_context);    \
            }                                                               \
        }                                                                   \
        pthread_mutex_destroy(&((lc)->MR_lc_lock));                         \
    } while (0);

/*
** Get a free slot in the loop control if there is one.
*/
extern MR_LoopControlSlot* MR_lc_try_get_free_slot(MR_LoopControl* lc);

/*
** Try to spawn off this code using the free slot.
*/
#define MR_lc_spawn_off(lcs, label) \
    MR_lc_spawn_off_((lcs), MR_LABEL(MR_add_prefix(label)))

extern void MR_lc_spawn_off_(MR_LoopControlSlot* lcs, MR_Code* code_ptr);

/*
** Join and termiante a worker.
*/
#define MR_lc_join_and_terminate(lc, lcs)                                   \
    do {                                                                    \
        MR_lc_join((lc), (lcs));                                            \
                                                                            \
        /*                                                                  \
        ** Termination of this context must be handled in a macro so that   \
        ** C's stack pointer is set correctly.  It might appear that we     \
        ** don't need to save the context since it doesn't hold a           \
        ** computation, but we do so that we save bookkeeping information.  \
        ** A similar mistake was the cause of a hard-to-diagnose bug in     \
        ** parallel stack segments grades.                                  \
        */                                                                  \
        MR_save_context(MR_ENGINE(MR_eng_this_context));                    \
        MR_ENGINE(MR_eng_this_context) = NULL;                              \
        MR_idle();                                                          \
    } while (0);

/*
** Join a worker context with the main thread.  Termination of the context
** is handled in the macro above.
*/
extern void MR_lc_join(MR_LoopControl* lc, MR_LoopControlSlot* lcs);

#endif /* MR_THREAD_SAFE && MR_LL_PARALLEL_CONJ */

#endif /* not MERCURY_PAR_BUILTIN_H */
