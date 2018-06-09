// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2009-2011 The University of Melbourne.
// Copyright (C) 2014-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This module contains the definitions of the primitive operations we use to
// implement builtins for parallel grades as C macros. Some of these macros can
// be invoked either from the predicates representing the operations in
// par_builtin.m in the library directory, or from a foreign_proc version of
// those predicates inserted directly into compiler-generated code.

#ifndef MERCURY_PAR_BUILTIN_H
#define MERCURY_PAR_BUILTIN_H

#include "mercury_context.h"
#include "mercury_thread.h"
#include "mercury_threadscope.h"
#include "mercury_atomic_ops.h"

////////////////////////////////////////////////////////////////////////////
//
// Futures for dependant AND parallelism.
//
////////////////////////////////////////////////////////////////////////////

#ifdef MR_THREAD_SAFE

    struct MR_Future_Struct {
        // Th lock preventing concurrent accesses.
        MercuryLock     MR_fut_lock;

        // A linked list of all the contexts blocked on this future.
        MR_Context      *MR_fut_suspended;

        // Has this future been signalled yet?
        volatile int    MR_fut_signalled;

        MR_Word         MR_fut_value;
    };

#else // !MR_THREAD_SAFE

    struct MR_Future_Struct {
        char dummy; // ANSI C doesn't allow empty structs
    };

#endif // !MR_THREAD_SAFE

// The mutex needs to be destroyed when the future is garbage collected.
// For efficiency we might want to ignore this altogether, e.g. on Linux
// pthread_mutex_destroy() only checks that the mutex is unlocked.
//
// We initialize the value field only to prevent its previous value,
// which may point to an allocated block, keeping that block alive.
// Semantically, the value field is undefined at this point in time.

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
    // In threadscope grades we need to pass the name of the future to the
    // threadscope event.

    #define MR_par_builtin_new_future(Future, Name)                         \
        do {                                                                \
            MR_par_builtin_new_future_2(Future);                            \
            MR_threadscope_post_new_future(Future, Name);                   \
        } while (0)

  #else // ! MR_THREADSCOPE

    #define MR_par_builtin_new_future(Future)                               \
        do {                                                                \
            MR_par_builtin_new_future_2(Future);                            \
        } while (0)

  #endif // ! MR_THREADSCOPE

    // If MR_fut_signalled is true, then we guarantee that reading MR_fut_value
    // is safe, even without a lock; see the corresponding code in
    // MR_par_builtin_signal_future().
    // If MR_fut_signalled is false, then we do take a lock and re-read
    // this value (to ensure there was not a race).

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
                                                                            \
                    MR_incr_sp(1);                                          \
                    MR_sv(1) = (MR_Word) Future;                            \
                                                                            \
                    /*                                                      \
                    ** Save this context and put it on the list of          \
                    ** suspended contexts for this future.                  \
                    */                                                      \
                                                                            \
                    ctxt = MR_ENGINE(MR_eng_this_context);                  \
                    MR_save_context(ctxt);                                  \
                                                                            \
                    ctxt->MR_ctxt_resume =                                  \
                        MR_ENTRY(mercury__par_builtin__wait_resume);        \
                    ctxt->MR_ctxt_resume_engine =                           \
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
                                                                            \
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
            ** threads (and posting those messages).                        \
            */                                                              \
                                                                            \
            MR_maybe_post_signal_future(Future);                            \
            MR_LOCK(&(Future->MR_fut_lock), "future.signal");               \
                                                                            \
            /*                                                              \
            ** If the same future is passed twice to a procedure then it    \
            ** could be signalled twice, but the value must be the same.    \
            */                                                              \
                                                                            \
            if (Future->MR_fut_signalled) {                                 \
                assert(Future->MR_fut_value == Value);                      \
            } else {                                                        \
                Future->MR_fut_value = Value;                               \
                /*                                                          \
                ** Ensure that the value is available before we update      \
                ** MR_fut_signalled.                                        \
                */                                                          \
                                                                            \
                MR_CPU_SFENCE;                                              \
                Future->MR_fut_signalled = MR_TRUE;                         \
            }                                                               \
                                                                            \
            /* Schedule all the contexts blocked on this future. */         \
            ctxt = Future->MR_fut_suspended;                                \
            while (ctxt != NULL) {                                          \
                next = ctxt->MR_ctxt_next;                                  \
                MR_schedule_context(ctxt);  /* Clobbers MR_ctxt_next. */    \
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

////////////////////////////////////////////////////////////////////////////
//
// Builtins for loop coordination.
//
////////////////////////////////////////////////////////////////////////////

#if defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ)

typedef struct MR_LoopControl_Struct        MR_LoopControl;
typedef struct MR_LoopControlSlot_Struct    MR_LoopControlSlot;

struct MR_LoopControlSlot_Struct
{
    MR_Context                              *MR_lcs_context;
    MR_bool                                 MR_lcs_is_free;
};

struct MR_LoopControl_Struct
{
    // Outstanding workers is manipulated with atomic instructions.
    MR_THREADSAFE_VOLATILE MR_Integer       MR_lc_outstanding_workers;

    // This lock protects only the next field
    MR_THREADSAFE_VOLATILE MR_Us_Lock       MR_lc_master_context_lock;
    MR_Context* MR_THREADSAFE_VOLATILE      MR_lc_master_context;

    // Unused atm
    MR_THREADSAFE_VOLATILE MR_bool          MR_lc_finished;

    // When a slot becomes free, its index is stored here so that when a free
    // slot is requested, the slot with this index is checked first.

    unsigned                                MR_lc_free_slot_hint;

    // MR_lc_slots MUST be the last field, since in practice, we treat
    // the array as having as many slots as we need, adding the size of
    // all the elements except the first to sizeof(MR_LoopControl) when
    // we allocate memory for the structure.

    unsigned                                MR_lc_num_slots;
    MR_LoopControlSlot                      MR_lc_slots[1];
};

#else

// We have to define these types so that par_builtin.m can use them
// as foreign types, even in grades that do not support them.

typedef MR_Word MR_LoopControl;
typedef MR_Word MR_LoopControlSlot;

#endif

#if defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ)

#ifdef MR_DEBUG_LOOP_CONTROL
    #define MR_IF_DEBUG_LOOP_CONTORL(stmt)                              \
        do {                                                            \
            stmt;                                                       \
        } while (0);
#else
    #define MR_IF_DEBUG_LOOP_CONTORL(stmt)                              \
        do {                                                            \
            ;                                                           \
        } while (0);
#endif

// XXX: Make these functions macros, they are now functions to make debugging
// and testing easier.

// Create and initialize a loop control structure.

extern MR_LoopControl   *MR_lc_create(unsigned num_workers);

// Wait for all workers, and then finalize and free the loop control structure.
// The caller must pass a module-unique (unquoted) string for resume_point_name
// that will be used in the name for a C label.

#define MR_lc_finish_part1(lc, part2_label)                                 \
    MR_IF_DEBUG_LOOP_CONTORL(                                               \
        fprintf(stderr, "lc_finish_part1(%p, %p), sp: %p\n",                \
            (lc), (part2_label), MR_sp));                                   \
                                                                            \
    do {                                                                    \
        (lc)->MR_lc_finished = MR_TRUE;                                     \
        /*                                                                  \
        ** This barrier ensures that MR_lc_finished has been set to MR_TRUE \
        ** before we read MR_lc_outstanding_contexts.                       \
        ** it works with another barrier in                                 \
        ** MR_lc_join_and_terminate(). See MR_lc_join_and_terminate().      \
        */                                                                  \
        MR_CPU_MFENCE;                                                      \
        MR_US_SPIN_LOCK(&((lc)->MR_lc_master_context_lock));                \
        if ((lc)->MR_lc_outstanding_workers > 0) {                          \
            /*                                                              \
            ** This context must wait until the workers are finished.       \
            ** This must be implemented as a macro, since we cannot move    \
            ** the C stack pointer without extra work.                      \
            */                                                              \
            if ((lc)->MR_lc_outstanding_workers != 0) {                     \
                MR_save_context(MR_ENGINE(MR_eng_this_context));            \
                MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume_engine =     \
                    MR_ENGINE(MR_eng_id);                                   \
                MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume =            \
                    (part2_label);                                          \
                (lc)->MR_lc_master_context = MR_ENGINE(MR_eng_this_context);\
                MR_US_UNLOCK(&((lc)->MR_lc_master_context_lock));           \
                MR_ENGINE(MR_eng_this_context) = NULL;                      \
                MR_idle(); /* Release the engine to the idle loop. */       \
            }                                                               \
        }                                                                   \
        MR_US_UNLOCK(&((lc)->MR_lc_master_context_lock));                   \
        /* Fall through to part2. */                                        \
    } while (0);

#define MR_lc_finish_part2(lc)                                              \
    MR_IF_DEBUG_LOOP_CONTORL(                                               \
        fprintf(stderr, "lc_finish_part2(%p), sp: %p\n", (lc), MR_sp));     \
                                                                            \
    do {                                                                    \
        unsigned i;                                                         \
                                                                            \
        /* All the jobs have finished. */                                   \
        for (i = 0; i < (lc)->MR_lc_num_slots; i++) {                       \
            if ((lc)->MR_lc_slots[i].MR_lcs_context != NULL) {              \
                /*                                                          \
                ** Note that we don't need to save the context here, it is  \
                ** saved in MR_join_and_terminate, which is important,      \
                ** because if we did save it here we would write invalid    \
                ** data into it.                                            \
                **                                                          \
                ** Reset the stack pointer, because loop control can some   \
                ** times leave a stack frame on the stack, and the rest     \
                ** of the runtime system will assume that the stack is      \
                ** clean.                                                   \
                */                                                          \
                (lc)->MR_lc_slots[i].MR_lcs_context->MR_ctxt_sp =           \
                    (lc)->MR_lc_slots[i].MR_lcs_context->                   \
                    MR_ctxt_detstack_zone->MR_zone_min;                     \
                MR_release_context((lc)->MR_lc_slots[i].MR_lcs_context);    \
            }                                                               \
        }                                                                   \
    } while (0);

// Get a free slot in the loop control if there is one.
//
// Deprecated: this was part of our old loop control design.

extern MR_Bool MR_lc_try_get_free_slot(MR_LoopControl *lc,
    MR_Unsigned *lcs_idx);

// Get a free slot in the loop control, or block until one is available.

#define MR_lc_wait_free_slot(lc, lcs_idx, retry_label)                      \
    MR_IF_DEBUG_LOOP_CONTORL(                                               \
        fprintf(stderr, "lc_wait_free_slot(%p, _, %p), sp: %p\n",           \
            (lc), (retry_label), MR_sp));                                   \
                                                                            \
    do {                                                                    \
        unsigned    hint, offset, i;                                        \
                                                                            \
        if ((lc)->MR_lc_outstanding_workers == (lc)->MR_lc_num_slots) {     \
            MR_US_SPIN_LOCK(&((lc)->MR_lc_master_context_lock));            \
            MR_CPU_MFENCE;                                                  \
            /*                                                              \
            ** Re-check outstanding workers while holding the lock.         \
            ** This ensures that we only commit to sleeping while holding   \
            ** the lock. But if there were a worker available, we would not \
            ** need to take the lock at all.                                \
            */                                                              \
            if ((lc)->MR_lc_outstanding_workers == (lc)->MR_lc_num_slots) { \
                MR_Context *ctxt;                                           \
                                                                            \
                /*                                                          \
                ** Block this context, and have it retry once it is         \
                ** unblocked.                                               \
                */                                                          \
                ctxt = MR_ENGINE(MR_eng_this_context);                      \
                MR_save_context(ctxt);                                      \
                ctxt->MR_ctxt_resume = retry_label;                         \
                ctxt->MR_ctxt_resume_engine = MR_ENGINE(MR_eng_id);         \
                (lc)->MR_lc_master_context = ctxt;                          \
                MR_CPU_SFENCE;                                              \
                MR_US_UNLOCK(&((lc)->MR_lc_master_context_lock));           \
                MR_ENGINE(MR_eng_this_context) = NULL;                      \
                MR_idle();                                                  \
            }                                                               \
            MR_US_UNLOCK(&((lc)->MR_lc_master_context_lock));               \
        }                                                                   \
                                                                            \
        hint = (lc)->MR_lc_free_slot_hint;                                  \
                                                                            \
        for (offset = 0; offset < (lc)->MR_lc_num_slots; offset++) {        \
            i = (hint + offset) % (lc)->MR_lc_num_slots;                    \
            if ((lc)->MR_lc_slots[i].MR_lcs_is_free) {                      \
                (lc)->MR_lc_slots[i].MR_lcs_is_free = MR_FALSE;             \
                (lc)->MR_lc_free_slot_hint =                                \
                    (i + 1) % (lc)->MR_lc_num_slots;                        \
                MR_atomic_inc_int(&((lc)->MR_lc_outstanding_workers));      \
                (lcs_idx) = i;                                              \
                break;                                                      \
            }                                                               \
        }                                                                   \
                                                                            \
        if ((lc)->MR_lc_slots[i].MR_lcs_context == NULL) {                  \
            /* Allocate a new context.*/                                    \
            (lc)->MR_lc_slots[i].MR_lcs_context =                           \
                MR_create_context("Loop control",                           \
                    MR_CONTEXT_SIZE_FOR_LOOP_CONTROL_WORKER, NULL);         \
            (lc)->MR_lc_slots[i].MR_lcs_context->                           \
                MR_ctxt_thread_local_mutables = MR_THREAD_LOCAL_MUTABLES;   \
        }                                                                   \
        /*                                                                  \
        ** Reset the stack pointer of the context, it may have been left    \
        ** in an inconsistent state.                                        \
        */                                                                  \
        (lc)->MR_lc_slots[i].MR_lcs_context->MR_ctxt_sp =                   \
            (lc)->MR_lc_slots[i].MR_lcs_context->                           \
            MR_ctxt_detstack_zone->MR_zone_min;                             \
                                                                            \
        MR_IF_DEBUG_LOOP_CONTORL(                                           \
            fprintf(stderr, "lc_wait_free_slot returning %d, sp: %p\n",     \
                (lcs_idx), MR_sp));                                         \
                                                                            \
    } while (0);

// Add a frame to the stack of the worker context in the loop control slot.

#define MR_lc_inc_worker_sp(lc, lcs_idx, N)                                 \
    do {                                                                    \
        MR_Context *ctxt;                                                   \
                                                                            \
        ctxt = ((MR_LoopControl*)lc)->MR_lc_slots[lcs_idx].MR_lcs_context;  \
        ctxt->MR_ctxt_sp += N;                                              \
    } while (0);

// Access a slot on the stack of the worker context in the loop control slot.

#define MR_lc_worker_sv(lc, lcs_idx, N)                                     \
  MR_based_stackvar(((MR_LoopControl*)lc)->MR_lc_slots[lcs_idx].            \
    MR_lcs_context->MR_ctxt_sp, (N))

// Try to spawn off this code using the free slot.

#define MR_lc_spawn_off(lc, lcs_idx, label)                             \
    MR_lc_spawn_off_func((lc), (lcs_idx), label)

extern void MR_lc_spawn_off_func(MR_LoopControl *lc, MR_Unsigned lcs_idx,
    MR_Code *code_ptr);

// Join and terminate a worker.

#define MR_lc_join_and_terminate(lc, lcs_idx)                                \
    do {                                                                     \
        MR_IF_DEBUG_LOOP_CONTORL(                                            \
            fprintf(stderr, "lc_join_and_terminate(%p, %d) parent_sp: %p\n", \
                (lc), (lcs_idx), MR_parent_sp));                             \
        /*                                                                   \
        ** Termination of this context must be handled in a macro so that    \
        ** C's stack pointer is set correctly. It might appear that we       \
        ** don't need to save the context since it doesn't hold a            \
        ** computation, but we do so that we save bookkeeping information.   \
        ** A similar mistake was the cause of a hard-to-diagnose bug in      \
        ** parallel stack segments grades.                                   \
        ** XXX Give a pointer to a description of that bug.                  \
        ** The context must be saved before we free the loop control slot,   \
        ** otherwise another engine may begin using it before we have        \
        ** saved it.                                                         \
        */                                                                   \
                                                                             \
        MR_save_context(MR_ENGINE(MR_eng_this_context));                     \
        MR_ENGINE(MR_eng_this_context) = NULL;                               \
                                                                             \
        MR_lc_join((lc), (lcs_idx));                                         \
                                                                             \
        MR_idle();                                                           \
    } while (0);

// Join a worker context with the main thread. Termination of the context
// is handled in the macro above.

extern void MR_lc_join(MR_LoopControl *lc, MR_Unsigned lcs_idx);

#endif // MR_THREAD_SAFE && MR_LL_PARALLEL_CONJ

#endif // not MERCURY_PAR_BUILTIN_H
