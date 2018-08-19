// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1997-2001, 2003, 2005-2007, 2009-2011 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#include "mercury_imp.h"
#include "mercury_regs.h"
#include "mercury_engine.h"
#include "mercury_memory.h"
#include "mercury_context.h"    // for MR_do_runnext
#include "mercury_runtime_util.h"
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

  #ifndef MR_HIGHLEVEL_CODE
    static MercuryLock  MR_all_engine_bases_lock;
    MercuryEngine       **MR_all_engine_bases = NULL;
    static MR_EngineId  MR_highest_engine_id;
  #endif
#endif

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
static MR_bool
MR_setup_engine_for_threads(MercuryEngine *eng, MR_EngineType engine_type);
static void
MR_shutdown_engine_for_threads(MercuryEngine *eng);
#endif

#ifdef MR_LL_PARALLEL_CONJ
static void *
MR_create_worksteal_thread_2(void *goal);

MercuryThread *
MR_create_worksteal_thread(void)
{
    MercuryThread   *thread;
    pthread_attr_t  attrs;
    int             err;
    char            errbuf[MR_STRERROR_BUF_SIZE];

    assert(!MR_thread_equal(MR_primordial_thread, MR_null_thread()));

    // Create threads in the detached state so that resources will be
    // automatically freed when threads terminate (we don't call
    // pthread_join() anywhere).

    thread = MR_GC_NEW_ATTRIB(MercuryThread, MR_ALLOC_SITE_RUNTIME);
    pthread_attr_init(&attrs);
    pthread_attr_setdetachstate(&attrs, PTHREAD_CREATE_DETACHED);
    err = pthread_create(thread, &attrs, MR_create_worksteal_thread_2, NULL);
    pthread_attr_destroy(&attrs);

#if 0
    fprintf(stderr, "pthread_create returned %d (errno = %d)\n", err, errno);
#endif

    if (err != 0) {
        MR_fatal_error("error creating thread: %s",
            MR_strerror(err, errbuf, sizeof(errbuf)));
    }

    return thread;
}

static void *
MR_create_worksteal_thread_2(void *arg)
{
  #ifdef MR_HAVE_THREAD_PINNING
     // TODO: We may use the cpu value returned to determine which CPUs
     // which engines are on. This can help with some interesting work
     // stealing algorithms.

    MR_pin_thread();
  #endif
    if (! MR_init_thread_inner(MR_use_later, MR_ENGINE_TYPE_SHARED)) {
        MR_fatal_error("Unable to init shared engine thread.");
    }
    return NULL;
}

#endif // MR_LL_PARALLEL_CONJ

// This interface is used by generated code and thread.m.
// Internal code should call MR_init_thread_inner.

MR_bool
MR_init_thread(MR_when_to_use when_to_use)
{
#ifdef MR_THREAD_SAFE
    // Check to see whether there is already an engine that is initialized
    // in this thread. If so we just return, there is nothing for us to do.

    if (MR_thread_engine_base != NULL) {
        return MR_FALSE;
    }
#endif // MR_THREAD_SAFE
    assert(when_to_use == MR_use_now);
    return MR_init_thread_inner(when_to_use, MR_ENGINE_TYPE_EXCLUSIVE);
}

// Set up a Mercury engine in the current thread.

MR_bool
MR_init_thread_inner(MR_when_to_use when_to_use, MR_EngineType engine_type)
{
    MercuryEngine   *eng;

    eng = MR_create_engine();

#ifdef MR_THREAD_SAFE
    if (MR_setup_engine_for_threads(eng, engine_type) == MR_FALSE) {
        MR_destroy_engine(eng);
        return MR_FALSE;
    }
    assert(MR_thread_engine_base == NULL);
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

#if defined(MR_LL_PARALLEL_CONJ) && defined(MR_THREADSCOPE)
    // TSC Synchronization is not used, support is commented out.
    // See runtime/mercury_threadscope.h for an explanation.
    //
    if (when_to_use == MR_use_later) {
        MR_threadscope_sync_tsc_slave();
    }

#endif

    switch (when_to_use) {
        case MR_use_later :
#ifdef MR_HIGHLEVEL_CODE
            MR_fatal_error("Sorry, not implemented: "
                "--high-level-code and multiple engines");
#else
            // This call never returns.
            (void) MR_call_engine(MR_ENTRY(MR_do_idle), MR_FALSE);
#endif
            return MR_FALSE;

        case MR_use_now :
            // The following is documented in mercury_engine.h, so any
            // changes here may need changes there as well.

            if (MR_ENGINE(MR_eng_this_context) == NULL) {
                MR_ENGINE(MR_eng_this_context) =
                    MR_create_context("init_thread",
                        MR_CONTEXT_SIZE_REGULAR, NULL);
            }
            MR_load_context(MR_ENGINE(MR_eng_this_context));
#ifdef MR_THREADSCOPE
            MR_threadscope_post_run_context();
#endif
            MR_save_registers();
            return MR_TRUE;

        default:
            MR_fatal_error("init_thread was passed a bad value");
    }
}

// Release resources associated with the Mercury engine for this thread.

void
MR_finalize_thread_engine(void)
{
#ifdef MR_THREAD_SAFE
    MercuryEngine   *eng;

    eng = MR_thread_engine_base;
    MR_set_thread_engine_base(NULL);
    MR_shutdown_engine_for_threads(eng);
    MR_destroy_engine(eng);
#endif
}

#ifdef MR_THREAD_SAFE
// Additional setup/shutdown of the engine for threads support.

static MR_bool
MR_setup_engine_for_threads(MercuryEngine *eng, MR_EngineType engine_type)
{
    MR_bool     ok = MR_TRUE;
  #ifndef MR_HIGHLEVEL_CODE
    MR_EngineId min;
    MR_EngineId max;
    MR_EngineId id;

    MR_LOCK(&MR_all_engine_bases_lock, "MR_setup_engine_for_threads");

    // Allocate an engine id.
    if (engine_type == MR_ENGINE_TYPE_SHARED) {
        min = 0;
        max = MR_num_ws_engines;
    } else {
        min = MR_num_ws_engines;
        max = MR_max_engines;
    }
    for (id = min; id < max; id++) {
        if (MR_all_engine_bases[id] == NULL) {
            break;
        }
    }

    if (id < max) {
        if (MR_highest_engine_id < id) {
            MR_highest_engine_id = id;
        }

        eng->MR_eng_id = id;
        eng->MR_eng_type = engine_type;
        eng->MR_eng_victim_counter = (id + 1) % MR_num_ws_engines;

        MR_all_engine_bases[id] = eng;
        MR_spark_deques[id] = eng->MR_eng_spark_deque;

        MR_verify_initial_engine_sleep_sync(id);

      #ifdef MR_THREADSCOPE
        MR_threadscope_setup_engine(eng);
      #endif
    } else {
      #ifdef MR_DEBUG_THREADS
        if (MR_debug_threads) {
            fprintf(stderr, "Exhausted engine ids.\n");
        }
      #endif
        ok = MR_FALSE;
    }

    MR_UNLOCK(&MR_all_engine_bases_lock, "MR_setup_engine_for_threads");
  #endif

    return ok;
}

static void
MR_shutdown_engine_for_threads(MercuryEngine *eng)
{
  #ifndef MR_HIGHLEVEL_CODE
    MR_EngineId id = eng->MR_eng_id;

    MR_LOCK(&MR_all_engine_bases_lock, "MR_shutdown_engine_for_threads");

    MR_verify_final_engine_sleep_sync(eng->MR_eng_id, eng->MR_eng_type);

    assert(MR_all_engine_bases[id] == eng);
    MR_all_engine_bases[id] = NULL;

    if (MR_highest_engine_id == id) {
        int i;
        for (i = id - 1; i >= 0; i--) {
            if (MR_all_engine_bases[i] != NULL) {
                MR_highest_engine_id = (MR_EngineId) i;
                break;
            }
        }
    }

    assert(MR_spark_deques[id] == eng->MR_eng_spark_deque);
    MR_spark_deques[id] = NULL;

    MR_UNLOCK(&MR_all_engine_bases_lock, "MR_shutdown_engine_for_threads");
  #endif
}
#endif // MR_THREAD_SAFE

#if defined(MR_THREAD_SAFE)
// XXX: maybe these should only be conditionally compiled when MR_DEBUG_THREADS
// is also set. - pbone

int
MR_mutex_lock(MercuryLock *lock, const char *from)
{
    int err;

    fprintf(stderr,
        "%" MR_INTEGER_LENGTH_MODIFIER "d locking on %p (%s)\n",
        MR_SELF_THREAD_ID, lock, from);
    fflush(stderr);
    err = pthread_mutex_lock(lock);
    fprintf(stderr,
        "%" MR_INTEGER_LENGTH_MODIFIER "d lock returned %d\n",
        MR_SELF_THREAD_ID, err);
    fflush(stderr);
    assert(err == 0);
    return err;
}

int
MR_mutex_unlock(MercuryLock *lock, const char *from)
{
    int err;

    fprintf(stderr,
        "%" MR_INTEGER_LENGTH_MODIFIER "d unlocking on %p (%s)\n",
        MR_SELF_THREAD_ID, lock, from);
    fflush(stderr);
    err = pthread_mutex_unlock(lock);
    fprintf(stderr,
        "%" MR_INTEGER_LENGTH_MODIFIER "d unlock returned %d\n",
        MR_SELF_THREAD_ID, err);
    fflush(stderr);
    assert(err == 0);
    return err;
}

int
MR_cond_signal(MercuryCond *cond, const char *from)
{
    int err;

    fprintf(stderr,
        "%" MR_INTEGER_LENGTH_MODIFIER "d signaling %p (%s)\n",
        MR_SELF_THREAD_ID, cond, from);
    fflush(stderr);
    err = pthread_cond_signal(cond);
    fprintf(stderr,
        "%" MR_INTEGER_LENGTH_MODIFIER "d signal returned %d\n",
        MR_SELF_THREAD_ID, err);
    fflush(stderr);
    assert(err == 0);
    return err;
}

int
MR_cond_broadcast(MercuryCond *cond, const char *from)
{
    int err;

    fprintf(stderr,
        "%" MR_INTEGER_LENGTH_MODIFIER "d broadcasting %p (%s)\n",
        MR_SELF_THREAD_ID, cond, from);
    fflush(stderr);
    err = pthread_cond_broadcast(cond);
    fprintf(stderr,
        "%" MR_INTEGER_LENGTH_MODIFIER "d broadcast returned %d\n",
        MR_SELF_THREAD_ID, err);
    fflush(stderr);
    assert(err == 0);
    return err;
}

int
MR_cond_wait(MercuryCond *cond, MercuryLock *lock, const char *from)
{
    int err;

    fprintf(stderr,
        "%" MR_INTEGER_LENGTH_MODIFIER "d waiting on cond: %p lock: %p (%s)\n",
        MR_SELF_THREAD_ID, cond, lock, from);
    fflush(stderr);
    err = pthread_cond_wait(cond, lock);
    fprintf(stderr,
        "%" MR_INTEGER_LENGTH_MODIFIER "d wait returned %d\n",
        MR_SELF_THREAD_ID, err);
    fflush(stderr);
    assert(err == 0);
    return err;
}

int
MR_cond_timed_wait(MercuryCond *cond, MercuryLock *lock,
    const struct timespec *abstime, const char *from)
{
    int err;

    fprintf(stderr,
        "%" MR_INTEGER_LENGTH_MODIFIER "d timed-waiting on cond: %p lock: %p (%s)\n",
        MR_SELF_THREAD_ID, cond, lock, from);
    fflush(stderr);
    err = pthread_cond_timedwait(cond, lock, abstime);
    fprintf(stderr,
        "%" MR_INTEGER_LENGTH_MODIFIER "d timed-wait returned %d\n",
        MR_SELF_THREAD_ID, err);
    fflush(stderr);
    return err;
}

void
MR_sem_init(MercurySem *sem, unsigned int value)
{
#if defined(MR_USE_LIBDISPATCH)
    *sem = dispatch_semaphore_create(value);
    if (*sem == NULL) {
        MR_perror("cannot initialize semaphore");
        exit(EXIT_FAILURE);
    }
#else // !MR_USE_LIBDISPATCH
    // XXX we should check errno and say *why* we could not initialize
    // the semaphore.
    if (sem_init(sem, 0, value) == -1) {
        MR_perror("cannot initialize semaphore");
        exit(EXIT_FAILURE);
    }
#endif
}

int
MR_sem_wait(MercurySem *sem, const char *from)
{
    int err;

    fprintf(stderr,
        "%" MR_INTEGER_LENGTH_MODIFIER "d waiting on sem: %p (%s)\n",
        MR_SELF_THREAD_ID, sem, from);
    fflush(stderr);
#if defined(MR_USE_LIBDISPATCH)
    err = dispatch_semaphore_wait(*sem, DISPATCH_TIME_FOREVER);
#else
    err = sem_wait(sem);
#endif
    fprintf(stderr,
        "%" MR_INTEGER_LENGTH_MODIFIER "d wait returned %d\n",
        MR_SELF_THREAD_ID, err);
    fflush(stderr);

    return err;
}

int
MR_sem_timed_wait(MercurySem *sem, const struct timespec *abstime,
        const char *from)
{
    int err;

    fprintf(stderr,
        "%" MR_INTEGER_LENGTH_MODIFIER "d timed wait on sem: %p (%s)\n",
            MR_SELF_THREAD_ID, sem, from);
    fflush(stderr);
#if defined(MR_USE_LIBDISPATCH)
    err = dispatch_semaphore_wait(*sem, dispatch_walltime(abstime, 0));
#else
    err = sem_timedwait(sem, abstime);
#endif
    fprintf(stderr,
        "%" MR_INTEGER_LENGTH_MODIFIER "d timed wait returned %d\n",
        MR_SELF_THREAD_ID, err);
    fflush(stderr);

    return err;
}

int
MR_sem_post(MercurySem *sem, const char *from)
{
    int err;

    fprintf(stderr,
        "%" MR_INTEGER_LENGTH_MODIFIER "d posting to sem: %p (%s)\n",
        MR_SELF_THREAD_ID, sem, from);
    fflush(stderr);
#if defined(MR_USE_LIBDISPATCH)
    err = dispatch_semaphore_signal(*sem);
#else
    err = sem_post(sem);
#endif
    fprintf(stderr,
        "%" MR_INTEGER_LENGTH_MODIFIER "d post returned %d\n",
        MR_SELF_THREAD_ID, err);
    fflush(stderr);

    return err;
}

void
MR_sem_destroy(MercurySem *sem)
{
#if defined(MR_USE_LIBDISPATCH)
   dispatch_release(*sem);
#else
   if (sem_destroy(sem) == -1) {
        MR_perror("cannot destroy semaphore");
        exit(EXIT_FAILURE);
    }
#endif
}

// For pthreads-win32 MR_null_thread() is defined as follows. For other
// pthread implementations it is defined as a macro in mercury_thread.h.

#if defined(MR_PTHREADS_WIN32)
MercuryThread
MR_null_thread(void)
{
    const MercuryThread null_thread = {NULL, 0};
    return null_thread;
}
#endif // MR_PTHREADS_WIN32

#endif  // MR_THREAD_SAFE

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

    muts = MR_GC_NEW_ATTRIB(MR_ThreadLocalMuts, MR_ALLOC_SITE_RUNTIME);
#ifdef MR_THREAD_SAFE
    pthread_mutex_init(&muts->MR_tlm_lock, MR_MUTEX_ATTR);
#endif
    muts->MR_tlm_values = MR_GC_NEW_ARRAY_ATTRIB(MR_Word, numslots,
        MR_ALLOC_SITE_RUNTIME);

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

#ifdef MR_THREAD_SAFE
void
MR_init_thread_stuff(void)
{
    int i;

    pthread_mutex_init(&MR_global_lock, MR_MUTEX_ATTR);
  #ifndef MR_THREAD_LOCAL_STORAGE
    MR_KEY_CREATE(&MR_engine_base_key, NULL);
  #endif
    MR_KEY_CREATE(&MR_exception_handler_key, NULL);
    pthread_mutex_init(&MR_thread_barrier_lock, MR_MUTEX_ATTR);
  #ifdef MR_HIGHLEVEL_CODE
    pthread_cond_init(&MR_thread_barrier_cond, MR_COND_ATTR);
  #endif

  #ifndef MR_HIGHLEVEL_CODE
    pthread_mutex_init(&MR_all_engine_bases_lock, MR_MUTEX_ATTR);
    MR_all_engine_bases =
        MR_GC_malloc(sizeof(MercuryEngine *) * MR_max_engines);
    for (i = 0; i < MR_max_engines; i++) {
        MR_all_engine_bases[i] = NULL;
    }
  #endif
}
#endif

