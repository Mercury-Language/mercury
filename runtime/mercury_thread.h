// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1997-1998, 2000, 2003, 2005-2007, 2009-2011 The University of Melbourne.
// Copyright (C) 2014-2016 The Mercury team.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

#ifndef MERCURY_THREAD_H
#define MERCURY_THREAD_H

#include "mercury_std.h"

#ifdef  MR_THREAD_SAFE

  #include <signal.h>   // for sigset_t on the SPARC
  #include <pthread.h>

  #if defined(MR_USE_LIBDISPATCH)
      #include <dispatch/dispatch.h>
  #else
      #include <semaphore.h> // POSIX semaphores
  #endif

  #define MR_MUTEX_ATTR       NULL
  #define MR_COND_ATTR        NULL
  #define MR_THREAD_ATTR      NULL

  typedef pthread_t         MercuryThread;
  typedef pthread_key_t     MercuryThreadKey;
  typedef pthread_mutex_t   MercuryLock;
  typedef pthread_cond_t    MercuryCond;

  #if defined(MR_USE_LIBDISPATCH)
    typedef dispatch_semaphore_t MercurySem;
  #else
    typedef sem_t             MercurySem;
  #endif

  extern int    MR_mutex_lock(MercuryLock *lock, const char *from);
  extern int    MR_mutex_unlock(MercuryLock *lock, const char *from);
  extern int    MR_cond_signal(MercuryCond *cond, const char *from);
  extern int    MR_cond_broadcast(MercuryCond *cond, const char *from);
  extern int    MR_cond_wait(MercuryCond *cond, MercuryLock *lock,
                    const char *from);
  extern int    MR_cond_timed_wait(MercuryCond *cond, MercuryLock *lock,
                    const struct timespec *abstime, const char *from);

  extern void   MR_sem_init(MercurySem *sem, unsigned int);
  extern int    MR_sem_wait(MercurySem *sem, const char *from);
  extern int    MR_sem_timed_wait(MercurySem *sem, const struct timespec *abstime,
                    const char *from);
  extern int    MR_sem_post(MercurySem *sem, const char *from);
  extern void   MR_sem_destroy(MercurySem *sem);

  #if defined(MR_PTHREADS_WIN32)
    extern MercuryThread MR_null_thread(void);
  #else
    #define MR_null_thread() ((MercuryThread) 0)
  #endif

  #define MR_thread_equal(a, b)       pthread_equal((a), (b))

  #if defined(MR_PTHREADS_WIN32)
    #define MR_SELF_THREAD_ID ((MR_Integer) pthread_self().p)
  #else
    #define MR_SELF_THREAD_ID ((MR_Integer) pthread_self())
  #endif

  extern MR_bool    MR_debug_threads;

  #ifndef MR_DEBUG_THREADS

    // The following macros should be used once the use of locking in the
    // generated code is considered stable, since the alternative versions do
    // the same thing, but with debugging support enabled.

    #define MR_LOCK(lck, from)      pthread_mutex_lock((lck))
    #define MR_UNLOCK(lck, from)    pthread_mutex_unlock((lck))

    #define MR_COND_SIGNAL(cnd, from)    pthread_cond_signal((cnd))
    #define MR_COND_BROADCAST(cnd, from) pthread_cond_broadcast((cnd))
    #define MR_COND_WAIT(cnd, mtx, from) pthread_cond_wait((cnd), (mtx))
    #define MR_COND_TIMED_WAIT(cond, mtx, abstime, from)                \
        pthread_cond_timedwait((cond), (mtx), (abstime))

    #if defined(MR_USE_LIBDISPATCH)
      #define MR_SEM_WAIT(sem, from)                                    \
        dispatch_semaphore_wait(*(sem), DISPATCH_TIME_FOREVER)
      #define MR_SEM_POST(sem, from)  dispatch_semaphore_signal(*(sem))
      #define MR_SEM_TIMED_WAIT(sem, abstime, from)                     \
        dispatch_semaphore_wait(*(sem), dispatch_walltime((abstime), 0))
    #else
      #define MR_SEM_WAIT(sem, from)  sem_wait((sem))
      #define MR_SEM_POST(sem, from)  sem_post((sem))
      #define MR_SEM_TIMED_WAIT(sem, abstime, from)                     \
        sem_timedwait((sem), (abstime))
    #endif // !MR_USE_LIBDISPATCH

  #else // MR_DEBUG_THREADS

    #define MR_LOCK(lck, from)                                          \
                ( MR_debug_threads ?                                    \
                    MR_mutex_lock((lck), (from))                        \
                :                                                       \
                    pthread_mutex_lock((lck))                           \
                )
    #define MR_UNLOCK(lck, from)                                        \
                ( MR_debug_threads ?                                    \
                    MR_mutex_unlock((lck), (from))                      \
                :                                                       \
                    pthread_mutex_unlock((lck))                         \
                )

    #define MR_COND_SIGNAL(cnd, from)                                   \
                ( MR_debug_threads ?                                    \
                    MR_cond_signal((cnd), (from))                       \
                :                                                       \
                    pthread_cond_signal((cnd))                          \
                )
    #define MR_COND_BROADCAST(cnd, from)                                \
                ( MR_debug_threads ?                                    \
                    MR_cond_broadcast((cnd), (from))                    \
                :                                                       \
                    pthread_cond_broadcast((cnd))                       \
                )
    #define MR_COND_WAIT(cnd, mtx, from)                                \
                ( MR_debug_threads ?                                    \
                    MR_cond_wait((cnd), (mtx), (from))                  \
                :                                                       \
                    pthread_cond_wait((cnd), (mtx))                     \
                )
    #define MR_COND_TIMED_WAIT(cond, mtx, abstime, from)                \
        ( MR_debug_threads ?                                            \
            MR_cond_timed_wait((cond), (mtx), (abstime), (from))        \
        :                                                               \
            pthread_cond_timedwait((cond), (mtx), (abstime))            \
        )

    #if defined(MR_USE_LIBDISPATCH)

      #define MR_SEM_WAIT(sem, from)                                      \
          ( MR_debug_threads ?                                            \
              MR_sem_wait((sem), (from))                                  \
          :                                                               \
              dispatch_semaphore_wait(*(sem), DISPATCH_TIME_FOREVER)      \
          )

      #define MR_SEM_TIMED_WAIT(sem, abstime, from)                       \
          ( MR_debug_threads ?                                            \
              MR_sem_timed_wait((sem), (abstime), (from))                 \
          :                                                               \
              dispatch_semaphore_wait(*(sem), dispatch_walltime((abstime), 0)) \
          )

      #define MR_SEM_POST(sem, from)                                      \
          ( MR_debug_threads ?                                            \
              MR_sem_post((sem), (from))                                  \
          :                                                               \
              dispatch_semaphore_signal(*(sem))                           \
          )

    #else // !MR_USE_LIBDISPATCH

      #define MR_SEM_WAIT(sem, from)                                      \
          ( MR_debug_threads ?                                            \
              MR_sem_wait((sem), (from))                                  \
          :                                                               \
              sem_wait((sem))                                             \
          )

      #define MR_SEM_TIMED_WAIT(sem, abstime, from)                       \
          ( MR_debug_threads ?                                            \
              MR_sem_timed_wait((sem), (abstime), (from))                 \
          :                                                               \
              sem_timedwait((sem), (abstime))                             \
          )

      #define MR_SEM_POST(sem, from)                                      \
          ( MR_debug_threads ?                                            \
              MR_sem_post((sem), (from))                                  \
          :                                                               \
              sem_post((sem))                                             \
          )

    #endif // !MR_USE_LIBDISPATCH

  #endif // MR_DEBUG_THREADS

  // MR_SEM_IS_EINTR is used to test if MR_SEM_WAIT or MR_SEM_TIMED_WAIT was
  // interrupted by a signal. We do not test errno when using libdispatch as
  // the manual page for dispatch_semaphore_wait does not mention errno nor
  // EINTR.
  #if defined(MR_USE_LIBDISPATCH)
      #define MR_SEM_IS_EINTR(errno)    MR_FALSE
  #else
      #define MR_SEM_IS_EINTR(errno)    (errno == EINTR)
  #endif

  // The following two macros are used to protect pragma foreign_proc
  // predicates which are not thread-safe.
  // See the comments below.

  #define MR_OBTAIN_GLOBAL_LOCK(where)  MR_LOCK(&MR_global_lock, (where))
  #define MR_RELEASE_GLOBAL_LOCK(where) MR_UNLOCK(&MR_global_lock, (where))

  #define MR_GETSPECIFIC(key) pthread_getspecific((key))
  #define MR_KEY_CREATE       pthread_key_create

  // create_worksteal_thread() creates a new POSIX thread, and creates and
  // initializes a work-stealing Mercury engine to run in that thread.

  extern MercuryThread      *MR_create_worksteal_thread(void);

  // The primordial thread. Currently used for debugging.

  extern MercuryThread      MR_primordial_thread;

  // MR_global_lock is a mutex for ensuring that only one non-threadsafe
  // piece of pragma c code executes at a time. If `not_threadsafe' is
  // given or `threadsafe' is not given in the attributes of a pragma
  // c code definition of a predicate, then the generated code will
  // obtain this lock before executing the C code fragment, and then
  // release it afterwards.
  // XXX we should emit a warning if may_call_mercury and not_threadsafe
  // (the defaults) are specified since if you obtain the lock then
  // call back into Mercury deadlock could result.

  extern MercuryLock        MR_global_lock;

  #ifndef MR_HIGHLEVEL_CODE
  // This points to an array containing MR_max_engines pointers to Mercury
  // engines. It is indexed by engine id. The first item in the array is the
  // primordial thread. A null entry represents an unallocated engine id.
  // During initialisation, the pointer may be null.
  // This is exported only for leave_signal_handler.
  // All other accesses require the MR_all_engine_bases_lock.

    extern struct MR_mercury_engine_struct **MR_all_engine_bases;
  #endif

  // MR_exception_handler_key stores a key which can be used to get
  // the current exception handler for the current thread.

  extern MercuryThreadKey   MR_exception_handler_key;

#else // not MR_THREAD_SAFE

  #define MR_LOCK(nothing, from)        do { } while (0)
  #define MR_UNLOCK(nothing, from)      do { } while (0)

  #define MR_COND_SIGNAL(nothing, from)    do { } while (0)
  #define MR_COND_BROADCAST(nothing, from) do { } while (0)
  #define MR_COND_WAIT(no, thing, from)    (0)

  #define MR_OBTAIN_GLOBAL_LOCK(where)  do { } while (0)
  #define MR_RELEASE_GLOBAL_LOCK(where) do { } while (0)

#endif

// These are used to prevent the process terminating as soon as the original
// Mercury thread terminates.

extern MR_Integer MR_thread_barrier_count;
#ifdef MR_THREAD_SAFE
  extern MercuryLock MR_thread_barrier_lock;
  #ifdef MR_HIGHLEVEL_CODE
    extern MercuryCond MR_thread_barrier_cond;
  #endif
#endif
#ifndef MR_HIGHLEVEL_CODE
  extern struct MR_Context_Struct *MR_thread_barrier_context;
#endif

// The following enum is used as the argument to init_thread/init_thread_inner.
// MR_use_now should be passed to indicate that
// it has been called in a context in which it should initialize
// the current thread's environment and return.
// MR_use_later should be passed to indicate that the thread should
// be initialized, then suspend waiting for work to appear in the
// runqueue. The engine is destroyed when the execution of work from
// the runqueue returns.

typedef enum {
    MR_use_now,
    MR_use_later
} MR_when_to_use;

// In low-level C parallel grades, there are two types of Mercury engines.
// "Shared" engines may execute code from any Mercury thread.
// "Exclusive" engines execute code only for a single Mercury thread.
// Shared engines may steal work from other shared engines, so are also
// called work-stealing engines; we do not have shared engines that
// refrain from work-stealing.
//
// In low-level C non-parallel grades, all Mercury threads execute on
// the same unique Mercury engine. That engine is equivalent to a shared engine.
//
// In high-level C parallel grades, all Mercury threads execute in their
// own POSIX thread. All engines are exclusive engines.
//
// In high-level C non-parallel grades, only a single Mercury thread exists,
// executing in a single Mercury engine. That engine is equivalent
// to an exclusive engine, or a shared engine with no other engines present.

typedef enum {
    MR_ENGINE_TYPE_SHARED = 1,
    MR_ENGINE_TYPE_EXCLUSIVE = 2
} MR_EngineType;

#ifdef MR_HIGHLEVEL_CODE
  #define MR_PRIMORIDAL_ENGINE_TYPE   MR_ENGINE_TYPE_EXCLUSIVE
#else
  #define MR_PRIMORIDAL_ENGINE_TYPE   MR_ENGINE_TYPE_SHARED
#endif

// Create and initialize a new Mercury engine running in the current
// POSIX thread.
//
// See the comments above for the meaning of the argument.
// If there is already a Mercury engine running in the current POSIX thread
// then init_thread is just a no-op.
//
// Returns MR_TRUE if a Mercury engine was created as a result of this call
// *and* it is the caller's responsibility to finalize it (it is intended that
// the caller can store the return value and call finalize_thread_engine
// if it is true).

extern MR_bool  MR_init_thread(MR_when_to_use);
extern MR_bool  MR_init_thread_inner(MR_when_to_use, MR_EngineType);

// Finalize the thread engine running in the current POSIX thread.
// This will release the resources used by this thread -- this is very
// important because the memory used for the det stack for each thread
// can be re-used by the next init_thread.

extern void     MR_finalize_thread_engine(void);

// The values of thread-local mutables are stored in an array per Mercury
// thread. This makes it easy for a newly spawned thread to inherit (copy)
// all the thread-local mutables of its parent thread.
// Accesses to the array are protected by a mutex, in case a parallel
// conjunctions tries to read a thread-local value while another parallel
// conjunction (in the same Mercury thread) is writing to it.
//
// Each thread-local mutable has an associated index into the array, which is
// allocated to it during initialisation. For ease of implementation there is
// an arbitrary limit to the number of thread-local mutables that are allowed.

typedef struct MR_ThreadLocalMuts MR_ThreadLocalMuts;

struct MR_ThreadLocalMuts {
  #ifdef MR_THREAD_SAFE
    MercuryLock     MR_tlm_lock;
  #endif
    MR_Word         *MR_tlm_values;
};

#define MR_MAX_THREAD_LOCAL_MUTABLES    128
extern MR_Unsigned  MR_num_thread_local_mutables;

// Allocate an index into the thread-local mutable array for a mutable.

extern MR_Unsigned  MR_new_thread_local_mutable_index(void);

// Allocate a thread-local mutable array.

extern MR_ThreadLocalMuts   *MR_create_thread_local_mutables(
                                MR_Unsigned numslots);

// Make a copy of a thread-local mutable array.

extern MR_ThreadLocalMuts   *MR_clone_thread_local_mutables(
                                const MR_ThreadLocalMuts *old_muts);

#define MR_THREAD_LOCAL_MUTABLES                                        \
    (MR_ENGINE(MR_eng_this_context)->MR_ctxt_thread_local_mutables)

#define MR_SET_THREAD_LOCAL_MUTABLES(tlm)                               \
    do {                                                                \
        MR_THREAD_LOCAL_MUTABLES = (tlm);                               \
    } while (0)

#define MR_get_thread_local_mutable(type, var, mut_index)               \
    do {                                                                \
        MR_ThreadLocalMuts  *tlm;                                       \
                                                                        \
        tlm = MR_THREAD_LOCAL_MUTABLES;                                 \
        MR_LOCK(&tlm->MR_tlm_lock, "MR_get_thread_local_mutable");      \
        var = * ((type *) &tlm->MR_tlm_values[(mut_index)]);            \
        MR_UNLOCK(&tlm->MR_tlm_lock, "MR_get_thread_local_mutable");    \
    } while (0)

#define MR_set_thread_local_mutable(type, var, mut_index)               \
    do {                                                                \
        MR_ThreadLocalMuts  *tlm;                                       \
                                                                        \
        tlm = MR_THREAD_LOCAL_MUTABLES;                                 \
        MR_LOCK(&tlm->MR_tlm_lock, "MR_set_thread_local_mutable");      \
        * ((type *) &tlm->MR_tlm_values[(mut_index)]) = (var);          \
        MR_UNLOCK(&tlm->MR_tlm_lock, "MR_set_thread_local_mutable");    \
    } while (0)

// Initialise some static structures in mercury_thread.c.

extern void     MR_init_thread_stuff(void);

#endif  // MERCURY_THREAD_H
