// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1995-2007, 2009-2011 The University of Melbourne.
// Copyright (C) 2014, 2016 The Mercury team.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

// mercury_context.c - handles multithreading stuff.

/*
INIT mercury_sys_init_scheduler_wrapper
ENDINIT
*/

#ifndef _GNU_SOURCE
  // This must be defined prior to including <sched.h> for sched_setaffinity,
  // etc.

  #define _GNU_SOURCE
#endif

#include "mercury_imp.h"

#include <stdio.h>
#ifdef MR_THREAD_SAFE
  #include "mercury_thread.h"
  #include "mercury_stm.h"
  #ifndef MR_HIGHLEVEL_CODE
    #include <semaphore.h>
  #endif
#endif
#ifdef MR_CAN_DO_PENDING_IO
  #include <sys/types.h>    // for fd_set
  #include <sys/time.h>     // for struct timeval
  #ifdef MR_HAVE_UNISTD_H
    #include <unistd.h>     // for select() on OS X
  #endif
#endif
#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
  #include <math.h>         // for sqrt and pow
#endif

#ifdef MR_HAVE_SCHED_H
  #include <sched.h>
#endif

#ifdef MR_MINGW
  #include <sys/time.h>     // for gettimeofday()
#endif

#ifdef MR_WIN32
  #include <sys/timeb.h>    // for _ftime()
#endif

#ifdef MR_WIN32_GETSYSTEMINFO
  #include "mercury_windows.h"
#endif

#if defined(MR_THREAD_SAFE) && defined(MR_HAVE_HWLOC)
  #include <hwloc.h>
#endif

#include "mercury_memory_handlers.h"
#include "mercury_context.h"
#include "mercury_engine.h"             // for `MR_memdebug'
#include "mercury_threadscope.h"        // for data types and posting events
#include "mercury_reg_workarounds.h"    // for `MR_fd*' stuff

#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
#define MR_PROFILE_PARALLEL_EXECUTION_FILENAME "parallel_execution_profile.txt"
#endif

////////////////////////////////////////////////////////////////////////////

static void     MR_init_context_maybe_generator(MR_Context *c, const char *id,
                    MR_GeneratorPtr gen);

////////////////////////////////////////////////////////////////////////////

#if defined(MR_LL_PARALLEL_CONJ)
static void     MR_milliseconds_from_now(struct timespec *timeout,
                    unsigned int msecs);

// Engine states and notifications
// -------------------------------
//
// An engine may be in one of the following states, see the es_state field
// engine_sleep_sync_i
//
// working      The engine has work to do and is working on it.
//              The engine will not check for notifications, all
//              notifications will be ignored.
//
// idle         The engine finished its work and is looking for
//              more work. It is looking for a context to resume or a local
//              spark. If found, the engine will move to the working state,
//              if not, it will check for notifications and if there are
//              none it moves to the stealing state. Only notify an idle
//              engine with notifications that may be ignored.
//
// stealing     The engine is now attempting to work steal. It has now
//              incremented the idle engine count to make it easier to
//              receive notifications. If it finds a spark it will decrement
//              the count and execute the spark. Otherwise it checks for
//              notifications and moves to the sleeping state. This state
//              is similar to idle but separate as it allows another engine
//              to understand if this engine has modified the idle engine
//              count (which we don't want to do in the idle state as that
//              will often find a local spark to execute).
//
// sleeping     The engine has committed to going to sleep, to wake it up
//              one must post to its sleep semaphore ensuring that it does
//              not sleep. Any notification can be sent at this stage as
//              all will be acted upon, including the context notification
//              which cannot be dropped.
//
// notified
//              The engine has received a notification, it cannot receive
//              another notification now. This state is initiated by the
//              notifier, and therefore is done with either a compare and
//              swap or a lock depending on the state of the engine. See
//              try_wake_engine and try_notify_engine. Upon receiving the
//              notification the engine will set its new status
//              appropriately.
//
// More information about these states including which transitions are legal
// can be found in notes/par_engine_state.{txt,dot}

// Busy isn't a normal state, but it's used with the CAS code to make some
// operations atomic.

#define ENGINE_STATE_BUSY           0x0000
#define ENGINE_STATE_WORKING        0x0001
#define ENGINE_STATE_IDLE           0x0002
#define ENGINE_STATE_STEALING       0x0004
#define ENGINE_STATE_SLEEPING       0x0008
#define ENGINE_STATE_NOTIFIED       0x0010
#define ENGINE_STATE_ALL            0xFFFF

struct engine_sleep_sync_i {
    MercurySem                                  es_sleep_semaphore;
    MercuryLock                                 es_wake_lock;
    volatile MR_Unsigned                        es_state;
    volatile unsigned                           es_action;
    volatile union MR_engine_wake_action_data   es_action_data;
};

typedef struct {
    struct engine_sleep_sync_i d;
    // Padding ensures that engine sleep synchronisation data for different
    // engines doesn't share cache lines.

    char padding[PAD_CACHE_LINE(sizeof(struct engine_sleep_sync_i))];
} engine_sleep_sync;

static
engine_sleep_sync *engine_sleep_sync_data;

static engine_sleep_sync *
get_engine_sleep_sync(MR_EngineId i)
{
    MR_assert(i < MR_max_engines);
    return &engine_sleep_sync_data[i];
}
#endif // MR_LL_PARALLEL_CONJ

// The run queue is protected with MR_runqueue_lock.

MR_Context              *MR_runqueue_head;
MR_Context              *MR_runqueue_tail;
#ifdef  MR_THREAD_SAFE
  MercuryLock           MR_runqueue_lock;
#endif

MR_PendingContext       *MR_pending_contexts;
#ifdef  MR_THREAD_SAFE
  MercuryLock           MR_pending_contexts_lock;
#endif

#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
MR_bool                 MR_profile_parallel_execution = MR_FALSE;

  #ifndef MR_HIGHLEVEL_CODE
static MR_Stats         MR_profile_parallel_executed_global_sparks =
                            { 0, 0, 0, 0 };
static MR_Stats         MR_profile_parallel_executed_contexts = { 0, 0, 0, 0 };
static MR_Stats         MR_profile_parallel_executed_nothing = { 0, 0, 0, 0 };
// This cannot be static as it is used in macros by other modules.
MR_Stats                MR_profile_parallel_executed_local_sparks =
                            { 0, 0, 0, 0 };
static MR_Integer       MR_profile_parallel_contexts_created_for_sparks = 0;

// We don't access these atomically. They are protected by the free context
// list lock.

static MR_Integer       MR_profile_parallel_small_context_reused = 0;
static MR_Integer       MR_profile_parallel_regular_context_reused = 0;
static MR_Integer       MR_profile_parallel_small_context_kept = 0;
static MR_Integer       MR_profile_parallel_regular_context_kept = 0;
  #endif // ! MR_HIGHLEVEL_CODE
#endif // MR_PROFILE_PARALLEL_EXECUTION_SUPPORT

#ifdef MR_THREAD_SAFE
// Detect number of processors.

unsigned         MR_num_processors;
#if defined(MR_HAVE_HWLOC)
    static hwloc_topology_t MR_hw_topology;
    static hwloc_cpuset_t   MR_hw_available_pus = NULL;
#elif defined(MR_HAVE_SCHED_SETAFFINITY)
    static cpu_set_t        *MR_available_cpus;
    // The number of CPUs that MR_available_cpus can refer to.
    static unsigned         MR_cpuset_size = 0;
#endif

// Local variables for thread pinning.

#if defined(MR_LL_PARALLEL_CONJ) && defined(MR_HAVE_THREAD_PINNING)
MR_bool                 MR_thread_pinning = MR_FALSE;
static MercuryLock      MR_thread_pinning_lock;
static unsigned         MR_num_threads_left_to_pin;
MR_Unsigned             MR_primordial_thread_cpu;
#endif
#endif

#if defined(MR_LL_PARALLEL_CONJ) && \
    defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT)
// This is used to give each context its own unique ID. It is accessed with
// atomic operations.

static MR_ContextId     MR_next_context_id = 0;

// Allocate a context ID.

static MR_ContextId
allocate_context_id(void);
#endif

// free_context_list and free_small_context_list are a global linked lists
// of unused context structures, with regular and small stacks respectively.
// If the MR_MemoryZone pointers are not NULL, then they point to allocated
// MR_MemoryZones.

static MR_Context       *free_context_list = NULL;
#ifndef MR_STACK_SEGMENTS
static MR_Context       *free_small_context_list = NULL;
#endif
#ifdef  MR_THREAD_SAFE
  static MercuryLock    free_context_list_lock;
#endif

#ifdef  MR_LL_PARALLEL_CONJ
MR_Integer volatile         MR_num_idle_ws_engines = 0;
static MR_Integer volatile  MR_num_outstanding_contexts = 0;
static MercurySem           shutdown_ws_semaphore;

static MercuryLock      MR_par_cond_stats_lock;

// This array will contain MR_max_engines pointers to deques.
// The slot i points to the spark deque of engine id i.
// Slots are NULL for unallocated engines.

MR_SparkDeque           **MR_spark_deques = NULL;
#endif

////////////////////////////////////////////////////////////////////////////

#ifdef MR_THREAD_SAFE
// Reset or initialize the cpuset that tracks which CPUs are available for
// binding.

static void     MR_reset_available_cpus(void);

static void     MR_detect_num_processors(void);

  #ifdef MR_LL_PARALLEL_CONJ
static void     MR_setup_num_threads(void);

// Try to wake up a sleeping engine and tell it to do action. The engine is
// only woken if it is in the sleeping state. If the engine is not sleeping
// use try_notify_engine below. If the engine is woken without a race, this
// function returns MR_TRUE, otherwise it returns MR_FALSE.

static MR_bool  try_wake_engine(MR_EngineId engine_id, int action,
                    union MR_engine_wake_action_data *action_data);

// Send a notification to the engine. This is applicable if the engine is
// in any other state (not sleeping). This function does not use the
// semaphore so it cannot wake a sleeping engine. Don't confuse the
// dropable and non-dropable notifications with the notify/wake methods.
// The only connection is that in general non-dropable notifications should
// be used wit try_notify_engine.
//
// The engine's current state must be passed in engine_state as it is used
// with the CAS operation.

static MR_bool  try_notify_engine(MR_EngineId engine_id, int action,
                    union MR_engine_wake_action_data *action_data,
                    MR_Unsigned engine_state);
  #endif    // MR_LL_PARALLEL_CONJ
#endif // MR_THREAD_SAFE

#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
// Write out the profiling data that we collect during execution.

static void     MR_write_out_profiling_parallel_execution(void);
#endif

#if defined(MR_LL_PARALLEL_CONJ) && defined(MR_HAVE_THREAD_PINNING)
static void     MR_setup_thread_pinning(void);

static MR_bool  MR_do_pin_thread(int cpu);

// Determine which CPU this thread is currently running on.

static int      MR_current_cpu(void);

// Mark the given CPU as unavailable for thread pinning. This may mark other
// CPUs as unavailable, if, for instance they share resources with this
// processor and we can place other tasks elsewhere to avoid this sharing.
// These resources are usually only considered for hardware threads that share
// cores.

static void     MR_make_cpu_unavailable(int cpu);
#endif

////////////////////////////////////////////////////////////////////////////

void
MR_init_context_stuff(void)
{
#ifdef MR_LL_PARALLEL_CONJ
    unsigned i;
#endif

#ifdef  MR_THREAD_SAFE

    pthread_mutex_init(&MR_runqueue_lock, MR_MUTEX_ATTR);
    pthread_mutex_init(&free_context_list_lock, MR_MUTEX_ATTR);
    pthread_mutex_init(&MR_pending_contexts_lock, MR_MUTEX_ATTR);
  #ifdef MR_LL_PARALLEL_CONJ
    #ifdef MR_DEBUG_RUNTIME_GRANULARITY_CONTROL
    pthread_mutex_init(&MR_par_cond_stats_lock, MR_MUTEX_ATTR);
    #endif
    MR_sem_init(&shutdown_ws_semaphore, 0);
  #endif
    pthread_mutex_init(&MR_STM_lock, MR_MUTEX_ATTR);

  #ifdef MR_HIGHLEVEL_CODE
    MR_KEY_CREATE(&MR_backjump_handler_key, NULL);
    MR_KEY_CREATE(&MR_backjump_next_choice_id_key, (void *)0);
  #endif

    MR_detect_num_processors();
    assert(MR_num_processors > 0);

  #ifdef MR_LL_PARALLEL_CONJ
    MR_setup_num_threads();
    assert(MR_num_ws_engines > 0);

    #if defined(MR_HAVE_THREAD_PINNING)
    MR_setup_thread_pinning();
    #endif

    MR_granularity_wsdeque_length =
        MR_granularity_wsdeque_length_factor * MR_num_ws_engines;

    MR_spark_deques = MR_GC_NEW_ARRAY_ATTRIB(MR_SparkDeque*,
        MR_max_engines, MR_ALLOC_SITE_RUNTIME);
    for (i = 0; i < MR_max_engines; i++) {
        MR_spark_deques[i] = NULL;
    }

    engine_sleep_sync_data = MR_GC_NEW_ARRAY_ATTRIB(engine_sleep_sync,
        MR_max_engines, MR_ALLOC_SITE_RUNTIME);
    for (i = 0; i < MR_max_engines; i++) {
        engine_sleep_sync *esync = get_engine_sleep_sync(i);
        MR_sem_init(&esync->d.es_sleep_semaphore, 0);
        pthread_mutex_init(&esync->d.es_wake_lock, MR_MUTEX_ATTR);
        // All engines are initially working (because telling them to wake up
        // before they are started would be useless).

        esync->d.es_state = ENGINE_STATE_WORKING;
        esync->d.es_action = MR_ENGINE_ACTION_NONE;
    }
  #endif
#endif // MR_THREAD_SAFE
}

#ifdef MR_THREAD_SAFE
// Detect number of processors.

static void
MR_reset_available_cpus(void)
{
  #if defined(MR_HAVE_HWLOC)
    hwloc_cpuset_t  inherited_binding;

    // Gather the cpuset that our parent process bound this process to.
    //
    // (For information about how to deliberately restrict a process and it's
    // sub-processors to a set of CPUs on Linux see cpuset(7).

    inherited_binding = hwloc_bitmap_alloc();
    hwloc_get_cpubind(MR_hw_topology, inherited_binding, HWLOC_CPUBIND_PROCESS);

    // Set the available processors to the union of inherited_binding and the
    // cpuset we are allowed to use as reported by libhwloc. In my tests with
    // libhwloc_1.0-1 (Debian) hwloc reported that all cpus on the system are
    // available, it didn't exclude cpus not in the processor's cpuset(7).

    if (MR_hw_available_pus == NULL) {
        MR_hw_available_pus = hwloc_bitmap_alloc();
    }
    hwloc_bitmap_and(MR_hw_available_pus, inherited_binding,
        hwloc_topology_get_allowed_cpuset(MR_hw_topology));

    hwloc_bitmap_free(inherited_binding);
  #elif defined(MR_HAVE_SCHED_GETAFFINITY)
    unsigned cpuset_size;
    unsigned num_processors;

    if (MR_cpuset_size) {
        cpuset_size = MR_cpuset_size;
        num_processors = MR_num_processors;
    } else {
      #if defined(MR_HAVE_SYSCONF) && defined(_SC_NPROCESSORS_ONLN)
        num_processors = sysconf(_SC_NPROCESSORS_ONLN);
      #else
        // Make the CPU set at least 32 processors wide.

        num_processors = 32;
      #endif
        cpuset_size = CPU_ALLOC_SIZE(num_processors);
        MR_cpuset_size = cpuset_size;
    }

    if (MR_available_cpus == NULL) {
        MR_available_cpus = CPU_ALLOC(num_processors);
    }

    if (-1 == sched_getaffinity(0, cpuset_size, MR_available_cpus)) {
        MR_perror("Couldn't get CPU affinity");
      #if defined(MR_LL_PARALLEL_CONJ) && defined(MR_HAVE_THREAD_PINNING)
        MR_thread_pinning = MR_FALSE;
      #endif
        CPU_FREE(MR_available_cpus);
        MR_available_cpus = NULL;
    }
  #endif
}

static void
MR_detect_num_processors(void)
{
  #ifdef MR_HAVE_HWLOC
    if (-1 == hwloc_topology_init(&MR_hw_topology)) {
        MR_fatal_error("Error allocating libhwloc topology object");
    }
    if (-1 == hwloc_topology_load(MR_hw_topology)) {
        MR_fatal_error("Error detecting hardware topology (hwloc)");
    }
  #endif

    // Setup num processors.

    MR_reset_available_cpus();
  #ifdef MR_HAVE_HWLOC
    MR_num_processors = hwloc_bitmap_weight(MR_hw_available_pus);
  #elif defined(MR_HAVE_SCHED_GETAFFINITY)
    MR_num_processors = CPU_COUNT_S(MR_cpuset_size, MR_available_cpus);
  #elif defined(MR_WIN32_GETSYSTEMINFO)
    {
        SYSTEM_INFO sysinfo;
        GetSystemInfo(&sysinfo);
        MR_num_processors = sysinfo.dwNumberOfProcessors;
    }
  #elif defined(MR_HAVE_SYSCONF) && defined(_SC_NPROCESSORS_ONLN)
    MR_num_processors = sysconf(_SC_NPROCESSORS_ONLN);
  #else
    #warning "Cannot detect MR_num_processors"
    MR_num_processors = 1;
  #endif
}

#ifdef MR_LL_PARALLEL_CONJ
static void
MR_setup_num_threads(void)
{
    // If MR_num_threads is unset, configure it to match number of processors
    // on the system. If we do this, then we prepare to set processor
    // affinities later on.

    if (MR_num_ws_engines == 0) {
        MR_num_ws_engines = MR_num_processors;
    }

  #ifdef MR_DEBUG_THREADS
    if (MR_debug_threads) {
        fprintf(stderr, "Detected %d processors, will use %d threads\n",
            MR_num_processors, MR_num_ws_engines);
    }
  #endif
}
#endif // MR_LL_PARALLEL_CONJ
#endif // MR_THREAD_SAFE

// Thread pinning.

#if defined(MR_HAVE_THREAD_PINNING) && defined(MR_LL_PARALLEL_CONJ)
// Pin the primordial thread first to the CPU it is currently using
// (if support is available for thread pinning).

static unsigned
MR_pin_thread_no_locking(void)
{
    unsigned    cpu;
    unsigned    i = 0;

    cpu = MR_current_cpu();
#ifdef MR_DEBUG_THREAD_PINNING
    fprintf(stderr, "Currently running on cpu %d\n", cpu);
#endif

    for (i = 0; (i < MR_num_processors) && MR_thread_pinning; i++) {
        if (MR_do_pin_thread((cpu + i) % MR_num_processors)) {
#ifdef MR_DEBUG_THREAD_PINNING
            fprintf(stderr, "Pinned to cpu %d\n", (cpu + i) % MR_num_processors);
            fprintf(stderr, "Now running on cpu %d\n", MR_current_cpu());
#endif
            MR_num_threads_left_to_pin--;
            MR_make_cpu_unavailable((cpu + i) % MR_num_processors);
            return (cpu + i) % MR_num_processors;
        }
        if (!MR_thread_pinning) {
            // If MR_thread_pinning becomes false then an error prevented us
            // from pinning the thread.
            // When we fail to pin a thread but MR_thread_pinning remains true
            // it means that CPU has already had a thread pinned to it.

            fprintf(stderr, "Couldn't pin Mercury engine to processor");
            break;
        }
    }

    return cpu;
}

unsigned
MR_pin_thread(void)
{
    unsigned cpu;

    MR_LOCK(&MR_thread_pinning_lock, "MR_pin_thread");
    cpu = MR_pin_thread_no_locking();
    MR_UNLOCK(&MR_thread_pinning_lock, "MR_pin_thread");

    return cpu;
}

void
MR_pin_primordial_thread(void)
{
    // We don't need locking to pin the primordial thread as it is called
    // before any other threads exist.

    MR_primordial_thread_cpu = MR_pin_thread_no_locking();
}

static void MR_setup_thread_pinning(void)
{
    MR_num_threads_left_to_pin = MR_num_ws_engines;

    pthread_mutex_init(&MR_thread_pinning_lock, MR_MUTEX_ATTR);

  // Comment this back in to enable thread pinning by default
  // if we autodetected the number of CPUs without error.

#if 0
    if (MR_num_processors > 1) {
        MR_thread_pinning = MR_TRUE;
    }
#endif
}

// Determine which CPU this thread is currently running on.

static int MR_current_cpu(void)
{
#if defined(MR_HAVE_SCHED_GETCPU)
    int         os_cpu;
#if defined(MR_HAVE_HWLOC)
    hwloc_obj_t pu;
#endif

    os_cpu = sched_getcpu();
    if (-1 == os_cpu) {
        os_cpu = 0;

        if (MR_thread_pinning) {
            MR_perror("Warning: unable to determine the current CPU for "
                "this thread: ");
        }
    }

#if defined(MR_HAVE_HWLOC)
    pu = hwloc_get_pu_obj_by_os_index(MR_hw_topology, os_cpu);
    return pu->logical_index;
#else
    return os_cpu;
#endif

#else // ! MR_HAVE_SCHED_GETCPU
    // We have no idea!
    return 0;
#endif
}

static MR_bool
MR_do_pin_thread(int cpu)
{
    // Make sure that we are allowed to bind to this CPU.

#if defined(MR_HAVE_HWLOC)
    hwloc_obj_t pu;

    if (hwloc_bitmap_iszero(MR_hw_available_pus)) {
        // Each available CPU already has a thread pinned to it. Reset the
        // available_pus set so that we can oversubscribe CPUs but still
        // attempt to balance load.

        MR_reset_available_cpus();
    }

    pu = hwloc_get_obj_by_type(MR_hw_topology, HWLOC_OBJ_PU, cpu);
    if (!hwloc_bitmap_intersects(MR_hw_available_pus, pu->cpuset)) {
        return MR_FALSE;
    }
#elif defined(MR_HAVE_SCHED_SETAFFINITY)
    if (CPU_COUNT_S(MR_cpuset_size, MR_available_cpus) == 0) {
        // As above, reset the available cpus.

        MR_reset_available_cpus();
    }
    if (!CPU_ISSET_S(cpu, MR_cpuset_size, MR_available_cpus)) {
        return MR_FALSE;
    }
#endif

#if defined(MR_HAVE_HWLOC)
    errno = hwloc_set_cpubind(MR_hw_topology, pu->cpuset,
        HWLOC_CPUBIND_THREAD);
    if (errno != 0) {
        MR_perror("Warning: Couldn't set CPU affinity: ");
        MR_thread_pinning = MR_FALSE;
        return MR_FALSE;
    }
#elif defined(MR_HAVE_SCHED_SETAFFINITY)
    cpu_set_t   *cpus;

    cpus = CPU_ALLOC(MR_num_processors);

    CPU_ZERO_S(MR_cpuset_size, cpus);
    CPU_SET_S(cpu, MR_cpuset_size, cpus);
    if (sched_setaffinity(0, MR_cpuset_size, cpus) == -1) {
        MR_perror("Warning: Couldn't set CPU affinity: ");
        // If this failed once, it will probably fail again, so we disable it.

        MR_thread_pinning = MR_FALSE;
        return MR_FALSE;
    }
#endif

    return MR_TRUE;
}

#if defined(MR_HAVE_HWLOC)
static MR_bool  MR_make_pu_unavailable(const struct hwloc_obj *pu);
#endif

static void MR_make_cpu_unavailable(int cpu)
{
#if defined(MR_HAVE_HWLOC)
    hwloc_obj_t pu;
    pu = hwloc_get_obj_by_type(MR_hw_topology, HWLOC_OBJ_PU, cpu);
    MR_make_pu_unavailable(pu);
#elif defined(MR_HAVE_SCHED_SETAFFINITY)
    CPU_CLR_S(cpu, MR_cpuset_size, MR_available_cpus);
#endif
}

#if defined(MR_HAVE_HWLOC)
static MR_bool MR_make_pu_unavailable(const struct hwloc_obj *pu)
{
    hwloc_obj_t core;
    static int  siblings_to_make_unavailable;
    int         i;

#ifdef MR_DEBUG_THREAD_PINNING
    char        *cpusetstr;

    hwloc_bitmap_asprintf(&cpusetstr, MR_hw_available_pus);
    fprintf(stderr, "Old available CPU set: %s\n", cpusetstr);
    free(cpusetstr);
    hwloc_bitmap_asprintf(&cpusetstr, pu->cpuset);
    fprintf(stderr, "Making this CPU set unavailable: %s\n", cpusetstr);
    free(cpusetstr);
#endif

    hwloc_bitmap_andnot(MR_hw_available_pus, MR_hw_available_pus, pu->cpuset);

#ifdef MR_DEBUG_THREAD_PINNING
    hwloc_bitmap_asprintf(&cpusetstr, MR_hw_available_pus);
    fprintf(stderr, "New available CPU set: %s\n", cpusetstr);
    free(cpusetstr);
#endif

    siblings_to_make_unavailable = hwloc_bitmap_weight(MR_hw_available_pus) -
        MR_num_threads_left_to_pin;

    if (siblings_to_make_unavailable > 0) {
        // Remove sibling processing units that share a core with the one
        // we have just removed.

        core = pu->parent;
        if (core->type != HWLOC_OBJ_CORE) {
            return MR_FALSE;
        }

        for (i = 0;
             (i < core->arity && siblings_to_make_unavailable > 0);
             i++) {
            if (core->children[i] == pu) {
                continue;
            }
            if (hwloc_bitmap_intersects(core->children[i]->cpuset,
                    MR_hw_available_pus)) {
                if (!MR_make_pu_unavailable(core->children[i])) {
                    return MR_FALSE;
                }
            }
        }
    }

    return MR_TRUE;
}
#endif

#endif // MR_HAVE_THREAD_PINNING && MR_LL_PARALLEL_CONJ

void
MR_finalize_context_stuff(void)
{
#ifdef MR_THREAD_SAFE
    pthread_mutex_destroy(&MR_runqueue_lock);
    pthread_mutex_destroy(&free_context_list_lock);
  #ifdef MR_LL_PARALLEL_CONJ
    MR_sem_destroy(&shutdown_ws_semaphore);
  #endif
#endif

#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
    if (MR_profile_parallel_execution) {
        MR_write_out_profiling_parallel_execution();
    }
#endif
}

#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
static int
fprint_stats(FILE *stream, const char *message, MR_Stats *stats);

// Write out the profiling data for parallel execution.
//
// This writes out a flat text file which may be parsed by a machine or easily
// read by a human. There is no advantage in using a binary format since we
// do this once at the end of execution and it is a small amount of data.
// Therefore we use a text file, since it has the advantage of being human
// readable.

static void
MR_write_out_profiling_parallel_execution(void)
{
    FILE    *file;
    int     result;

    file = fopen(MR_PROFILE_PARALLEL_EXECUTION_FILENAME, "w");
    if (NULL == file) goto Error;

    result = fprintf(file, "Mercury parallel execution profiling data\n\n");
    if (result < 0) goto Error;

    if (MR_cpu_cycles_per_sec) {
        result = fprintf(file, "CPU cycles per second: %ld\n",
            MR_cpu_cycles_per_sec);
        if (result < 0) goto Error;
    }

    result = fprint_stats(file, "MR_do_runnext(): global sparks executed",
        &MR_profile_parallel_executed_global_sparks);
    if (result < 0) goto Error;

    result = fprint_stats(file, "MR_do_runnext(): global contexts resumed",
        &MR_profile_parallel_executed_contexts);
    if (result < 0) goto Error;

    result = fprint_stats(file, "MR_do_runnext(): executed nothing",
        &MR_profile_parallel_executed_nothing);
    if (result < 0) goto Error;

    result = fprint_stats(file, "Local sparks executed",
        &MR_profile_parallel_executed_local_sparks);
    if (result < 0) goto Error;

    result = fprintf(file, "Contexts created for global spark execution: %d\n",
        MR_profile_parallel_contexts_created_for_sparks);
    if (result < 0) goto Error;

    result = fprintf(file, "Number of times a small context was reused: %d\n",
        MR_profile_parallel_small_context_reused);
    if (result < 0) goto Error;

    result = fprintf(file,
        "Number of times a regular context was reused: %d\n",
        MR_profile_parallel_regular_context_reused);
    if (result < 0) goto Error;

    result = fprintf(file,
        "Number of times a small context was kept for later use: %d\n",
        MR_profile_parallel_small_context_kept);
    if (result < 0) goto Error;

    result = fprintf(file,
        "Number of times a regular context was kept for later use: %d\n",
        MR_profile_parallel_regular_context_kept);
    if (result < 0) goto Error;

    if (fclose(file) != 0) goto Error;

    return;

Error:
    MR_perror(MR_PROFILE_PARALLEL_EXECUTION_FILENAME);
    abort();
}

#define MR_FPRINT_STATS_FORMAT_STRING_FULL                        \
    ("%s: count %" MR_INTEGER_LENGTH_MODIFIER "u (%"              \
    MR_INTEGER_LENGTH_MODIFIER "ur, %" MR_INTEGER_LENGTH_MODIFIER \
    "unr), average %.0f, standard deviation %.0f\n")
#define MR_FPRINT_STATS_FORMAT_STRING_SINGLE                      \
    ("%s: count %" MR_INTEGER_LENGTH_MODIFIER "u (%"              \
    MR_INTEGER_LENGTH_MODIFIER "ur, %" MR_INTEGER_LENGTH_MODIFIER \
    "unr), sample %ul\n")
#define MR_FPRINT_STATS_FORMAT_STRING_NONE           \
    ("%s: count %" MR_INTEGER_LENGTH_MODIFIER "u (%" \
    MR_INTEGER_LENGTH_MODIFIER "ur, %" MR_INTEGER_LENGTH_MODIFIER "unr)\n")

static int
fprint_stats(FILE *stream, const char *message, MR_Stats *stats)
{
    MR_Unsigned     count;
    double          average;
    double          sum_squared_over_n;
    double          standard_deviation;

    count = (unsigned)(stats->MR_stat_count_recorded +
        stats->MR_stat_count_not_recorded);

    if (stats->MR_stat_count_recorded > 1) {
        average = (double)stats->MR_stat_sum /
            (double)stats->MR_stat_count_recorded;
        sum_squared_over_n = pow((double)stats->MR_stat_sum,2.0)/
            (double)stats->MR_stat_count_recorded;
        standard_deviation =
            sqrt(((double)stats->MR_stat_sum_squares - sum_squared_over_n) /
            (double)(stats->MR_stat_count_recorded - 1));

        return fprintf(stream, MR_FPRINT_STATS_FORMAT_STRING_FULL, message,
            count, stats->MR_stat_count_recorded,
            stats->MR_stat_count_not_recorded, average, standard_deviation);
    } else if (stats->MR_stat_count_recorded == 1) {
        return fprintf(stream, MR_FPRINT_STATS_FORMAT_STRING_SINGLE,
            message, count, stats->MR_stat_count_recorded,
            stats->MR_stat_count_not_recorded, stats->MR_stat_sum);
    } else {
        return fprintf(stream, MR_FPRINT_STATS_FORMAT_STRING_NONE,
            message, count, stats->MR_stat_count_recorded,
            stats->MR_stat_count_not_recorded);
    }
};

#endif // MR_PROFILE_PARALLEL_EXECUTION_SUPPORT

static void
MR_init_context_maybe_generator(MR_Context *c, const char *id,
    MR_GeneratorPtr gen)
{

#ifndef MR_HIGHLEVEL_CODE
    const char  *detstack_name;
    const char  *nondetstack_name;
    size_t      detstack_size;
    size_t      nondetstack_size;
#endif

    c->MR_ctxt_id = id;
    c->MR_ctxt_next = NULL;
    c->MR_ctxt_resume = NULL;
#ifdef  MR_THREAD_SAFE
    c->MR_ctxt_exclusive_engine = MR_ENGINE_ID_NONE;
    c->MR_ctxt_resume_engine = 0;
    c->MR_ctxt_resume_engine_required = MR_FALSE;
    c->MR_ctxt_resume_c_depth = 0;
    c->MR_ctxt_resume_stack = NULL;
#endif

#ifndef MR_HIGHLEVEL_CODE
    c->MR_ctxt_succip = MR_ENTRY(MR_do_not_reached);

    switch (c->MR_ctxt_size) {
        case MR_CONTEXT_SIZE_REGULAR:
            detstack_name  = "detstack";
            nondetstack_name = "nondetstack";
            detstack_size  = MR_detstack_size;
            nondetstack_size = MR_nondetstack_size;
            break;
#ifndef MR_STACK_SEGMENTS
        case MR_CONTEXT_SIZE_SMALL:
            detstack_name  = "small_detstack";
            nondetstack_name = "small_nondetstack";
            detstack_size  = MR_small_detstack_size;
            nondetstack_size = MR_small_nondetstack_size;
            break;
#endif
    }

#ifdef MR_DEBUG_CONTEXT_CREATION_SPEED
    MR_debug_log_message("Allocating det stack");
#endif
    if (c->MR_ctxt_detstack_zone == NULL) {
        if (gen != NULL) {
            c->MR_ctxt_detstack_zone = MR_create_or_reuse_zone("gen_detstack",
                    MR_gen_detstack_size, MR_next_offset(),
                    MR_gen_detstack_zone_size, MR_default_handler);
        } else {
            c->MR_ctxt_detstack_zone = MR_create_or_reuse_zone(detstack_name,
                    detstack_size, MR_next_offset(),
                    MR_detstack_zone_size, MR_default_handler);
        }

        if (c->MR_ctxt_prev_detstack_zones != NULL) {
            // We may be able to reuse a previously allocated stack, but
            // a context should be reused only when its stacks are empty.

            MR_fatal_error("MR_init_context_maybe_generator: prev det stack");
        }
    }
#ifdef MR_DEBUG_CONTEXT_CREATION_SPEED
    MR_debug_log_message("done");
#endif
    c->MR_ctxt_prev_detstack_zones = NULL;
    c->MR_ctxt_sp = c->MR_ctxt_detstack_zone->MR_zone_min;

#ifdef MR_DEBUG_CONTEXT_CREATION_SPEED
    MR_debug_log_message("Allocating nondet stack");
#endif
    if (c->MR_ctxt_nondetstack_zone == NULL) {
        if (gen != NULL) {
            c->MR_ctxt_nondetstack_zone =
                MR_create_or_reuse_zone("gen_nondetstack",
                    MR_gen_nondetstack_size, MR_next_offset(),
                    MR_gen_nondetstack_zone_size, MR_default_handler);
        } else {
            c->MR_ctxt_nondetstack_zone =
                MR_create_or_reuse_zone(nondetstack_name,
                    nondetstack_size, MR_next_offset(),
                    MR_nondetstack_zone_size, MR_default_handler);
        }

        if (c->MR_ctxt_prev_nondetstack_zones != NULL) {
            // We may be able to reuse a previously allocated stack, but
            // a context should be reused only when its stacks are empty.

            MR_fatal_error(
                "MR_init_context_maybe_generator: prev nondet stack");
        }
    }
#ifdef MR_DEBUG_CONTEXT_CREATION_SPEED
    MR_debug_log_message("done");
#endif
    c->MR_ctxt_prev_nondetstack_zones = NULL;
    // Note that maxfr and curfr point to the last word in the frame,
    // not to the first word, so we need to add the size of the frame,
    // minus one word, to the base address to get the maxfr/curfr pointer
    // for the first frame on the nondet stack.

    c->MR_ctxt_maxfr = c->MR_ctxt_nondetstack_zone->MR_zone_min +
        MR_NONDET_FIXED_SIZE - 1;
    c->MR_ctxt_curfr = c->MR_ctxt_maxfr;
    MR_redoip_slot_word(c->MR_ctxt_curfr) = (MR_Word)
        MR_ENTRY(MR_do_not_reached);
    MR_redofr_slot_word(c->MR_ctxt_curfr) = (MR_Word) NULL;
    MR_prevfr_slot_word(c->MR_ctxt_curfr) = (MR_Word) NULL;
    MR_succip_slot_word(c->MR_ctxt_curfr) = (MR_Word)
        MR_ENTRY(MR_do_not_reached);
    MR_succfr_slot_word(c->MR_ctxt_curfr) = (MR_Word) NULL;

  #ifdef MR_USE_MINIMAL_MODEL_STACK_COPY
    if (gen != NULL) {
        MR_fatal_error("MR_init_context_maybe_generator: "
            "generator and stack_copy");
    }

    if (c->MR_ctxt_genstack_zone == NULL) {
        c->MR_ctxt_genstack_zone = MR_create_or_reuse_zone("genstack",
            MR_genstack_size, MR_next_offset(),
            MR_genstack_zone_size, MR_default_handler);
    }
    c->MR_ctxt_gen_next = 0;

    if (c->MR_ctxt_cutstack_zone == NULL) {
        c->MR_ctxt_cutstack_zone = MR_create_or_reuse_zone("cutstack",
            MR_cutstack_size, MR_next_offset(),
            MR_cutstack_zone_size, MR_default_handler);
    }
    c->MR_ctxt_cut_next = 0;

    if (c->MR_ctxt_pnegstack_zone == NULL) {
        c->MR_ctxt_pnegstack_zone = MR_create_or_reuse_zone("pnegstack",
            MR_pnegstack_size, MR_next_offset(),
            MR_pnegstack_zone_size, MR_default_handler);
    }
    c->MR_ctxt_pneg_next = 0;
  #endif // MR_USE_MINIMAL_MODEL_STACK_COPY

  #ifdef MR_USE_MINIMAL_MODEL_OWN_STACKS
    c->MR_ctxt_owner_generator = gen;
  #endif // MR_USE_MINIMAL_MODEL_OWN_STACKS

  #ifdef MR_LL_PARALLEL_CONJ
    c->MR_ctxt_parent_sp = NULL;
  #endif // MR_LL_PARALLEL_CONJ

#endif // !MR_HIGHLEVEL_CODE

#ifdef MR_USE_TRAIL
    if (gen != NULL) {
        MR_fatal_error("MR_init_context_maybe_generator: generator and trail");
    }

    if (c->MR_ctxt_trail_zone == NULL) {
        c->MR_ctxt_trail_zone = MR_create_or_reuse_zone("trail",
            MR_trail_size, MR_next_offset(),
            MR_trail_zone_size, MR_default_handler);
    }
    c->MR_ctxt_trail_ptr =
        (MR_TrailEntry *) c->MR_ctxt_trail_zone->MR_zone_min;
    c->MR_ctxt_ticket_counter = 1;
    c->MR_ctxt_ticket_high_water = 1;
#endif

#ifndef MR_HIGHLEVEL_CODE
    c->MR_ctxt_backjump_handler = NULL;
    c->MR_ctxt_backjump_next_choice_id = 0;
#endif

#ifndef MR_CONSERVATIVE_GC
    if (gen != NULL) {
        MR_fatal_error("MR_init_context: generator and no conservative gc");
    }

    c->MR_ctxt_hp = NULL;
    c->MR_ctxt_min_hp_rec = NULL;
#endif

#ifdef  MR_EXEC_TRACE_INFO_IN_CONTEXT
    c->MR_ctxt_call_seqno = 0;
    c->MR_ctxt_call_depth = 0;
    c->MR_ctxt_event_number = 0;
#endif

    // The caller is responsible for initialising this field.
    c->MR_ctxt_thread_local_mutables = NULL;
}

MR_Context *
MR_create_context(const char *id, MR_ContextSize ctxt_size, MR_Generator *gen)
{
    MR_Context  *c = NULL;

#ifdef MR_LL_PARALLEL_CONJ
    MR_atomic_inc_int(&MR_num_outstanding_contexts);
#endif

    MR_LOCK(&free_context_list_lock, "create_context");

    // Regular contexts have stacks at least as big as small contexts,
    // so we can return a regular context in place of a small context
    // if one is already available.

#ifndef MR_STACK_SEGMENTS
    if (ctxt_size == MR_CONTEXT_SIZE_SMALL && free_small_context_list) {
        c = free_small_context_list;
        free_small_context_list = c->MR_ctxt_next;
#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
        if (MR_profile_parallel_execution) {
            MR_profile_parallel_small_context_reused++;
        }
#endif
    }
#endif
    if (c == NULL && free_context_list != NULL) {
        c = free_context_list;
        free_context_list = c->MR_ctxt_next;
#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
        if (MR_profile_parallel_execution) {
            MR_profile_parallel_regular_context_reused++;
        }
#endif
    }
    MR_UNLOCK(&free_context_list_lock, "create_context i");

    if (c != NULL) {
#ifdef MR_THREADSCOPE
        MR_Unsigned old_id = c->MR_ctxt_num_id;

        c->MR_ctxt_num_id = allocate_context_id();
        MR_threadscope_post_reuse_context(c, old_id);
#endif
#ifdef MR_DEBUG_STACK_SEGMENTS
        MR_debug_log_message("Re-used an old context: %p", c);
#endif
    }
    else {
        c = MR_GC_NEW_ATTRIB(MR_Context, MR_ALLOC_SITE_RUNTIME);
#ifdef MR_DEBUG_STACK_SEGMENTS
        if (c) {
            MR_debug_log_message("Creating new context: %p", c);
        }
#endif
        c->MR_ctxt_size = ctxt_size;
#ifndef MR_HIGHLEVEL_CODE
        c->MR_ctxt_detstack_zone = NULL;
        c->MR_ctxt_nondetstack_zone = NULL;
#endif
#ifdef MR_USE_TRAIL
        c->MR_ctxt_trail_zone = NULL;
#endif
#ifdef MR_THREADSCOPE
        c->MR_ctxt_num_id = allocate_context_id();
        MR_threadscope_post_create_context(c);
#endif
    }

#ifdef MR_DEBUG_CONTEXT_CREATION_SPEED
    MR_debug_log_message("Calling MR_init_context_maybe_generator");
#endif
    MR_init_context_maybe_generator(c, id, gen);
    return c;
}

// TODO: We should gc the cached contexts, or otherwise not cache too many.

void
MR_release_context(MR_Context *c)
{
    MR_assert(c);

#ifdef MR_THREADSCOPE
    MR_threadscope_post_release_context(c);
#endif

#ifdef MR_THREAD_SAFE
    MR_assert(c->MR_ctxt_resume_stack == NULL);
#endif

    // TODO: When retrieving a context from the cached contexts, try to
    // retrieve one with a matching engine ID, or give each engine a local
    // cache of spare contexts.
#ifdef MR_LL_PARALLEL_CONJ
    c->MR_ctxt_resume_engine = MR_ENGINE(MR_eng_id);
#endif

    // XXX Not sure if this is an overall win yet.
#if 0 && defined(MR_CONSERVATIVE_GC) && !defined(MR_HIGHLEVEL_CODE)
    // Clear stacks to prevent retention of data.
    MR_clear_zone_for_GC(c->MR_ctxt_detstack_zone,
        c->MR_ctxt_detstack_zone->MR_zone_min);
    MR_clear_zone_for_GC(c->MR_ctxt_nondetstack_zone,
        c->MR_ctxt_nondetstack_zone->MR_zone_min);
#endif // defined(MR_CONSERVATIVE_GC) && !defined(MR_HIGHLEVEL_CODE)

    c->MR_ctxt_thread_local_mutables = NULL;

#ifdef MR_LL_PARALLEL_CONJ
    MR_atomic_dec_int(&MR_num_outstanding_contexts);
#endif

    MR_LOCK(&free_context_list_lock, "release_context");
    switch (c->MR_ctxt_size) {
        case MR_CONTEXT_SIZE_REGULAR:
            c->MR_ctxt_next = free_context_list;
            free_context_list = c;
#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
            if (MR_profile_parallel_execution) {
                MR_profile_parallel_regular_context_kept++;
            }
#endif
            break;
#ifndef MR_STACK_SEGMENTS
        case MR_CONTEXT_SIZE_SMALL:
            c->MR_ctxt_next = free_small_context_list;
            free_small_context_list = c;
#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
            if (MR_profile_parallel_execution) {
                MR_profile_parallel_small_context_kept++;
            }
#endif
            break;
#endif
    }
    MR_UNLOCK(&free_context_list_lock, "release_context");
}

#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
static MR_ContextId
allocate_context_id(void) {
    return MR_atomic_add_and_fetch_int(&MR_next_context_id, 1);
}
#endif

#ifdef MR_LL_PARALLEL_CONJ

// Search for a ready context which we can handle.
static MR_Context *
MR_find_ready_context(void)
{
    MR_Context  *cur;
    MR_Context  *prev;
    MR_Context  *preferred_context;
    MR_Context  *preferred_context_prev;
    MR_EngineId engine_id = MR_ENGINE(MR_eng_id);
    MR_Unsigned depth = MR_ENGINE(MR_eng_c_depth);

    // XXX check pending io

    // Give preference to contexts as follows:
    //
    //  A context that must be run on this engine.
    //  A context that prefers to be run on this engine.
    //  Any runnable context that may be ran on this engine.
    //
    // TODO: There are other scheduling decisions we should test, such as
    // running older versus younger contexts, or more recently stopped/runnable
    // contexts.

    cur = MR_runqueue_head;
    prev = NULL;
    preferred_context = NULL;
    preferred_context_prev = NULL;
    while (cur != NULL) {
      #ifdef MR_DEBUG_THREADS
        if (MR_debug_threads) {
            fprintf(stderr,
                "%ld Eng: %d, c_depth: %" MR_INTEGER_LENGTH_MODIFIER
                "u, Considering context %p\n",
                MR_SELF_THREAD_ID, engine_id, depth, cur);
        }
      #endif

        if (cur->MR_ctxt_resume_engine_required == MR_TRUE) {
          #ifdef MR_DEBUG_THREADS
            if (MR_debug_threads) {
                fprintf(stderr,
                    "%ld Context %p requires engine %d and c_depth %"
                    MR_INTEGER_LENGTH_MODIFIER "u\n",
                    MR_SELF_THREAD_ID, cur, cur->MR_ctxt_resume_engine,
                    cur->MR_ctxt_resume_c_depth);
            }
          #endif
            if ((cur->MR_ctxt_resume_engine == engine_id) &&
                (cur->MR_ctxt_resume_c_depth == depth))
            {
                preferred_context = cur;
                preferred_context_prev = prev;
                cur->MR_ctxt_resume_engine_required = MR_FALSE;
                // This is the best thread to resume.

                break;
            }
        } else if (cur->MR_ctxt_exclusive_engine != MR_ENGINE_ID_NONE) {
          #ifdef MR_DEBUG_THREADS
            if (MR_debug_threads) {
                fprintf(stderr,
                    "%ld Context %p requires exclusive engine %d\n",
                    MR_SELF_THREAD_ID, cur, cur->MR_ctxt_exclusive_engine);
            }
          #endif
            if (cur->MR_ctxt_exclusive_engine == engine_id) {
                // This context is exclusive to this engine.

                preferred_context = cur;
                preferred_context_prev = prev;
                break;
            }
        } else {
          #ifdef MR_DEBUG_THREADS
            if (MR_debug_threads) {
                fprintf(stderr, "%ld Context prefers engine %d\n",
                    MR_SELF_THREAD_ID, cur->MR_ctxt_resume_engine);
            }
          #endif
            if (cur->MR_ctxt_resume_engine == engine_id) {
                // This context prefers to be ran on this engine.

                preferred_context = cur;
                preferred_context_prev = prev;
            } else if (preferred_context == NULL) {
                // There is no preferred context yet, and this context is okay.

                preferred_context = cur;
                preferred_context_prev = prev;
            }
        }

        prev = cur;
        cur = cur->MR_ctxt_next;
    }

    if (preferred_context != NULL) {
        if (preferred_context_prev != NULL) {
            preferred_context_prev->MR_ctxt_next =
                preferred_context->MR_ctxt_next;
        } else {
            MR_runqueue_head = preferred_context->MR_ctxt_next;
        }
        if (MR_runqueue_tail == preferred_context) {
            MR_runqueue_tail = preferred_context_prev;
        }
      #ifdef MR_DEBUG_THREADS
        if (MR_debug_threads) {
            fprintf(stderr, "%ld Will run context %p\n",
                MR_SELF_THREAD_ID, preferred_context);
        }
      #endif
    } else {
      #ifdef MR_DEBUG_THREADS
        if (MR_debug_threads) {
            fprintf(stderr, "%ld No suitable context to run\n",
                MR_SELF_THREAD_ID);
        }
      #endif
    }

    return preferred_context;
}

static MR_bool
MR_attempt_steal_spark(MR_Spark *spark)
{
    int             i;
    int             offset;
    int             victim_id;
    int             max_victim_id;
    MR_SparkDeque   *victim;
    int             steal_result;
    MR_bool         result = MR_FALSE;

    offset = MR_ENGINE(MR_eng_victim_counter);

    // This is the highest victim to attempt stealing from. We do not
    // steal from exclusive engines, numbered from MR_num_ws_engines up.
    // To try that out, set max_victim_id to MR_highest_engine_id and
    // change the condition in MR_fork_new_child.

    max_victim_id = MR_num_ws_engines;

    for (i = 0; i < max_victim_id; i++) {
        victim_id = (i + offset) % max_victim_id;
        if (victim_id == MR_ENGINE(MR_eng_id)) {
            // There is no point in stealing from ourselves.
            continue;
        }
        // The victim engine may be shutting down as we attempt to steal from
        // it. However, the spark deque is allocated separately so that it may
        // outlive the engine, and since the spark deque must be empty when the
        // engine is destroyed, any attempt to steal from it must fail.

        victim = MR_spark_deques[victim_id];
        if (victim != NULL) {
            steal_result = MR_wsdeque_steal_top(victim, spark);
            // If we lost a race to steal the spark, we continue to attempt
            // to steal the spark until we succeed (steal_result == 1) or
            // until the deque is empty (steal_result == 0)

            while (steal_result == -1) {
                MR_ATOMIC_PAUSE;
                steal_result = MR_wsdeque_steal_top(victim, spark);
            }
            if (steal_result == 1) {
                // Steal successful.
                result = MR_TRUE;
                break;
            }
        }
    }

    MR_ENGINE(MR_eng_victim_counter) = victim_id;
    return result;
}

static void
MR_milliseconds_from_now(struct timespec *timeout, unsigned int msecs)
{
#if defined(MR_HAVE_GETTIMEOFDAY)

    const long          NANOSEC_PER_SEC = 1000000000L;
    struct timeval      now;
    MR_int_least64_t    nanosecs;

    gettimeofday(&now, NULL);
    timeout->tv_sec = now.tv_sec;
    nanosecs = ((MR_int_least64_t) (now.tv_usec + (msecs * 1000))) * 1000L;
    if (nanosecs >= NANOSEC_PER_SEC) {
        timeout->tv_sec++;
        nanosecs %= NANOSEC_PER_SEC;
    }
    timeout->tv_nsec = (long) nanosecs;

#elif defined(MR_WIN32)

    const long          NANOSEC_PER_SEC = 1000000000L;
    const long          NANOSEC_PER_MILLISEC = 1000000L;
    struct _timeb       now;
    MR_int_least64_t    nanosecs;

    _ftime(&now);
    timeout->tv_sec = now.time;
    nanosecs = ((MR_int_least64_t) (msecs + now.millitm)) *
        NANOSEC_PER_MILLISEC;
    if (nanosecs >= NANOSEC_PER_SEC) {
        timeout->tv_sec++;
        nanosecs %= NANOSEC_PER_SEC;
    }
    timeout->tv_nsec = (long) nanosecs;

#else

    #error Missing definition of MR_milliseconds_from_now.

#endif
}

#endif  // MR_LL_PARALLEL_CONJ

void
MR_flounder(void)
{
    MR_fatal_error("computation floundered");
}

void
MR_sched_yield(void)
{
#if defined(MR_HAVE_SCHED_YIELD)
    sched_yield();
#elif defined(MR_CAN_DO_PENDING_IO)
    struct timeval timeout = {0, 1};
    select(0, NULL, NULL, NULL, &timeout);
#endif
}

#ifndef MR_HIGHLEVEL_CODE
// Check to see if any contexts that blocked on IO have become runnable.
// Return the number of contexts that are still blocked.
// The parameter specifies whether or not the call to select should block.

static int
MR_check_pending_contexts(MR_bool block)
{
#ifdef  MR_CAN_DO_PENDING_IO
    int                 err;
    int                 max_id;
    int                 n_ids;
    fd_set              rd_set0;
    fd_set              wr_set0;
    fd_set              ex_set0;
    fd_set              rd_set;
    fd_set              wr_set;
    fd_set              ex_set;
    struct timeval      timeout;
    MR_PendingContext   *pctxt;

    if (MR_pending_contexts == NULL) {
        return 0;
    }

    MR_fd_zero(&rd_set0);
    MR_fd_zero(&wr_set0);
    MR_fd_zero(&ex_set0);
    max_id = -1;
    for (pctxt = MR_pending_contexts ; pctxt ; pctxt = pctxt -> next) {
        if (pctxt->waiting_mode & MR_PENDING_READ) {
            if (max_id > pctxt->fd) {
                max_id = pctxt->fd;
            }
            FD_SET(pctxt->fd, &rd_set0);
        }
        if (pctxt->waiting_mode & MR_PENDING_WRITE) {
            if (max_id > pctxt->fd) {
                max_id = pctxt->fd;
            }
            FD_SET(pctxt->fd, &wr_set0);
        }
        if (pctxt->waiting_mode & MR_PENDING_EXEC) {
            if (max_id > pctxt->fd) {
                max_id = pctxt->fd;
            }
            FD_SET(pctxt->fd, &ex_set0);
        }
    }
    max_id++;

    if (max_id == 0) {
        MR_fatal_error("no fd's set!");
    }

    if (block) {
        do {
            rd_set = rd_set0;
            wr_set = wr_set0;
            ex_set = ex_set0;
            err = select(max_id, &rd_set, &wr_set, &ex_set, NULL);
        } while (err == -1 && MR_is_eintr(errno));
    } else {
        do {
            rd_set = rd_set0;
            wr_set = wr_set0;
            ex_set = ex_set0;
            timeout.tv_sec = 0;
            timeout.tv_usec = 0;
            err = select(max_id, &rd_set, &wr_set, &ex_set, &timeout);
        } while (err == -1 && MR_is_eintr(errno));
    }

    if (err < 0) {
        MR_fatal_error("select failed!");
    }

    n_ids = 0;
    for (pctxt = MR_pending_contexts; pctxt; pctxt = pctxt -> next) {
        n_ids++;
        if (    ((pctxt->waiting_mode & MR_PENDING_READ)
                && FD_ISSET(pctxt->fd, &rd_set))
            ||  ((pctxt->waiting_mode & MR_PENDING_WRITE)
                && FD_ISSET(pctxt->fd, &wr_set))
            ||  ((pctxt->waiting_mode & MR_PENDING_EXEC)
                && FD_ISSET(pctxt->fd, &ex_set))
            )
        {
            MR_schedule_context(pctxt->context);
        }
    }

    return n_ids;

#else   // !MR_CAN_DO_PENDING_IO

    MR_fatal_error("select() unavailable!");

#endif // !MR_CAN_DO_PENDING_IO
}
#endif // not MR_HIGHLEVEL_CODE

void
MR_schedule_context(MR_Context *ctxt)
{
#ifdef MR_LL_PARALLEL_CONJ
    MR_EngineId                         engine_id;
    MR_bool                             engine_required;
    union MR_engine_wake_action_data    notify_context_data;
    engine_sleep_sync                   *esync;

    notify_context_data.MR_ewa_context = ctxt;

#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
    MR_threadscope_post_context_runnable(ctxt);
#endif

    // Try to give this context straight to the engine that would execute it.

    if (ctxt->MR_ctxt_resume_engine_required == MR_TRUE) {
        engine_id = ctxt->MR_ctxt_resume_engine;
        engine_required = MR_TRUE;
    } else if (ctxt->MR_ctxt_exclusive_engine != MR_ENGINE_ID_NONE) {
        engine_id = ctxt->MR_ctxt_exclusive_engine;
        engine_required = MR_TRUE;
    } else {
        engine_id = ctxt->MR_ctxt_resume_engine;
        engine_required = MR_FALSE;
    }

    esync = get_engine_sleep_sync(engine_id);
#ifdef MR_DEBUG_THREADS
    if (MR_debug_threads) {
        fprintf(stderr,
            "%ld Scheduling context %p desired engine: %d required: %d\n",
            MR_SELF_THREAD_ID, ctxt, engine_id, engine_required);
    }
#endif
    if (engine_required) {
        // Only engine_id may execute this context, attempt to wake it.
        //
        // Note that there is a race condition here. If the engine that can
        // run this context is working, then try_wake_engine() will fail, if
        // it then becomes idle and checks the run queue before we acquire
        // the run queue lock below then it can go to sleep and won't be
        // notified that there is a context to execute. The context will be
        // placed on the run queue awaiting the engine. If the context can
        // only be executed by a single engine, then that engine will only
        // check the run queue if it first executes a spark, causing it to
        // call MR_do_idle after completing the spark.
        //
        // This is only a problem for contexts that can only be executed on
        // a single engine. In other causes this engine is guaranteed to
        // eventually call MR_do_idle and execute the context. Potentially
        // causing a loss of parallelism but not a deadlock.
        //
        // We can fix this race by adding an extra message, which we
        // tentatively call MR_ENGINE_ACTION_CONTEXT_ADVICE, which does not
        // contain a context but tells an engine that one is available.
        // After placing a context on the run queue we can deliver this
        // message to an idle engine that should check the run queue if it
        // hasn't already. We must also guarantee that an engine checks if
        // it has any notifications before going into the sleeping state.
        //
        // I have a workspace in which I fix these problems, however it is
        // buggy in other ways so I cannot commit it yet. For now I'm
        // documenting it in this comment.
        //
        // See runtime/design/par_engine_state.{txt,dot} for details of the
        // proposed changes to the engine notification code. Although the
        // proposed changes in these files are much more complex than is
        // strictly needed, we believe that they avoid other problems.

#ifdef MR_DEBUG_THREADS
        if (MR_debug_threads) {
            fprintf(stderr, "%ld Context _must_ run on engine %d\n",
                MR_SELF_THREAD_ID, engine_id);
        }
#endif

        // Only engine_id may execute this context, if it is sleeping
        // attempt to wake it.

        if (esync->d.es_state == ENGINE_STATE_SLEEPING) {
            if (try_wake_engine(engine_id, MR_ENGINE_ACTION_CONTEXT,
                &notify_context_data))
            {
                // We have successfully given the context to the correct engine.
                return;
            }
        }
    } else {
        // If there is some idle engine, try to wake it up, starting with the
        // preferred engine.

        if (MR_num_idle_ws_engines > 0) {
            if (MR_try_wake_ws_engine(engine_id, MR_ENGINE_ACTION_CONTEXT,
                &notify_context_data, NULL))
            {
                // The context has been given to an engine.
                return;
            }
        }
    }
#endif // MR_LL_PARALLEL_CONJ

    MR_LOCK(&MR_runqueue_lock, "schedule_context");
    ctxt->MR_ctxt_next = NULL;
    if (MR_runqueue_tail) {
        MR_runqueue_tail->MR_ctxt_next = ctxt;
        MR_runqueue_tail = ctxt;
    } else {
        MR_runqueue_head = ctxt;
        MR_runqueue_tail = ctxt;
    }
    MR_UNLOCK(&MR_runqueue_lock, "schedule_context");

#ifdef MR_LL_PARALLEL_CONJ
    if (engine_required) {
        // The engine is only runnable on a single context, that context was
        // busy earlier and couldn't be handed the engine. If that context
        // is now idle or stealing it may have already checked the runqueue
        // (where we just put the context). Therefore we re-attempt to
        // notify the engine to ensure that it re-checks the runqueue.
        //
        // This is only a problem when only a single engine can execute a
        // context. In any other case the current engine will eventually check
        // the runqueue.
        //
        // The updates to the run queue are guaranteed by the compiler and
        // the processor to be visible before the runqueue is unlocked.
        // And the engine's update of its state from working->idle will be
        // available before it can lock the runqueue. Therefore, if the
        // engine is working we do not message it because it will check the
        // runqueue anyway.

        MR_Unsigned state;

        state = esync->d.es_state;
        while (state & (ENGINE_STATE_SLEEPING | ENGINE_STATE_IDLE |
                ENGINE_STATE_STEALING)) {
            if (state == ENGINE_STATE_SLEEPING) {
                if (try_wake_engine(engine_id,
                        MR_ENGINE_ACTION_CONTEXT_ADVICE, NULL)) {
                    break;
                }
            } else if ((state == ENGINE_STATE_IDLE)
                    || (state == ENGINE_STATE_STEALING)) {
                if (try_notify_engine(engine_id,
                        MR_ENGINE_ACTION_CONTEXT_ADVICE, NULL, state)) {
                    break;
                }
            }
            MR_sched_yield();
            state = esync->d.es_state;
        }
    }
#endif // MR_LL_PARALLEL_CONJ
}

#ifdef MR_LL_PARALLEL_CONJ
void
MR_verify_initial_engine_sleep_sync(MR_EngineId id)
{
    engine_sleep_sync   *esync = get_engine_sleep_sync(id);

    assert(esync->d.es_state == ENGINE_STATE_WORKING);
    assert(esync->d.es_action == MR_ENGINE_ACTION_NONE);
}

void
MR_verify_final_engine_sleep_sync(MR_EngineId id, MR_EngineType engine_type)
{
    engine_sleep_sync   *esync = get_engine_sleep_sync(id);

    // Shared engines are shut down by notification.
    // Exclusive engines are shut down at the end of the Mercury thread.

    if (engine_type == MR_ENGINE_TYPE_SHARED) {
        assert(esync->d.es_state == ENGINE_STATE_NOTIFIED);
    } else {
        assert(esync->d.es_state == ENGINE_STATE_WORKING);
    }
    assert(esync->d.es_action == MR_ENGINE_ACTION_NONE);
}

// Try to wake a work-stealing engine, starting at the preferred engine.

MR_bool
MR_try_wake_ws_engine(MR_EngineId preferred_engine, int action,
    union MR_engine_wake_action_data *action_data, MR_EngineId *target_eng)
{
    MR_EngineId current_engine;
    int i = 0;
    int state;
    MR_bool result;
    MR_Unsigned valid_states;

    // Set the valid set of states that can be notified for this action.

    switch (action) {
        case MR_ENGINE_ACTION_SHUTDOWN:
        case MR_ENGINE_ACTION_CONTEXT_ADVICE:
            valid_states = ENGINE_STATE_IDLE | ENGINE_STATE_STEALING |
                ENGINE_STATE_SLEEPING;
            break;
        case MR_ENGINE_ACTION_WORKSTEAL_ADVICE:
            valid_states = ENGINE_STATE_STEALING | ENGINE_STATE_SLEEPING;
            break;
        case MR_ENGINE_ACTION_CONTEXT:
            valid_states = ENGINE_STATE_SLEEPING;
            break;
        default:
            abort();
    }

    // Right now this algorithm is naive, it searches from the preferred engine
    // around the loop until it finds an engine.

    for (i = 0; i < MR_num_ws_engines; i++) {
        current_engine = (i + preferred_engine) % MR_num_ws_engines;
        if (current_engine == MR_ENGINE(MR_eng_id)) {
            // Don't post superfluous events to ourself.

            continue;
        }
        state = get_engine_sleep_sync(current_engine)->d.es_state;
        if (state & valid_states) {
            switch (state) {
                case ENGINE_STATE_SLEEPING:
                    result = try_wake_engine(current_engine,
                        action, action_data);
                    if (result) {
                        goto success;
                    }
                    break;

                case ENGINE_STATE_IDLE:
                case ENGINE_STATE_STEALING:
                    result = try_notify_engine(current_engine, action,
                        action_data, state);
                    if (result) {
                        goto success;
                    }
                    break;
            }
        }
    }

    return MR_FALSE;

success:
    if (target_eng) {
        *target_eng = current_engine;
    }
    return MR_TRUE;
}

static MR_bool
try_wake_engine(MR_EngineId engine_id, int action,
    union MR_engine_wake_action_data *action_data)
{
    MR_bool success = MR_FALSE;
    engine_sleep_sync *esync = get_engine_sleep_sync(engine_id);

#ifdef MR_DEBUG_THREADS
    if (MR_debug_threads) {
        fprintf(stderr,
            "%ld Trying to wake up engine %d, action %d\n",
            MR_SELF_THREAD_ID, engine_id, action);
    }
#endif

    // Our caller made an initial check of the engine's state. But we check
    // it again after taking the lock.

    MR_LOCK(&(esync->d.es_wake_lock), "try_wake_engine, wake_lock");
    if (esync->d.es_state == ENGINE_STATE_SLEEPING) {
        if (engine_id < MR_num_ws_engines) {
            MR_atomic_dec_int(&MR_num_idle_ws_engines);
#ifdef MR_DEBUG_THREADS
            if (MR_debug_threads) {
                fprintf(stderr, "%ld Decrement MR_num_idle_ws_engines %"
                    MR_INTEGER_LENGTH_MODIFIER "d\n",
                    MR_SELF_THREAD_ID, MR_num_idle_ws_engines);
            }
#endif
        }

        // We now KNOW that the engine is in one of the correct states.
        //
        // We tell the engine what to do, and tell others that we have woken it
        // before actually waking it.

        esync->d.es_action = action;
        if (action_data) {
            esync->d.es_action_data = *action_data;
        }
        esync->d.es_state = ENGINE_STATE_NOTIFIED;
        MR_CPU_SFENCE;
        MR_SEM_POST(&(esync->d.es_sleep_semaphore),
            "try_wake_engine sleep_sem");
        success = MR_TRUE;
    }
    MR_UNLOCK(&(esync->d.es_wake_lock), "try_wake_engine wake_lock");

#ifdef MR_DEBUG_THREADS
    if (MR_debug_threads) {
        fprintf(stderr,
            "%ld Wake result %d\n",
            MR_SELF_THREAD_ID, success);
    }
#endif

    return success;
}

MR_bool
try_notify_engine(MR_EngineId engine_id, int action,
    union MR_engine_wake_action_data *action_data, MR_Unsigned engine_state)
{
    engine_sleep_sync *esync = get_engine_sleep_sync(engine_id);

#ifdef MR_DEBUG_THREADS
    if (MR_debug_threads) {
        fprintf(stderr,
            "%ld Trying to notify engine %d, action %d, state %d\n",
            MR_SELF_THREAD_ID, engine_id, action, engine_state);
    }
#endif

    // As in try_wake_engine, we expect our caller to read the current state
    // of the engine. But in this case it should also provide the state of
    // the engine so we can use it for the CAS below.

    if (MR_compare_and_swap_uint(&(esync->d.es_state), engine_state,
            ENGINE_STATE_BUSY)) {
        // Tell the engine what to do.
        esync->d.es_action = action;
        if (action_data) {
            esync->d.es_action_data = *action_data;
        }
        if (engine_state == ENGINE_STATE_STEALING) {
            // The engine was idle if it was in the stealing state.
            // It is not idle anymore so fixup the count.

            MR_assert(engine_id < MR_num_ws_engines);
            MR_atomic_dec_int(&MR_num_idle_ws_engines);
#ifdef MR_DEBUG_THREADS
            if (MR_debug_threads) {
                fprintf(stderr, "%ld Decrement MR_num_idle_ws_engines %"
                    MR_INTEGER_LENGTH_MODIFIER "d\n",
                    MR_SELF_THREAD_ID, MR_num_idle_ws_engines);
            }
#endif
        }

        // Write the data before we move into the working state.
        MR_CPU_SFENCE;
        esync->d.es_state = ENGINE_STATE_NOTIFIED;

        // We don't adjust the idle engine counter, the engine itself does
        // that, especially if this message is dropable.

#ifdef MR_DEBUG_THREADS
        if (MR_debug_threads) {
            fprintf(stderr, "%ld Notified engine\n", MR_SELF_THREAD_ID);
        }
#endif
        return MR_TRUE;
    } else {
#ifdef MR_DEBUG_THREADS
        if (MR_debug_threads) {
            fprintf(stderr, "%ld Could not notify engine (lost CAS race)\n",
                MR_SELF_THREAD_ID);
        }
#endif
        return MR_FALSE;
    }
}

void
MR_shutdown_ws_engines(void)
{
    int i;
    MR_bool result;

    for (i = 0; i < MR_num_ws_engines; i++) {
        if (i == MR_ENGINE(MR_eng_id)) {
            continue;
        }

        while (1) {
            engine_sleep_sync *esync = get_engine_sleep_sync(i);
            MR_Unsigned state = esync->d.es_state;

            // We can only notify the engine if it is in the idle or
            // sleeping states. Only in these states can we be sure that
            // the engine will observe our message. If it is sleeping then
            // the semaphore is used for synchronization. If it is idle, it
            // must do a CAS before it sleeps (and it cannot work because
            // there will be no available work if the system is shutting down.

            if (state == ENGINE_STATE_IDLE) {
                if (try_notify_engine(i, MR_ENGINE_ACTION_SHUTDOWN, NULL,
                        state)) {
                    break;
                }
            } else if (state == ENGINE_STATE_SLEEPING) {
                result = try_wake_engine(i, MR_ENGINE_ACTION_SHUTDOWN,
                    NULL);
                break;
            }
            // An engine may still appear to be working if this processor
            // has not yet seen the other processor write to its state
            // field yet. If this happens, wait until it does.
            //
            // Yield to the OS because we cannot know how long we may have
            // to wait.

            MR_sched_yield();
        }
    }

    for (i = 0; i < (MR_num_ws_engines - 1); i++) {
        int err;

        do {
            err = MR_SEM_WAIT(&shutdown_ws_semaphore, "MR_shutdown_ws_engines");
        } while (err == -1 && MR_SEM_IS_EINTR(errno));
    }
}

#endif // MR_LL_PARALLEL_CONJ

#ifndef MR_HIGHLEVEL_CODE

////////////////////////////////////////////////////////////////////////////
//
// Parallel runtime idle loop.
//
// This also contains code to run the next runnable context for non-parallel
// low level C grades.
//

#ifdef MR_THREAD_SAFE
static void
action_shutdown_ws_engine(void);

static MR_Code*
action_worksteal(MR_EngineId victim_engine_id);

// This always returns a valid code address.

static MR_Code*
action_context(MR_Context *context);
#endif // MR_THREAD_SAFE

// The run queue used to include timing code. It has been removed and may be
// added in the future.

MR_define_extern_entry(MR_do_idle);

#ifdef MR_THREAD_SAFE
MR_define_extern_entry(MR_do_idle_worksteal);
MR_define_extern_entry(MR_do_sleep);

static MR_Code*
do_get_context(void);

static MR_Code*
do_local_spark(MR_Code *join_label);

static MR_Code*
do_work_steal(void);

static void
save_dirty_context(MR_Code *join_label);

// Prepare the engine to execute a spark. Only call this if either:
// 1) the engine does not have a context.
// 2) the engine's context is free for use with the spark.

static void
prepare_engine_for_spark(volatile MR_Spark *spark);

// Prepare the engine to execute a context. This loads the context into the
// engine after discarding any existing context. All the caller need do is
// jump to the resume/start point.

static void
prepare_engine_for_context(MR_Context *context);
#endif // MR_THREAD_SAFE

MR_BEGIN_MODULE(scheduler_module_idle)
    MR_init_entry_an(MR_do_idle);
MR_BEGIN_CODE
MR_define_entry(MR_do_idle);
{
#ifdef MR_THREAD_SAFE
    MR_Code             *jump_target;
    MR_EngineId         engine_id = MR_ENGINE(MR_eng_id);
    engine_sleep_sync   *esync = get_engine_sleep_sync(engine_id);

    // We can set the idle status without a compare and swap. There are no
    // notifications that could have arrived while the engine was working,
    // and that cannot safely be ignored. This is a deliberate design
    // choice, to avoid a compare and swap in the common state transitions
    // between idle and working, and vice versa.
    //
    // We must advertise that we are in the idle state now (even if we are
    // about to find work) before checking the context run queue.
    // schedule_context() requires this so that it can reliably deliver a
    // context advice message.

    esync->d.es_state = ENGINE_STATE_IDLE;

    // Try to get a context.

    jump_target = do_get_context();
    if (jump_target != NULL) {
        esync->d.es_state = ENGINE_STATE_WORKING;
        MR_GOTO(jump_target);
    }
    jump_target = do_local_spark(NULL);
    if (jump_target != NULL) {
        esync->d.es_state = ENGINE_STATE_WORKING;
        MR_GOTO(jump_target);
    }

    // TODO: Use multiple entry points into a single MODULE structure.

    if (MR_ENGINE(MR_eng_type) == MR_ENGINE_TYPE_SHARED) {
        MR_GOTO(MR_ENTRY(MR_do_idle_worksteal));
    } else {
        MR_GOTO(MR_ENTRY(MR_do_sleep));
    }

#else // !MR_THREAD_SAFE
    // When an engine becomes idle in a non parallel grade, it simply picks up
    // another context.

    if (MR_runqueue_head == NULL && MR_pending_contexts == NULL) {
        MR_fatal_error("empty runqueue!");
    }

    while (MR_runqueue_head == NULL) {
        MR_check_pending_contexts(MR_TRUE); // block
    }

    MR_ENGINE(MR_eng_this_context) = MR_runqueue_head;
    MR_runqueue_head = MR_runqueue_head->MR_ctxt_next;
    if (MR_runqueue_head == NULL) {
        MR_runqueue_tail = NULL;
    }

    MR_load_context(MR_ENGINE(MR_eng_this_context));
    MR_GOTO(MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume);
#endif // !MR_THREAD_SAFE
}
MR_END_MODULE

#ifdef MR_THREAD_SAFE
MR_BEGIN_MODULE(scheduler_module_idle_worksteal)
    MR_init_entry_an(MR_do_idle_worksteal);
MR_BEGIN_CODE
MR_define_entry(MR_do_idle_worksteal);
{
    MR_Code             *jump_target;
    MR_EngineId         engine_id = MR_ENGINE(MR_eng_id);
    engine_sleep_sync   *esync = get_engine_sleep_sync(engine_id);

    // Only work-stealing engines beyond this point.
    MR_assert(MR_ENGINE(MR_eng_type) == MR_ENGINE_TYPE_SHARED);

    if (!MR_compare_and_swap_uint(&(esync->d.es_state), ENGINE_STATE_IDLE,
            ENGINE_STATE_STEALING)) {
        while (esync->d.es_state == ENGINE_STATE_BUSY) {
            MR_ATOMIC_PAUSE;
        }

        // The compare and swap failed, which means there is a notification.

        switch (esync->d.es_action) {
            case MR_ENGINE_ACTION_SHUTDOWN:
                action_shutdown_ws_engine();

            case MR_ENGINE_ACTION_CONTEXT_ADVICE:
                MR_GOTO(MR_ENTRY(MR_do_idle));

            case MR_ENGINE_ACTION_WORKSTEAL_ADVICE:
                jump_target = action_worksteal(
                    esync->d.es_action_data.MR_ewa_worksteal_engine);
                if (jump_target != NULL) {
                    MR_GOTO(jump_target);
                } else {
                    MR_GOTO(MR_ENTRY(MR_do_idle));
                }

            case MR_ENGINE_ACTION_CONTEXT:
            case MR_ENGINE_ACTION_NONE:
            default:
                abort();
                break;
        }
        // We attempted to act on the notification but we lost a race above
        // when attempting to worksteal. Now we continue into the
        // workstealing state.

        esync->d.es_state = ENGINE_STATE_STEALING;
    }

    // The compare and swap must be visible before the increment.

    MR_CPU_SFENCE;
    MR_atomic_inc_int(&MR_num_idle_ws_engines);
#ifdef MR_DEBUG_THREADS
    if (MR_debug_threads) {
        fprintf(stderr, "%ld Increment MR_num_idle_ws_engines %d\n",
            MR_SELF_THREAD_ID, MR_num_idle_ws_engines);
    }
#endif

    jump_target = do_work_steal();
    if (jump_target != NULL) {
        MR_atomic_dec_int(&MR_num_idle_ws_engines);
#ifdef MR_DEBUG_THREADS
        if (MR_debug_threads) {
            fprintf(stderr, "%ld Decrement MR_num_idle_ws_engines %d\n",
                MR_SELF_THREAD_ID, MR_num_idle_ws_engines);
        }
#endif
        MR_CPU_SFENCE;
        esync->d.es_state = ENGINE_STATE_WORKING;
        MR_GOTO(jump_target);
    }

    MR_GOTO(MR_ENTRY(MR_do_sleep));
}
MR_END_MODULE
#endif // MR_THREAD_SAFE

#ifdef MR_THREAD_SAFE
// Put the engine to sleep since there's no work to do.
//
// This call does not return.
//
// REQUIREMENT: Only call this with either no context or a clean context.
// REQUIREMENT: This must be called from the same C and Mercury stack depths as
//              the call into the idle loop.

MR_BEGIN_MODULE(scheduler_module_idle_sleep)
    MR_init_entry_an(MR_do_sleep);
MR_BEGIN_CODE
MR_define_entry(MR_do_sleep);
{
    MR_EngineId         engine_id = MR_ENGINE(MR_eng_id);
    engine_sleep_sync   *esync = get_engine_sleep_sync(engine_id);

    MR_Unsigned     in_state;
    unsigned        action;
    int             result;
    MR_Code         *jump_target;
    MR_Unsigned     state;
#ifdef MR_WORKSTEAL_POLLING
    struct timespec ts;
    struct timeval  tv;
#endif

    // Shared engines and exclusive engines enter via different states.

    if (MR_ENGINE(MR_eng_type) == MR_ENGINE_TYPE_SHARED) {
        in_state = ENGINE_STATE_STEALING;
    } else {
        in_state = ENGINE_STATE_IDLE;
    }
    if (MR_compare_and_swap_uint(&(esync->d.es_state), in_state,
            ENGINE_STATE_SLEEPING)) {
        // We have permission to sleep, and must commit to sleeping.

#ifdef MR_DEBUG_THREADS
        if (MR_debug_threads) {
            fprintf(stderr, "%ld Engine %d going to sleep\n",
                MR_SELF_THREAD_ID, MR_ENGINE(MR_eng_id));
        }
#endif

#ifdef MR_THREADSCOPE
        MR_threadscope_post_engine_sleeping();
#endif

retry_sleep:
#if defined(MR_HAVE_GETTIMEOFDAY) && defined(MR_HAVE_SEMAPHORE_H)

#ifdef MR_WORKSTEAL_POLLING
        gettimeofday(&tv, NULL);
        // Sleep for 2ms.
        tv.tv_usec += 2000;

        if (tv.tv_usec >= 1000000) {
            tv.tv_usec = tv.tv_sec % 1000000;
            tv.tv_sec += 1;
        }
        ts.tv_sec = tv.tv_sec;
        ts.tv_nsec = tv.tv_usec * 1000;
        result = MR_SEM_TIMED_WAIT(&(esync->d.es_sleep_semaphore), &ts,
            "MR_do_sleep");
#else
        result = MR_SEM_WAIT(&(esync->d.es_sleep_semaphore), "MR_do_sleep");
#endif

        if (result != 0) {
            // Sem_wait reported an error.

            switch (errno) {
                case EINTR:
                    // An interrupt woke the engine, go back to sleep.

#ifdef MR_DEBUG_THREADS
                    if (MR_debug_threads) {
                        fprintf(stderr, "%ld Engine %d sleep interrupted\n",
                            MR_SELF_THREAD_ID, MR_ENGINE(MR_eng_id));
                    }
#endif
                    goto retry_sleep;
                case ETIMEDOUT:
                    // A wait timed out, check for any sparks.

#ifdef MR_DEBUG_THREADS
                    if (MR_debug_threads) {
                        fprintf(stderr, "%ld Engine %d sleep timed out\n",
                            MR_SELF_THREAD_ID, MR_ENGINE(MR_eng_id));
                    }
#endif
                    MR_LOCK(&(esync->d.es_wake_lock), "do_sleep, wake_lock");
                    state = esync->d.es_state;
                    if (state == ENGINE_STATE_NOTIFIED) {
                        // A notification occurred after the timeout but
                        // before we took the lock above.
                        //
                        // So set a null jump target and do not get a spark.
                        // Then we will execute the goto below and wait on
                        // the semaphore once more, which will instantly
                        // succeed and proceed to interpret the notification
                        // below.

                        jump_target = NULL;
                    } else {
                        if (MR_ENGINE(MR_eng_type) == MR_ENGINE_TYPE_SHARED) {
                            jump_target = do_work_steal();
                        } else {
                            jump_target = NULL;
                        }
                        if (jump_target != NULL) {
                            MR_atomic_dec_int(&MR_num_idle_ws_engines);
#ifdef MR_DEBUG_THREADS
                            if (MR_debug_threads) {
                                fprintf(stderr,
                                    "%ld Decrement MR_num_idle_ws_engines %d\n",
                                    MR_SELF_THREAD_ID, MR_num_idle_ws_engines);
                            }
#endif
                            MR_CPU_SFENCE;
                            esync->d.es_state = ENGINE_STATE_WORKING;
                        }
                    }
                    MR_UNLOCK(&(esync->d.es_wake_lock), "do_sleep, wake_lock");
                    if (jump_target != NULL) {
                        MR_GOTO(jump_target);
                    }
                    goto retry_sleep;
                default:
                    MR_perror("sem_timedwait");
                    abort();
            }
        } // if sem_wait raised an error
#else
        MR_fatal_error(
            "low-level parallel grades need gettimeofday() and "
            "sem_timedwait()\n");
#endif
    } else {
        // The compare and swap failed, retrieve the new state.
        // Wait until the engine state is no-longer busy, indicating that
        // the action information is available.

        do {
            state = esync->d.es_state;
            MR_ATOMIC_PAUSE;
        } while (state == ENGINE_STATE_BUSY);
        // Read state above before reading action below.
        MR_CPU_LFENCE;
    }
    // Either we slept and were notified, or were notified before we slept.
    // Either way, check why we were notified.

    MR_assert(state == ENGINE_STATE_NOTIFIED);

    action = esync->d.es_action;
    esync->d.es_action = MR_ENGINE_ACTION_NONE;

    switch (action) {
        case MR_ENGINE_ACTION_SHUTDOWN:
            if (MR_ENGINE(MR_eng_type) == MR_ENGINE_TYPE_SHARED) {
                action_shutdown_ws_engine();
            } else {
                fprintf(stderr, "Mercury runtime: Exclusive engine %d "
                    "received shutdown action\n", MR_ENGINE(MR_eng_id));
            }
            break;

        case MR_ENGINE_ACTION_WORKSTEAL_ADVICE:
            if (MR_ENGINE(MR_eng_type) == MR_ENGINE_TYPE_SHARED) {
                jump_target = action_worksteal(
                    esync->d.es_action_data.MR_ewa_worksteal_engine);
                if (jump_target != NULL) {
                    MR_GOTO(jump_target);
                }
            }
            MR_GOTO(MR_ENTRY(MR_do_idle));

        case MR_ENGINE_ACTION_CONTEXT:
            MR_GOTO(action_context(esync->d.es_action_data.MR_ewa_context));

        case MR_ENGINE_ACTION_CONTEXT_ADVICE:
            MR_GOTO(MR_ENTRY(MR_do_idle));

        case MR_ENGINE_ACTION_NONE:
        default:
            fprintf(stderr,
                "Mercury runtime: Engine %d woken with no action\n",
                MR_ENGINE(MR_eng_id));
            break;
    } // Switch on action

    // Each valid case ends with a GOTO, so execution cannot reach here.
    abort();
}
MR_END_MODULE

static void
action_shutdown_ws_engine(void)
{
    MR_EngineId engine_id = MR_ENGINE(MR_eng_id);

#ifdef MR_DEBUG_THREADS
    if (MR_debug_threads) {
        fprintf(stderr, "%ld Engine %d doing ACTION_SHUTDOWN\n",
            MR_SELF_THREAD_ID, engine_id);
    }
#endif
    // The primordial thread has the responsibility of cleaning up
    // the Mercury runtime. It cannot exit by this route.
    // Exclusive engines also do not exit by this route.

    assert(engine_id != 0);
    assert(MR_ENGINE(MR_eng_type) == MR_ENGINE_TYPE_SHARED);
    MR_finalize_thread_engine();
    MR_SEM_POST(&shutdown_ws_semaphore, "MR_do_sleep shutdown_sem");
    pthread_exit(0);
}

static MR_Code*
action_worksteal(MR_EngineId victim_engine_id)
{
    MR_SparkDeque *victim;
    int steal_result;
    MR_Spark spark;
    engine_sleep_sync *esync;

    MR_assert(MR_ENGINE(MR_eng_type) == MR_ENGINE_TYPE_SHARED);
    esync = get_engine_sleep_sync(MR_ENGINE(MR_eng_id));

#ifdef MR_DEBUG_THREADS
    if (MR_debug_threads) {
        fprintf(stderr, "%ld Engine %d workstealing, victim %d\n",
            MR_SELF_THREAD_ID, MR_ENGINE(MR_eng_id), victim_engine_id);
    }
#endif

    victim = MR_spark_deques[victim_engine_id];
    MR_assert(victim != NULL);
    steal_result = MR_wsdeque_steal_top(victim, &spark);
    while (steal_result == -1) {
        // Collision, relax the CPU and try again.
        MR_ATOMIC_PAUSE;
        steal_result = MR_wsdeque_steal_top(victim, &spark);
    }

    if (steal_result == 1) {
        esync->d.es_state = ENGINE_STATE_WORKING;

        // Steal from this engine next time, it may have more work.

        MR_ENGINE(MR_eng_victim_counter) = victim_engine_id;
#ifdef MR_THREADSCOPE
        MR_threadscope_post_steal_spark(spark.MR_spark_id);
#endif
#ifdef MR_DEBUG_THREADS
        if (MR_debug_threads) {
            fprintf(stderr, "%ld Engine %d executing spark\n",
                MR_SELF_THREAD_ID, MR_ENGINE(MR_eng_id));
        }
#endif
        prepare_engine_for_spark(&spark);
        return spark.MR_spark_resume;
    } else {
        // The deque is empty, next time try a different deque.
        // (+1 will do).

        MR_ENGINE(MR_eng_victim_counter) = victim_engine_id + 1;
        return NULL;
    }
}

static MR_Code*
action_context(MR_Context *context)
{
    MR_Code *resume_point;
    engine_sleep_sync *esync;

    esync = get_engine_sleep_sync(MR_ENGINE(MR_eng_id));
    esync->d.es_state = ENGINE_STATE_WORKING;
    prepare_engine_for_context(context);

#ifdef MR_DEBUG_STACK_SEGMENTS
    MR_debug_log_message("resuming old context: %p",
        context);
#endif
#ifdef MR_DEBUG_THREADS
    if (MR_debug_threads) {
        fprintf(stderr,
            "%ld Engine %d running context %p from action\n",
            MR_SELF_THREAD_ID, MR_ENGINE(MR_eng_id), context);
    }
#endif

    resume_point = (MR_Code*)(context->MR_ctxt_resume);
    context->MR_ctxt_resume = NULL;

    return resume_point;
}
#endif

#ifdef MR_THREAD_SAFE

static MR_Code*
do_get_context(void)
{
    MR_Context *ready_context;
    MR_Code *resume_point;

    // Look for a runnable context and execute it. If there was no runnable
    // context, then proceed to MR_do_runnext_local.

    #ifdef MR_THREADSCOPE
    MR_threadscope_post_looking_for_global_context();
    #endif

    // We can only read the runqueue head after the store to engine's state
    // has finished.

    MR_CPU_MFENCE;

    if (MR_runqueue_head != NULL) {
        MR_LOCK(&MR_runqueue_lock, "do_get_context (i)");
        ready_context = MR_find_ready_context();
        MR_UNLOCK(&MR_runqueue_lock, "do_get_context (ii)");

        if (ready_context != NULL) {
            prepare_engine_for_context(ready_context);

            #ifdef MR_DEBUG_STACK_SEGMENTS
            MR_debug_log_message("resuming old context: %p", ready_context);
            #endif
            #ifdef MR_DEBUG_THREADS
            if (MR_debug_threads) {
                fprintf(stderr, "%ld Engine %d resuming context %p\n",
                    MR_SELF_THREAD_ID, MR_ENGINE(MR_eng_id), ready_context);
            }
            #endif

            resume_point = (MR_Code*)(ready_context->MR_ctxt_resume);
            ready_context->MR_ctxt_resume = NULL;

            return resume_point;
        }
    }

    return NULL;
}

static void
prepare_engine_for_context(MR_Context *context)
{
    // Discard whatever unused context we may have, and switch to the new one.

    if (MR_ENGINE(MR_eng_this_context) != NULL) {
    #ifdef MR_DEBUG_STACK_SEGMENTS
        MR_debug_log_message("destroying old context %p",
            MR_ENGINE(MR_eng_this_context));
    #endif
        // Saving the context is important. Details such as the current
        // stack pointer must be reset before the context is released.

        MR_save_context(MR_ENGINE(MR_eng_this_context));
        MR_release_context(MR_ENGINE(MR_eng_this_context));
    }

    MR_assert(context->MR_ctxt_exclusive_engine == MR_ENGINE_ID_NONE
        || context->MR_ctxt_exclusive_engine == MR_ENGINE(MR_eng_id));

    MR_ENGINE(MR_eng_this_context) = context;
    MR_load_context(context);
#ifdef MR_THREADSCOPE
    MR_threadscope_post_run_context();
#endif
}

static void
prepare_engine_for_spark(volatile MR_Spark *spark)
{
    MR_Context *this_context = MR_ENGINE(MR_eng_this_context);

    if (this_context == NULL) {
        // Get a new context

#ifdef MR_DEBUG_CONTEXT_CREATION_SPEED
        MR_debug_log_message("Need a new context.");
#endif
        MR_ENGINE(MR_eng_this_context) = MR_create_context("from spark",
            MR_CONTEXT_SIZE_FOR_SPARK, NULL);
#ifdef MR_THREADSCOPE
        MR_threadscope_post_create_context_for_spark(
            MR_ENGINE(MR_eng_this_context));
#endif
#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
        if (MR_profile_parallel_execution) {
            MR_atomic_inc_int(
                &MR_profile_parallel_contexts_created_for_sparks);
        }
#endif

        MR_load_context(MR_ENGINE(MR_eng_this_context));
#ifdef MR_DEBUG_STACK_SEGMENTS
        MR_debug_log_message("created new context for spark: %p",
            MR_ENGINE(MR_eng_this_context));
#endif
    } else {
#ifdef MR_THREADSCOPE
        MR_Unsigned old_id;

        old_id = MR_ENGINE(MR_eng_this_context)->MR_ctxt_num_id;
        MR_ENGINE(MR_eng_this_context)->MR_ctxt_num_id = allocate_context_id();
        MR_threadscope_post_reuse_context(MR_ENGINE(MR_eng_this_context),
            old_id);
#endif
    }
#ifdef MR_THREADSCOPE
    MR_threadscope_post_run_context();
#endif

    // At this point we have a context, either a dirty context that is
    // compatible, or a clean one.

    MR_parent_sp = spark->MR_spark_sync_term->MR_st_parent_sp;
    MR_SET_THREAD_LOCAL_MUTABLES(spark->MR_spark_thread_local_mutables);

    MR_assert(MR_parent_sp);
    MR_assert(spark->MR_spark_sync_term->MR_st_count > 0);
}

static MR_Code*
do_local_spark(MR_Code *join_label)
{
    volatile MR_Spark *spark;
    MR_Context        *this_context = MR_ENGINE(MR_eng_this_context);

#ifdef MR_THREADSCOPE
    MR_threadscope_post_looking_for_local_spark();
#endif

    spark = MR_wsdeque_pop_bottom(MR_ENGINE(MR_eng_spark_deque));
    if (NULL == spark) {
        return NULL;
    }

    // The current context may be dirty and incompatible with this spark, if
    // so we put the spark back onto the deque. This test is only applicable
    // when running a local spark.
    //
    // Our caller will then save the context and look for a different
    // context to run, if it cannot find a context then it will call this
    // function again to run the incompatible spark, allocating a new context.

    if ((this_context != NULL) &&
        (join_label != NULL) &&
        (spark->MR_spark_sync_term->MR_st_orig_context != this_context))
    {
        // The cast discards the volatile qualifier, which is okay.
        MR_wsdeque_putback_bottom(MR_ENGINE(MR_eng_spark_deque),
            (MR_Spark*) spark);
        return NULL;
    }

#ifdef MR_THREADSCOPE
    MR_threadscope_post_run_spark(spark->MR_spark_id);
#endif

    prepare_engine_for_spark(spark);

    return spark->MR_spark_resume;
}

static MR_Code*
do_work_steal(void)
{
    MR_Spark spark;

    MR_assert(MR_ENGINE(MR_eng_type) == MR_ENGINE_TYPE_SHARED);

    #ifdef MR_THREADSCOPE
    MR_threadscope_post_work_stealing();
    #endif

    // A context may be created to execute a spark, so only attempt to
    // steal sparks if doing so would not exceed the limit of outstanding
    // contexts.
    //
    // This condition is simply a crude way to limit memory consumption by
    // parallel execution. It is currently affected by contexts created for
    // explicit concurrency, which may be surprising.
    // XXX why the non-strict inequality?

    if ((MR_ENGINE(MR_eng_this_context) != NULL) ||
        (MR_num_outstanding_contexts <= MR_max_outstanding_contexts)) {
        // Attempt to steal a spark
        if (MR_attempt_steal_spark(&spark)) {
#ifdef MR_THREADSCOPE
            MR_threadscope_post_steal_spark(spark.MR_spark_id);
#endif
#ifdef MR_DEBUG_THREADS
            if (MR_debug_threads) {
                fprintf(stderr, "%ld Engine %d executing spark\n",
                    MR_SELF_THREAD_ID, MR_ENGINE(MR_eng_id));
            }
#endif

            prepare_engine_for_spark(&spark);
            return spark.MR_spark_resume;
        }
    }

    return NULL;
}

static void
save_dirty_context(MR_Code *join_label)
{
    MR_Context *this_context = MR_ENGINE(MR_eng_this_context);

#ifdef MR_THREADSCOPE
    MR_threadscope_post_stop_context(MR_TS_STOP_REASON_BLOCKED);
#endif
    this_context->MR_ctxt_resume_engine = MR_ENGINE(MR_eng_id);
    MR_save_context(this_context);
    // Make sure the context gets saved before we set the join label,
    // use a memory barrier.

    MR_CPU_SFENCE;
    this_context->MR_ctxt_resume = join_label;
    MR_ENGINE(MR_eng_this_context) = NULL;
}
#endif // MR_THREAD_SAFE

#endif // !MR_HIGHLEVEL_CODE

#ifdef MR_LL_PARALLEL_CONJ
MR_Code*
MR_do_join_and_continue(MR_SyncTerm *jnc_st, MR_Code *join_label)
{
    MR_bool     jnc_last;
    MR_Context  *this_context = MR_ENGINE(MR_eng_this_context);
    MR_Code     *jump_target;

  #ifdef MR_THREADSCOPE
    MR_threadscope_post_end_par_conjunct((MR_Word*)jnc_st);
  #endif

    // Atomically decrement and fetch the number of conjuncts yet to complete.
    // If we are the last conjunct to complete (the parallel conjunction is
    // finished) then jnc_last will be true.

    // XXX: We should take the current TSC time here and use it to post the
    // various 'context stopped' threadscope events. This profile will be more
    // accurate.

    jnc_last = MR_atomic_dec_and_is_zero_uint(&(jnc_st->MR_st_count));

    if (jnc_last) {
        // All the conjuncts have finished,

        if (this_context != jnc_st->MR_st_orig_context) {
#ifdef MR_THREADSCOPE
            MR_threadscope_post_stop_context(MR_TS_STOP_REASON_FINISHED);
#endif
            // This context didn't originate this parallel conjunction and
            // we are the last branch to finish. The originating context should
            // be suspended waiting for us to finish, we should run it using
            // the current engine.
            //
            // We could be racing with the original context, in which case we
            // have to make sure that it is ready to be scheduled before we
            // schedule it. It will set its resume point to join_label to
            // indicate that it is ready.

            while (jnc_st->MR_st_orig_context->MR_ctxt_resume != join_label) {
                // XXX: Need to configure using sched_yield or spin waiting
                MR_ATOMIC_PAUSE;
            }
            // We must read the resume label before we read the context as
            // the context is written first.

            MR_CPU_LFENCE;
#ifdef MR_THREADSCOPE
            MR_threadscope_post_context_runnable(jnc_st->MR_st_orig_context);
#endif
            prepare_engine_for_context(jnc_st->MR_st_orig_context);

            // This field must be reset to NULL.
            jnc_st->MR_st_orig_context->MR_ctxt_resume = NULL;
        }

        // Continue the parallel conjunction.

        return join_label;
    } else {
        volatile MR_Spark *spark;

#ifdef MR_THREADSCOPE
        MR_threadscope_post_looking_for_local_spark();
#endif
        spark = MR_wsdeque_pop_bottom(MR_ENGINE(MR_eng_spark_deque));
        if (spark != NULL) {
            if ((this_context == jnc_st->MR_st_orig_context) &&
                    (spark->MR_spark_sync_term != jnc_st)) {
                // This spark is not compatible with the context.
                //
                // Change the context.

#ifdef MR_THREADSCOPE
                MR_threadscope_post_stop_context(MR_TS_STOP_REASON_BLOCKED);
#endif
                save_dirty_context(join_label);
                if (MR_runqueue_head != NULL) {
                    // There might be a suspended context. We should try
                    // to execute that.

                    MR_wsdeque_putback_bottom(MR_ENGINE(MR_eng_spark_deque),
                        (MR_Spark*) spark);
                    return MR_ENTRY(MR_do_idle);
                }
            }
            prepare_engine_for_spark(spark);

            return spark->MR_spark_resume;
        } else {
            if (this_context == jnc_st->MR_st_orig_context) {
                // Save our context and then look for work as per normal.

#ifdef MR_THREADSCOPE
                MR_threadscope_post_stop_context(MR_TS_STOP_REASON_BLOCKED);
#endif
                save_dirty_context(join_label);
            } else {
                // This engine and context should look for other work.

#ifdef MR_THREADSCOPE
                MR_threadscope_post_stop_context(MR_TS_STOP_REASON_FINISHED);
#endif
            }
            return MR_ENTRY(MR_do_idle);
        }
    }
}
#endif

#ifdef MR_LL_PARALLEL_CONJ

// Debugging functions for runtime granularity control.

#ifdef MR_DEBUG_RUNTIME_GRANULARITY_CONTROL

#define MR_PAR_COND_STATS_FILENAME "par_cond_stats.log"
static FILE * volatile MR_par_cond_stats_file = NULL;
static volatile MR_Unsigned MR_par_cond_stats_last;
static volatile MR_Unsigned MR_par_cond_stats_last_count;

void MR_record_conditional_parallelism_decision(MR_Unsigned decision)
{
    MR_LOCK(&MR_par_cond_stats_lock,
        "record_conditional_parallelism_decision");

    if (MR_par_cond_stats_file == NULL) {
        MR_par_cond_stats_file = fopen(MR_PAR_COND_STATS_FILENAME, "w");
        MR_par_cond_stats_last = decision;
        MR_par_cond_stats_last_count = 1;
    } else {
        if (decision == MR_par_cond_stats_last) {
            MR_par_cond_stats_last_count++;
        } else {
            fprintf(MR_par_cond_stats_file, "%d %d\n", MR_par_cond_stats_last,
                MR_par_cond_stats_last_count);
            MR_par_cond_stats_last = decision;
            MR_par_cond_stats_last_count = 1;
        }
    }

    MR_UNLOCK(&MR_par_cond_stats_lock,
        "record_conditional_parallelism_decision");
}

void MR_write_out_conditional_parallelism_log(void)
{
    MR_LOCK(&MR_par_cond_stats_lock,
        "write_out_conditional_parallelism_log");

    if (MR_par_cond_stats_file != NULL) {
        fprintf(MR_par_cond_stats_file, "%d %d\n",
            MR_par_cond_stats_last, MR_par_cond_stats_last_count);
        fclose(MR_par_cond_stats_file);
        MR_par_cond_stats_file = NULL;
    }

    MR_UNLOCK(&MR_par_cond_stats_lock,
        "write_out_conditional_parallelism_log");
}

#endif // MR_DEBUG_RUNTIME_GRANULARITY_CONTROL
#endif // MR_LL_PARALLEL_CONJ

// Forward decls to suppress gcc warnings.
void mercury_sys_init_scheduler_wrapper_init(void);
void mercury_sys_init_scheduler_wrapper_init_type_tables(void);
#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_scheduler_wrapper_write_out_proc_statics(FILE *fp);
#endif

void mercury_sys_init_scheduler_wrapper_init(void)
{
#ifndef MR_HIGHLEVEL_CODE
    scheduler_module_idle();
#ifdef MR_THREAD_SAFE
    scheduler_module_idle_worksteal();
    scheduler_module_idle_sleep();
#endif
#endif
}

void mercury_sys_init_scheduler_wrapper_init_type_tables(void)
{
    // No types to register.
}

#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_scheduler_wrapper_write_out_proc_statics(FILE *fp)
{
    // No proc_statics to write out.
}
#endif
