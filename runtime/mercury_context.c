/*
** vim: ts=4 sw=4 expandtab
*/
/*
INIT mercury_sys_init_scheduler_wrapper
ENDINIT
*/
/*
** Copyright (C) 1995-2007, 2009 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_context.c - handles multithreading stuff. */

#include "mercury_imp.h"

#include <stdio.h>
#ifdef MR_THREAD_SAFE
  #include "mercury_thread.h"
  #include "mercury_stm.h"
#endif
#ifdef MR_CAN_DO_PENDING_IO
  #include <sys/types.h>	/* for fd_set */
  #include <sys/time.h>		/* for struct timeval */
  #ifdef MR_HAVE_UNISTD_H
	#include <unistd.h>	/* for select() on OS X */
  #endif	
#endif
#if defined(MR_THREAD_SAFE) && defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT) 
  #include <math.h> /* for sqrt and pow */
#endif

#ifdef MR_HAVE_SCHED_H 
#include <sched.h>
#endif

#include "mercury_memory_handlers.h"
#include "mercury_context.h"
#include "mercury_engine.h"             /* for `MR_memdebug' */
#include "mercury_threadscope.h"        /* for data types and posting events */
#include "mercury_reg_workarounds.h"    /* for `MR_fd*' stuff */

#if defined(MR_THREAD_SAFE) && defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT) 
#define MR_PROFILE_PARALLEL_EXECUTION_FILENAME "parallel_execution_profile.txt"
#endif

/*---------------------------------------------------------------------------*/

/*
** The run queue and spark queue are protected and signalled with the
** same lock and condition variable.
**
** The single sync term lock is used to prevent races in MR_join_and_continue.
** The holder of the sync term lock may acquire the runqueue lock but not vice
** versa.  (We could also have one sync term lock per context, and make
** MR_join_and_continue acquire the sync term lock of the context that
** originated the parallel conjunction, but contention for the single lock
** doesn't seem to be an issue.)
*/
MR_Context              *MR_runqueue_head;
MR_Context              *MR_runqueue_tail;
#ifdef  MR_THREAD_SAFE
  MercuryLock           MR_runqueue_lock;
  MercuryCond           MR_runqueue_cond;
#endif
#ifdef  MR_LL_PARALLEL_CONJ
  MR_SparkDeque         MR_spark_queue;
  MercuryLock           MR_sync_term_lock;
#endif

MR_PendingContext       *MR_pending_contexts;
#ifdef  MR_THREAD_SAFE
  MercuryLock           MR_pending_contexts_lock;
#endif

#if defined(MR_THREAD_SAFE) && defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT) 
MR_bool                 MR_profile_parallel_execution = MR_FALSE;

static MR_Stats         MR_profile_parallel_executed_global_sparks = 
        { 0, 0, 0, 0 };
static MR_Stats         MR_profile_parallel_executed_contexts = { 0, 0, 0, 0 };
static MR_Stats         MR_profile_parallel_executed_nothing = { 0, 0, 0, 0 };
/* This cannot be static as it is used in macros by other modules. */
MR_Stats                MR_profile_parallel_executed_local_sparks = 
        { 0, 0, 0, 0 };
static MR_Integer       MR_profile_parallel_contexts_created_for_sparks = 0;

/*
** We don't access these atomically.  They are protected by the free context
** list lock
*/
static MR_Integer       MR_profile_parallel_small_context_reused = 0;
static MR_Integer       MR_profile_parallel_regular_context_reused = 0;
static MR_Integer       MR_profile_parallel_small_context_kept = 0;
static MR_Integer       MR_profile_parallel_regular_context_kept = 0;
#endif

/*
** Local variables for thread pinning.
*/
#if defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ) && \
    defined(MR_HAVE_SCHED_SETAFFINITY)
static MercuryLock      MR_next_cpu_lock;
MR_bool                 MR_thread_pinning = MR_FALSE;
static MR_Unsigned      MR_next_cpu = 0;
  #ifdef  MR_HAVE_SCHED_GETCPU
static MR_Integer       MR_primordial_thread_cpu = -1;
  #endif
#endif

#if defined(MR_LL_PARALLEL_CONJ) && defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT)
/*
** This is used to give each context its own unique ID.  It is protected by the
** free_context_list_lock.
*/
static MR_ContextId     MR_next_context_id = 0;
#endif

/*
** free_context_list and free_small_context_list are a global linked lists
** of unused context structures, with regular and small stacks respectively.
** If the MR_MemoryZone pointers are not NULL, then they point to allocated
** MR_MemoryZones.
*/
static MR_Context       *free_context_list = NULL;
static MR_Context       *free_small_context_list = NULL;
#ifdef  MR_THREAD_SAFE
  static MercuryLock    free_context_list_lock;
#endif

#ifdef  MR_LL_PARALLEL_CONJ
int volatile MR_num_idle_engines = 0;
int volatile MR_num_outstanding_contexts_and_global_sparks = 0;
MR_Integer volatile MR_num_outstanding_contexts_and_all_sparks = 0;

MR_Unsigned volatile MR_num_exited_engines = 0;

static MercuryLock MR_par_cond_stats_lock;
#endif

/*---------------------------------------------------------------------------*/

static void
MR_init_context_maybe_generator(MR_Context *c, const char *id,
    MR_GeneratorPtr gen);

/*
** Write out the profiling data that we collect during execution.
*/
static void
MR_write_out_profiling_parallel_execution(void);

#if defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ) && \
        defined(MR_HAVE_SCHED_SETAFFINITY) 
static void 
MR_do_pin_thread(int cpu);
#endif

/*---------------------------------------------------------------------------*/

void
MR_init_thread_stuff(void)
{
#ifdef  MR_THREAD_SAFE

    pthread_mutex_init(&MR_runqueue_lock, MR_MUTEX_ATTR);
    pthread_cond_init(&MR_runqueue_cond, MR_COND_ATTR);
    pthread_mutex_init(&free_context_list_lock, MR_MUTEX_ATTR);
    pthread_mutex_init(&MR_global_lock, MR_MUTEX_ATTR);
    pthread_mutex_init(&MR_pending_contexts_lock, MR_MUTEX_ATTR);
  #ifdef MR_LL_PARALLEL_CONJ
    MR_init_wsdeque(&MR_spark_queue, MR_INITIAL_GLOBAL_SPARK_QUEUE_SIZE);
    pthread_mutex_init(&MR_sync_term_lock, MR_MUTEX_ATTR);
    pthread_mutex_init(&MR_next_cpu_lock, MR_MUTEX_ATTR);
    #ifdef MR_DEBUG_RUNTIME_GRANULARITY_CONTROL
    pthread_mutex_init(&MR_par_cond_stats_lock, MR_MUTEX_ATTR);
    #endif
  #endif
    pthread_mutex_init(&MR_STM_lock, MR_MUTEX_ATTR);
  #ifndef MR_THREAD_LOCAL_STORAGE
    MR_KEY_CREATE(&MR_engine_base_key, NULL);
  #endif
    MR_KEY_CREATE(&MR_exception_handler_key, NULL);

  #ifdef MR_HIGHLEVEL_CODE
    MR_KEY_CREATE(&MR_backjump_handler_key, NULL);
    MR_KEY_CREATE(&MR_backjump_next_choice_id_key, (void *)0);
  #endif  

    /* These are actually in mercury_thread.c. */
    pthread_mutex_init(&MR_thread_barrier_lock, MR_MUTEX_ATTR);
  #ifdef MR_HIGHLEVEL_CODE
    pthread_cond_init(&MR_thread_barrier_cond, MR_COND_ATTR);
  #endif

    /* 
     * Configure MR_num_threads if unset to match number of processors on the
     * system, If we do this then we prepare to set processor affinities later
     * on.
     */
    if (MR_num_threads == 0)
    {
#if defined(MR_HAVE_SYSCONF) && defined(_SC_NPROCESSORS_ONLN) 
        long result;

        result = sysconf(_SC_NPROCESSORS_ONLN);
        if (result < 1) {
            /* We couldn't determine the number of processors. */
            MR_num_threads = 1;
        } else {
            MR_num_threads = result;
            /*
            ** On systems that don't support sched_setaffinity we don't try to
            ** automatically enable thread pinning.  This prevents a runtime
            ** warning that could unnecessarily confuse the user.
            **/
#if defined(MR_LL_PARALLEL_CONJ) && defined(MR_HAVE_SCHED_SETAFFINITY)
            /*
            ** Comment this back in to enable thread pinning by default if we
            ** autodetected the correct number of CPUs.
            */
            /* MR_thread_pinning = MR_TRUE; */
#endif
        }
#else /* ! defined(MR_HAVE_SYSCONF) && defined(_SC_NPROCESSORS_ONLN) */
        MR_num_threads = 1;
#endif /* ! defined(MR_HAVE_SYSCONF) && defined(_SC_NPROCESSORS_ONLN) */ 
    }
#endif /* MR_THREAD_SAFE */
}

/*
** Pin the primordial thread first to the CPU it is currently using.  (where
** support is available).
*/
void
MR_pin_primordial_thread(void)
{
#if defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ) && \
        defined(MR_HAVE_SCHED_SETAFFINITY) 
  #ifdef MR_HAVE_SCHED_GETCPU
    /*
    ** We don't need locking to pin the primordial thread as it is called
    ** before any other threads exist.
    */
    if (MR_thread_pinning) {
        MR_primordial_thread_cpu = sched_getcpu();
        if (MR_primordial_thread_cpu == -1) {
            perror("Warning: unable to determine the current CPU for "
             "the primordial thread: ");
        } else {
            MR_do_pin_thread(MR_primordial_thread_cpu);
        }
    }
    if (MR_primordial_thread_cpu == -1) {
        MR_pin_thread();
    }
  #else
    MR_pin_thread();
  #endif
#endif /* defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ) && \
            defined(MR_HAVE_SCHED_SETAFFINITY) */ 
}

void
MR_pin_thread(void)
{
#if defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ) && \
        defined(MR_HAVE_SCHED_SETAFFINITY) 
    MR_LOCK(&MR_next_cpu_lock, "MR_pin_thread");
    if (MR_thread_pinning) {
#if defined(MR_HAVE_SCHED_GETCPU)
        if (MR_next_cpu == MR_primordial_thread_cpu) {
            MR_next_cpu++;
        }
#endif
        MR_do_pin_thread(MR_next_cpu);
        MR_next_cpu++;
    }
    MR_UNLOCK(&MR_next_cpu_lock, "MR_pin_thread");
#endif /* defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ) && \
            defined(MR_HAVE_SCHED_SETAFFINITY) */ 
}

#if defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ) && \
        defined(MR_HAVE_SCHED_SETAFFINITY) 
static void 
MR_do_pin_thread(int cpu) 
{
    cpu_set_t   cpus;

    if (cpu < CPU_SETSIZE) {
        CPU_ZERO(&cpus);
        CPU_SET(cpu, &cpus);
        if (sched_setaffinity(0, sizeof(cpu_set_t), &cpus) == -1) {
            perror("Warning: Couldn't set CPU affinity: ");
            /* 
            ** If this failed once it will probably fail again so we
            ** disable it. 
            */
            MR_thread_pinning = MR_FALSE;
        }
    } else {
        perror("Warning: Couldn't set CPU affinity due to a static "
            "system limit: ");
        MR_thread_pinning = MR_FALSE;
    }
}
#endif /* defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ) && \
            defined(MR_HAVE_SCHED_SETAFFINITY) */ 

void
MR_finalize_thread_stuff(void)
{
#ifdef MR_THREAD_SAFE
    pthread_mutex_destroy(&MR_runqueue_lock);
    pthread_cond_destroy(&MR_runqueue_cond);
    pthread_mutex_destroy(&free_context_list_lock);
#endif

#ifdef  MR_LL_PARALLEL_CONJ
    pthread_mutex_destroy(&MR_sync_term_lock);
#endif

#if defined(MR_THREAD_SAFE) && defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT)
    if (MR_profile_parallel_execution) {
        MR_write_out_profiling_parallel_execution();
    }
#endif
}

#if defined(MR_THREAD_SAFE) && defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT) 
static int
fprint_stats(FILE *stream, const char *message, MR_Stats *stats);

/*
** Write out the profiling data for parallel execution.
**
** This writes out a flat text file which may be parsed by a machine or easily
** read by a human.  There is no advantage in using a binary format since we
** do this once at the end of execution and it's a small amount of data.
** Therefore a text file is used since it has the advantage of being human
** readable.
*/
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
        result = fprintf(file, "CPU cycles per second: %ld\n", MR_cpu_cycles_per_sec);
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

    if (0 != fclose(file)) goto Error;

    return;

    Error: 
        perror(MR_PROFILE_PARALLEL_EXECUTION_FILENAME);
        abort();
}

#define MR_FPRINT_STATS_FORMAT_STRING_FULL \
    ("%s: count %" MR_INTEGER_LENGTH_MODIFIER "u (%" \
    MR_INTEGER_LENGTH_MODIFIER "ur, %" MR_INTEGER_LENGTH_MODIFIER \
    "unr), average %.0f, standard deviation %.0f\n")
#define MR_FPRINT_STATS_FORMAT_STRING_SINGLE \
    ("%s: count %" MR_INTEGER_LENGTH_MODIFIER "u (%" \
    MR_INTEGER_LENGTH_MODIFIER "ur, %" MR_INTEGER_LENGTH_MODIFIER \
    "unr), sample %ul\n")
#define MR_FPRINT_STATS_FORMAT_STRING_NONE \
    ("%s: count %" MR_INTEGER_LENGTH_MODIFIER "u (%" \
    MR_INTEGER_LENGTH_MODIFIER "ur, %" MR_INTEGER_LENGTH_MODIFIER "unr)\n")

static int 
fprint_stats(FILE *stream, const char *message, MR_Stats *stats) {
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

#endif /* defined(MR_THREAD_SAFE) && defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT) */

static void 
MR_init_context_maybe_generator(MR_Context *c, const char *id,
    MR_GeneratorPtr gen)
{
    const char  *detstack_name;
    const char  *nondetstack_name;
    size_t      detstack_size;
    size_t      nondetstack_size;

    c->MR_ctxt_id = id;
    c->MR_ctxt_next = NULL;
    c->MR_ctxt_resume = NULL;
#ifdef  MR_THREAD_SAFE
    c->MR_ctxt_resume_owner_thread = (MercuryThread) NULL;
    c->MR_ctxt_resume_c_depth = 0;
    c->MR_ctxt_saved_owners = NULL;
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
        case MR_CONTEXT_SIZE_SMALL:
            detstack_name  = "small_detstack";
            nondetstack_name = "small_nondetstack";
            detstack_size  = MR_small_detstack_size;
            nondetstack_size = MR_small_nondetstack_size;
            break;
    }

    if (c->MR_ctxt_detstack_zone == NULL) {
        if (gen != NULL) {
            c->MR_ctxt_detstack_zone = MR_create_zone("gen_detstack",
                    0, MR_gen_detstack_size, MR_next_offset(),
                    MR_gen_detstack_zone_size, MR_default_handler);
        } else {
            c->MR_ctxt_detstack_zone = MR_create_zone(detstack_name,
                    0, detstack_size, MR_next_offset(),
                    MR_detstack_zone_size, MR_default_handler);
        }

        if (c->MR_ctxt_prev_detstack_zones != NULL) {
            /*
            ** We may be able to reuse a previously allocated stack, but
            ** a context should be reused only when its stacks are empty.
            */
            MR_fatal_error("MR_init_context_maybe_generator: prev det stack");
        }
    }
    c->MR_ctxt_prev_detstack_zones = NULL;
    c->MR_ctxt_sp = c->MR_ctxt_detstack_zone->MR_zone_min;

    if (c->MR_ctxt_nondetstack_zone == NULL) {
        if (gen != NULL) {
            c->MR_ctxt_nondetstack_zone = MR_create_zone("gen_nondetstack",
                    0, MR_gen_nondetstack_size, MR_next_offset(),
                    MR_gen_nondetstack_zone_size, MR_default_handler);
        } else {
            c->MR_ctxt_nondetstack_zone = MR_create_zone(nondetstack_name,
                    0, nondetstack_size, MR_next_offset(),
                    MR_nondetstack_zone_size, MR_default_handler);
        }

        if (c->MR_ctxt_prev_nondetstack_zones != NULL) {
            /*
            ** We may be able to reuse a previously allocated stack, but
            ** a context should be reused only when its stacks are empty.
            */
            MR_fatal_error(
                "MR_init_context_maybe_generator: prev nondet stack");
        }
    }
    c->MR_ctxt_prev_nondetstack_zones = NULL;
    /*
    ** Note that maxfr and curfr point to the last word in the frame,
    ** not to the first word, so we need to add the size of the frame,
    ** minus one word, to the base address to get the maxfr/curfr pointer
    ** for the first frame on the nondet stack.
    */
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
        c->MR_ctxt_genstack_zone = MR_create_zone("genstack", 0,
            MR_genstack_size, MR_next_offset(),
            MR_genstack_zone_size, MR_default_handler);
    }
    c->MR_ctxt_gen_next = 0;

    if (c->MR_ctxt_cutstack_zone == NULL) {
        c->MR_ctxt_cutstack_zone = MR_create_zone("cutstack", 0,
            MR_cutstack_size, MR_next_offset(),
            MR_cutstack_zone_size, MR_default_handler);
    }
    c->MR_ctxt_cut_next = 0;

    if (c->MR_ctxt_pnegstack_zone == NULL) {
        c->MR_ctxt_pnegstack_zone = MR_create_zone("pnegstack", 0,
            MR_pnegstack_size, MR_next_offset(),
            MR_pnegstack_zone_size, MR_default_handler);
    }
    c->MR_ctxt_pneg_next = 0;
  #endif /* MR_USE_MINIMAL_MODEL_STACK_COPY */

  #ifdef MR_USE_MINIMAL_MODEL_OWN_STACKS
    c->MR_ctxt_owner_generator = gen;
  #endif /* MR_USE_MINIMAL_MODEL_OWN_STACKS */

  #ifdef MR_LL_PARALLEL_CONJ
    c->MR_ctxt_parent_sp = NULL;
    MR_init_wsdeque(&c->MR_ctxt_spark_deque,
        MR_INITIAL_LOCAL_SPARK_DEQUE_SIZE);
  #endif /* MR_LL_PARALLEL_CONJ */

#endif /* !MR_HIGHLEVEL_CODE */

#ifdef MR_USE_TRAIL
    if (gen != NULL) {
        MR_fatal_error("MR_init_context_maybe_generator: generator and trail");
    }

    if (c->MR_ctxt_trail_zone == NULL) {
        c->MR_ctxt_trail_zone = MR_create_zone("trail", 0,
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

    /* The caller is responsible for initialising this field. */
    c->MR_ctxt_thread_local_mutables = NULL;
}

MR_Context *
MR_create_context(const char *id, MR_ContextSize ctxt_size, MR_Generator *gen)
{
    MR_Context  *c;
#if defined(MR_LL_PARALLEL_CONJ) && defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT)
    MR_Unsigned context_id;
#endif

    MR_LOCK(&free_context_list_lock, "create_context");

#ifdef MR_LL_PARALLEL_CONJ
    MR_num_outstanding_contexts_and_global_sparks++;
    MR_atomic_inc_int(&MR_num_outstanding_contexts_and_all_sparks);
#endif

    /*
    ** Regular contexts have stacks at least as big as small contexts,
    ** so we can return a regular context in place of a small context
    ** if one is already available.
    */
    if (ctxt_size == MR_CONTEXT_SIZE_SMALL && free_small_context_list) {
        c = free_small_context_list;
        free_small_context_list = c->MR_ctxt_next;
#if defined(MR_THREAD_SAFE) && defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT)
        if (MR_profile_parallel_execution) {
            MR_profile_parallel_small_context_reused++;
        }
#endif
    } else if (free_context_list != NULL) {
        c = free_context_list;
        free_context_list = c->MR_ctxt_next;
#if defined(MR_THREAD_SAFE) && defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT) 
        if (MR_profile_parallel_execution) {
            MR_profile_parallel_regular_context_reused++;
        }
#endif
    } else {
        c = NULL;
    }
#if defined(MR_LL_PARALLEL_CONJ) && defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT)
    context_id = MR_next_context_id++;
#endif
    MR_UNLOCK(&free_context_list_lock, "create_context i");

    if (c == NULL) {
        c = MR_GC_NEW(MR_Context);
        c->MR_ctxt_size = ctxt_size;
#ifndef MR_HIGHLEVEL_CODE
        c->MR_ctxt_detstack_zone = NULL;
        c->MR_ctxt_nondetstack_zone = NULL;
#endif
#ifdef MR_LL_PARALLEL_CONJ
        c->MR_ctxt_spark_deque.MR_sd_active_array = NULL;
#endif
#ifdef MR_USE_TRAIL
        c->MR_ctxt_trail_zone = NULL;
#endif
    }
#if defined(MR_LL_PARALLEL_CONJ) && defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT)
    c->MR_ctxt_num_id = context_id;
#endif
    
    MR_init_context_maybe_generator(c, id, gen);
    return c;
}

void 
MR_destroy_context(MR_Context *c)
{
    MR_assert(c);

#ifdef MR_THREAD_SAFE
    MR_assert(c->MR_ctxt_saved_owners == NULL);
#endif

    /* XXX not sure if this is an overall win yet */
#if 0 && defined(MR_CONSERVATIVE_GC) && !defined(MR_HIGHLEVEL_CODE)
    /* Clear stacks to prevent retention of data. */
    MR_clear_zone_for_GC(c->MR_ctxt_detstack_zone,
        c->MR_ctxt_detstack_zone->MR_zone_min);
    MR_clear_zone_for_GC(c->MR_ctxt_nondetstack_zone,
        c->MR_ctxt_nondetstack_zone->MR_zone_min);
#endif /* defined(MR_CONSERVATIVE_GC) && !defined(MR_HIGHLEVEL_CODE) */

    MR_LOCK(&free_context_list_lock, "destroy_context");
#ifdef MR_LL_PARALLEL_CONJ
    MR_num_outstanding_contexts_and_global_sparks--;
    MR_atomic_dec_int(&MR_num_outstanding_contexts_and_all_sparks);
#endif

    switch (c->MR_ctxt_size) {
        case MR_CONTEXT_SIZE_REGULAR:
            c->MR_ctxt_next = free_context_list;
            free_context_list = c;
#if defined(MR_THREAD_SAFE) && defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT)
            if (MR_profile_parallel_execution) {
                MR_profile_parallel_regular_context_kept++;
            }
#endif
            break;
        case MR_CONTEXT_SIZE_SMALL:
            c->MR_ctxt_next = free_small_context_list;
            free_small_context_list = c;
#if defined(MR_THREAD_SAFE) && defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT) 
            if (MR_profile_parallel_execution) {
                MR_profile_parallel_small_context_kept++;
            }
#endif
            break;
    }
    MR_UNLOCK(&free_context_list_lock, "destroy_context");
}

void 
MR_flounder(void)
{
    MR_fatal_error("computation floundered");
}

/*
** Check to see if any contexts that blocked on IO have become
** runnable. Return the number of contexts that are still blocked.
** The parameter specifies whether or not the call to select should
** block or not.
*/

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

#else   /* !MR_CAN_DO_PENDING_IO */

    MR_fatal_error("select() unavailable!");

#endif
}

void
MR_schedule_context(MR_Context *ctxt)
{
#if defined(MR_LL_PARALLEL_CONJ) && defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT)
    MR_threadscope_post_context_runnable(ctxt);
#endif
    MR_LOCK(&MR_runqueue_lock, "schedule_context");
    ctxt->MR_ctxt_next = NULL;
    if (MR_runqueue_tail) {
        MR_runqueue_tail->MR_ctxt_next = ctxt;
        MR_runqueue_tail = ctxt;
    } else {
        MR_runqueue_head = ctxt;
        MR_runqueue_tail = ctxt;
    }
#ifdef MR_THREAD_SAFE
    /*
    ** Wake one or more threads waiting in MR_do_runnext.  If there is a
    ** possibility that a woken thread might not accept this context then
    ** we wake up all the waiting threads.
    */
    if (ctxt->MR_ctxt_resume_owner_thread == (MercuryThread) NULL) {
        MR_SIGNAL(&MR_runqueue_cond, "schedule_context");
    } else {
        MR_BROADCAST(&MR_runqueue_cond, "schedule_context");
    }
#endif
    MR_UNLOCK(&MR_runqueue_lock, "schedule_context");
}

#ifdef MR_LL_PARALLEL_CONJ
void
MR_schedule_spark_globally(const MR_Spark *proto_spark)
{
    MR_LOCK(&MR_runqueue_lock, "schedule_spark_globally");
    MR_wsdeque_push_bottom(&MR_spark_queue, proto_spark);
    MR_num_outstanding_contexts_and_global_sparks++;
    MR_atomic_inc_int(&MR_num_outstanding_contexts_and_all_sparks);
    MR_SIGNAL(&MR_runqueue_cond, "schedule_spark_globally");
    MR_UNLOCK(&MR_runqueue_lock, "schedule_spark_globally");
}
#endif /* !MR_LL_PARALLEL_CONJ */


#ifndef MR_HIGHLEVEL_CODE

MR_define_extern_entry(MR_do_runnext);

MR_BEGIN_MODULE(scheduler_module)
    MR_init_entry_an(MR_do_runnext);
MR_BEGIN_CODE

MR_define_entry(MR_do_runnext);
#ifdef MR_THREAD_SAFE
{
    MR_Context      *tmp;
    MR_Context      *prev;
    MR_Spark        spark;
    unsigned        depth;
    MercuryThread   thd;
    int             wait_result;

#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
    MR_Timer        runnext_timer;
#endif
    /*
    ** If this engine is holding onto a context, the context should not be
    ** in the middle of running some code.
    */
    MR_assert(
        MR_ENGINE(MR_eng_this_context) == NULL
    ||
        MR_wsdeque_is_empty(
            &MR_ENGINE(MR_eng_this_context)->MR_ctxt_spark_deque)
    );

    depth = MR_ENGINE(MR_eng_c_depth);
    thd = MR_ENGINE(MR_eng_owner_thread);

    MR_LOCK(&MR_runqueue_lock, "MR_do_runnext (i)");

    MR_num_idle_engines++;

    while (1) {

#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
        if (MR_profile_parallel_execution) {
            MR_profiling_start_timer(&runnext_timer);
        }
#endif

        if (MR_exit_now) {
            /*
            ** The primordial thread has the responsibility of cleaning
            ** up the Mercury runtime.  It cannot exit by this route.
            */
            assert(thd != MR_primordial_thread);
            MR_destroy_thread(MR_cur_engine());
            MR_num_exited_engines++;
            MR_UNLOCK(&MR_runqueue_lock, "MR_do_runnext (ii)");
            pthread_exit(0);
        }

        /* Search for a ready context which we can handle. */
        tmp = MR_runqueue_head;
        /* XXX check pending io */
        prev = NULL;
        while (tmp != NULL) {
            if (tmp->MR_ctxt_resume_owner_thread == thd && 
                tmp->MR_ctxt_resume_c_depth == depth)
            {
                tmp->MR_ctxt_resume_owner_thread = (MercuryThread) NULL;
                tmp->MR_ctxt_resume_c_depth = 0;
                MR_num_idle_engines--;
                goto ReadyContext;
            }

            if (tmp->MR_ctxt_resume_owner_thread == (MercuryThread) NULL) {
                MR_num_idle_engines--;
                goto ReadyContext;
            }

            prev = tmp;
            tmp = tmp->MR_ctxt_next;
        }

        /* Check if the global spark queue is nonempty. */
        if (MR_wsdeque_take_top(&MR_spark_queue, &spark)) {
            MR_num_idle_engines--;
            MR_num_outstanding_contexts_and_global_sparks--;
            MR_atomic_dec_int(&MR_num_outstanding_contexts_and_all_sparks);
            goto ReadySpark;
        }

        /* Nothing to do, go back to sleep. */
#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
        if (MR_profile_parallel_execution) {
            MR_profiling_stop_timer(&runnext_timer, 
                    &MR_profile_parallel_executed_nothing);
        }
#endif
        do {
            wait_result = 
                MR_WAIT(&MR_runqueue_cond, &MR_runqueue_lock, "do_runnext");
        } while (wait_result != 0);
    }

  ReadyContext:

    if (prev != NULL) {
        prev->MR_ctxt_next = tmp->MR_ctxt_next;
    } else {
        MR_runqueue_head = tmp->MR_ctxt_next;
    }
    if (MR_runqueue_tail == tmp) {
        MR_runqueue_tail = prev;
    }
    MR_UNLOCK(&MR_runqueue_lock, "MR_do_runnext (iii)");

    /* Discard whatever unused context we may have and switch to tmp. */
    if (MR_ENGINE(MR_eng_this_context) != NULL) {
        MR_destroy_context(MR_ENGINE(MR_eng_this_context));
    }
#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
    if (MR_profile_parallel_execution) {
        MR_profiling_stop_timer(&runnext_timer, 
                &MR_profile_parallel_executed_contexts);
    }
#endif
    MR_ENGINE(MR_eng_this_context) = tmp;
    MR_load_context(tmp);
    MR_GOTO(tmp->MR_ctxt_resume);

  ReadySpark:

    MR_UNLOCK(&MR_runqueue_lock, "MR_do_runnext (iii)");

    /* Grab a new context if we haven't got one then begin execution. */
    if (MR_ENGINE(MR_eng_this_context) == NULL) {
        MR_ENGINE(MR_eng_this_context) = MR_create_context("from spark",
            MR_CONTEXT_SIZE_SMALL, NULL);
#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
        MR_threadscope_post_create_context_for_spark(MR_ENGINE(MR_eng_this_context));
        if (MR_profile_parallel_execution) {
            MR_atomic_inc_int(
                    &MR_profile_parallel_contexts_created_for_sparks);
        }
#endif
        MR_load_context(MR_ENGINE(MR_eng_this_context));
    } else {
#if defined(MR_LL_PARALLEL_CONJ) && defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT)
        MR_threadscope_post_run_context();
#endif
    }
    MR_parent_sp = spark.MR_spark_parent_sp;
    MR_assert(MR_parent_sp != MR_sp);
    MR_SET_THREAD_LOCAL_MUTABLES(spark.MR_spark_thread_local_mutables);
#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
    if (MR_profile_parallel_execution) {
        MR_profiling_stop_timer(&runnext_timer, 
                &MR_profile_parallel_executed_global_sparks);
    }
#endif
    MR_GOTO(spark.MR_spark_resume);
}
#else /* !MR_THREAD_SAFE */
{
    if (MR_runqueue_head == NULL && MR_pending_contexts == NULL) {
        MR_fatal_error("empty runqueue!");
    }

    while (MR_runqueue_head == NULL) {
        MR_check_pending_contexts(MR_TRUE); /* block */
    }

    MR_ENGINE(MR_eng_this_context) = MR_runqueue_head;
    MR_runqueue_head = MR_runqueue_head->MR_ctxt_next;
    if (MR_runqueue_head == NULL) {
        MR_runqueue_tail = NULL;
    }

    MR_load_context(MR_ENGINE(MR_eng_this_context));
    MR_GOTO(MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume);
}
#endif

MR_END_MODULE

#endif /* !MR_HIGHLEVEL_CODE */

#ifdef MR_LL_PARALLEL_CONJ
MR_Code*
MR_do_join_and_continue(MR_SyncTerm *jnc_st, MR_Code *join_label) 
{
    /*
     * XXX: We should take the current TSC time here and use it to post the
     * various 'context stopped' threadscope events.  This profile will be more
     * accurate. 
     */
    if (!jnc_st->MR_st_is_shared) {
        /* This parallel conjunction has only executed sequentially. */
        if (--jnc_st->MR_st_count == 0) {
            /*
            ** It has finished executing, continue execution from the join
            ** label.
            */
            return join_label;
        } else {
            /*
            ** It has not finished executing.  Try to finish it by executing
            ** our sparks.
            ** This code was formerly MR_join_and_continue_1() 
            */
            MR_Context  *jnc_ctxt;
            MR_bool     jnc_popped;
            MR_Spark    jnc_spark;

            jnc_ctxt = MR_ENGINE(MR_eng_this_context);
            jnc_popped = MR_wsdeque_pop_bottom(&jnc_ctxt->MR_ctxt_spark_deque,
                &jnc_spark);
            if (jnc_popped) {
                MR_atomic_dec_int(&MR_num_outstanding_contexts_and_all_sparks);
                return jnc_spark.MR_spark_resume;
            } else {
                /*
                ** Someone's stolen our sparks, we should try to execute
                ** something that's been scheduled globally.  XXX: We don't yet
                ** have work stealing so how can this happen!?
                */
                fprintf(stderr, "My sparks have been stolen!! %lp", pthread_self());
                return MR_ENTRY(MR_do_runnext);
            }
        }
    } else {
        /* This parallel conjunction may be executing in parallel. */
        MR_LOCK(&MR_sync_term_lock, "continue");
        if (--jnc_st->MR_st_count == 0) {
            /* This parallel conjunction has finished. */
            if (MR_ENGINE(MR_eng_this_context) == jnc_st->MR_st_orig_context) {
                /*
                ** This context originated this parallel conjunction and all
                ** the branches have finished so jump to the join label.
                */
                MR_UNLOCK(&MR_sync_term_lock, "continue i");
                return join_label;
            } else {
                /*
                ** This context didn't originate this parallel conjunction and
                ** we're the last branch to finish.  The originating context
                ** should be suspended waiting for us to finish, so wake it up.
                */
                jnc_st->MR_st_orig_context->MR_ctxt_resume = join_label;
                MR_schedule_context(jnc_st->MR_st_orig_context);
                MR_UNLOCK(&MR_sync_term_lock, "continue ii");
#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
                MR_threadscope_post_stop_context(MR_TS_STOP_REASON_FINISHED);
#endif
                return MR_ENTRY(MR_do_runnext);
            }
        } else {
            /*
            ** The parallel conjunction is being executed in parallel but it is
            ** not yet finished.  This code was Formerly
            ** MR_join_and_continue_2() 
            */
            MR_Context  *jnc_ctxt;
            MR_bool     jnc_popped;
            MR_Spark    jnc_spark;

            jnc_ctxt = MR_ENGINE(MR_eng_this_context);
            jnc_popped = MR_wsdeque_pop_bottom(&jnc_ctxt->MR_ctxt_spark_deque,
                &jnc_spark);
            if (jnc_popped && (jnc_spark.MR_spark_parent_sp == MR_parent_sp)) {
                /*
                ** The spark at the top of the stack is from to the same parallel
                ** conjunction that we've just been executing. We can immediately
                ** execute the next branch of the same parallel conjunction in
                ** the current context.
                */
                MR_UNLOCK(&MR_sync_term_lock, "continue_2 i");
                MR_atomic_dec_int(&MR_num_outstanding_contexts_and_all_sparks);
                return jnc_spark.MR_spark_resume;
            } else {
                /*
                ** The spark stack is empty or the next spark is from a different
                ** parallel conjunction to the one we've been executing.  Either
                ** way, there's nothing more we can do with this context right
                ** now.  Put back the spark we won't be using.
                */
                if (jnc_popped) {
                    MR_wsdeque_putback_bottom(&jnc_ctxt->MR_ctxt_spark_deque,
                        &jnc_spark);
                }
                /*
                ** If this context originated the parallel conjunction we've been
                ** executing, the rest of the parallel conjunction must have been
                ** put on the global spark queue to be executed in other
                ** contexts.  This context will need to be resumed once the
                ** parallel conjunction is completed, so suspend the context.
                **
                ** What if the other conjuncts where put on the global queue
                ** but haven't yet been taken by other threads?  Then this step
                ** is redundant.  It's not worth fixing, this problem will go
                ** away once we enable work-stealing. - pbone. 
                */
                if (jnc_ctxt == jnc_st->MR_st_orig_context) {
#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
                    MR_threadscope_post_stop_context(MR_TS_STOP_REASON_BLOCKED);
#endif
                    MR_save_context(jnc_ctxt);
                    MR_ENGINE(MR_eng_this_context) = NULL;
                } else {
#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
                    MR_threadscope_post_stop_context(MR_TS_STOP_REASON_FINISHED);
#endif
                }

                /* Finally look for other work. */
                MR_UNLOCK(&MR_sync_term_lock, "continue_2 ii");
                return MR_ENTRY(MR_do_runnext);
            }
        }
    }
}
#endif

#ifdef MR_LL_PARALLEL_CONJ

/*
 * Debugging functions for runtime granularity control.
 */

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

#endif /* MR_DEBUG_RUNTIME_GRANULARITY_CONTROL */
#endif /* MR_LL_PARALLEL_CONJ */

/* forward decls to suppress gcc warnings */
void mercury_sys_init_scheduler_wrapper_init(void);
void mercury_sys_init_scheduler_wrapper_init_type_tables(void);
#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_scheduler_wrapper_write_out_proc_statics(FILE *fp);
#endif

void mercury_sys_init_scheduler_wrapper_init(void)
{
#ifndef MR_HIGHLEVEL_CODE
    scheduler_module();
#endif
}

void mercury_sys_init_scheduler_wrapper_init_type_tables(void)
{
    /* no types to register */
}

#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_scheduler_wrapper_write_out_proc_statics(FILE *fp)
{
    /* no proc_statics to write out */
}
#endif
