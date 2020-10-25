// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1997-2007, 2009-2011 The University of Melbourne.
// Copyright (C) 2014-2016, 2018, 2020 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_context.h - defines Mercury multithreading stuff.
//
// A "context" is a Mercury thread. (We use a different term than "thread"
// to avoid confusing Mercury threads and POSIX threads.)
// Each context is represented by a value of type MR_Context,
// which contains a detstack, a nondetstack, a trail (if needed), the various
// pointers that refer to them, a succip, and a thread-resumption continuation.
// Contexts are initially stored in a free-list.
// When one is running, the POSIX thread that is executing it has a pointer
// to its context structure `this_context'. (WARNING: code that manipulates
// contexts must set this_context itself; it cannot rely on the generic
// mechanisms below to set it.) When a context suspends, it calls
// `MR_save_context(context_ptr)' which copies the context from the
// various registers and global variables into the structure referred to
// by `context_ptr'. The context contains no rN or fN registers - all
// registers are "context save" (by analogy to caller-save).
//
// When a new context is created for a parallel conjunction, information is
// passed to and from the new context via the stack frame of the procedure that
// originated the parallel conjunction. The code of a parallel conjunct has
// access to that original stack frame via the `parent_sp' register.
//
// Contexts can migrate transparently between multiple POSIX threads.
//
// Each POSIX thread has its own heap and solutions heap (both allocated
// in shared memory). This makes GC harder, but enables heap allocation
// to be done without locking which is very important for performance.
// Each context has a copy of the heap pointer that is taken when it is
// switched out. If the POSIX thread's heap pointer is the same as the
// copied one when the context is switched back in, then it is safe for
// the context to do heap reclamation on failure.
//
// If MR_THREAD_SAFE is not defined, then everything gets executed within a
// single POSIX thread. No locking is required.

#ifndef MERCURY_CONTEXT_H
#define MERCURY_CONTEXT_H

#include "mercury_regs.h"       // for MR_hp, etc.
                                // Must come before system headers.

#include <stdio.h>

#include "mercury_types.h"      // for MR_Word, MR_Code, etc
#include "mercury_trail.h"      // for MR_TrailEntry
#include "mercury_memory.h"     // for MR_MemoryZone
#include "mercury_thread.h"     // for MercuryLock
#include "mercury_goto.h"       // for MR_GOTO()
#include "mercury_conf.h"       // for MR_CONSERVATIVE_GC
#include "mercury_backjump.h"   // for MR_BackJumpHandler, etc
#include "mercury_atomic_ops.h" // for MR_atomic_*

#ifdef  MR_THREAD_SAFE
  #define MR_IF_THREAD_SAFE(x)  x
  #define MR_IF_NOT_THREAD_SAFE(x)
#else
  #define MR_IF_THREAD_SAFE(x)
  #define MR_IF_NOT_THREAD_SAFE(x) x
#endif

// Each engine has one MR_Context structure loaded into it (in the engine field
// named MR_eng_context) from a context which is pointed to by the engine's
// MR_eng_this_context field. Fields which can be expected to be accessed at
// least several times between context switches are accessed via MR_eng_context
// while the rest are accessed via MR_eng_this_context (which requires
// following an extra pointer). Note that some fields are further cached
// in abstract machine registers, and some in fact are only ever accessed
// via these abstract machine registers. The saved copies of some these
// abstract machine registers are kept not in the named fields below, but in
// the engine's fake reg array.
//
// All fields accessed via MR_eng_context and via abstract machine registers
// should be mentioned in the MR_save_context and MR_load_context macros.
// All fields accessed via MR_eng_this_context should be mentioned in the
// MR_copy_eng_this_context_fields macro. All fields accessed via direct
// specification of the context need explicit code to set them in all places
// where we create new contexts: in the mercury_thread module for parallelism,
// and in the mercury_mm_own_stacks module for minimal model tabling.
//
// The context structure has the following fields. The documentation of each
// field says how it is accessed, but please take this info with a pinch of
// salt; I (zs) don't guarantee its accuracy.
//
// id               A string to identify the context for humans who want to
//                  debug the handling of contexts.
//                  (Not accessed.)
//
// size             Whether this context has regular-sized stacks or smaller
//                  stacks. Some parallel programs can allocate many contexts
//                  and most parallel computations should not require very
//                  large stacks. We allocate contexts with "smaller" stacks
//                  for parallel computations (although whether they are
//                  actually smaller is up to the user).
//                  (Accessed only when directly specifying the context.)
//
// next             If this context is in the free-list `next' will point to
//                  the next free context. If this context is suspended waiting
//                  for a variable to become bound, `next' will point to the
//                  next waiting context. If this context is runnable but not
//                  currently running then `next' points to the next runnable
//                  context in the runqueue.
//                  (Accessed only when directly specifying the context.)
//
// exclusive_engine
//                  Either MR_ENGINE_ID_NONE, or else the exclusive engine
//                  that this context belongs to. A context with an exclusive
//                  engine may only be run on that engine. This restriction
//                  may be relaxed in the future so that it only applies when
//                  entering some foreign procs.
//                  (Accessed only when directly specifying the context.)
//
// resume           A pointer to the code at which execution should resume
//                  when this context is next scheduled.
//                  (Accessed via MR_eng_this_context.)
//
// resume_engine
//                  When resuming a context this is the engine that it prefers
//                  or is required to be resumed on. Doing so can avoid cache
//                  misses as the engine's cache may already be warm.
//                  (Accessed only when directly specifying the context.)
//
// resume_engine_required
// resume_c_depth
//                  If resume_engine_required is MR_FALSE then resume_engine is
//                  simply a preference, and the resume_c_depth field has no
//                  meaning. If resume_engine_required is MR_TRUE then
//                  resume_engine and resume_c_depth must match the engine's id
//                  and c_depth, to ensure that when we enter a Mercury engine
//                  from C we return to the same engine. See the comments in
//                  mercury_engine.h.
//                  (Both accessed only when directly specifying the context.)
//
// resume_stack
//                  A stack used to record the Mercury engines on which this
//                  context executed some C calls that called back into
//                  Mercury. We must execute this context in the correct
//                  engine when returning to those C calls. See the comments
//                  in mercury_engine.h.
//                  (Accessed via MR_eng_this_context.)
//
// succip           The succip for this context.
//                  (Accessed via abstract machine register.)
//
// detstack_zone    The current detstack zone for this context.
// prev_detstack_zones
//                  A list of any previous detstack zones for this context.
//                  (Both accessed via MR_eng_context.)
// sp               The saved sp for this context.
//                  (Accessed via abstract machine register.)
//
// nondetstack_zone The current nondetstack zone for this context.
// prev_nondetstack_zones
//                  A list of any previous nondetstack zones for this context.
//                  (Both accessed via MR_eng_context.)
// curfr            The saved curfr for this context.
// maxfr            The saved maxfr for this context.
//                  (Both accessed via abstract machine register.)
//
// genstack_zone    The generator stack zone for this context.
//                  (Accessed via MR_eng_context.)
// gen_next         The saved gen_next for this context.
//                  (Accessed via abstract machine register.)
//
// cutstack_zone    The cut stack zone for this context.
//                  (Accessed via MR_eng_context.)
// cut_next         The saved cut_next for this context.
//                  (Accessed via abstract machine register.)
//
// pnegstack_zone   The possibly_negated_context stack zone for this context.
//                  (Accessed via MR_eng_context.)
// pneg_next        The saved pneg_next for this context.
//                  (Accessed via abstract machine register.)
//
// parent_sp        The saved parent_sp for this context.
//                  (Accessed via abstract machine register.)
//
// trail_zone       The trail zone for this context.
// prev_trail_zones A list of any previous trail zones for this context.
//                  (Accessed via MR_eng_context.)
//
// trail_ptr        The saved MR_trail_ptr for this context.
// ticket_counter   The saved MR_ticket_counter for this context.
// ticket_highwater The saved MR_ticket_high_water for this context.
//                  (All accessed via abstract machine register.)
//
// backjump_handler         The backjump handler for this context.
// backjump_next_choice_id  The next available backjump choice id counter
//                          for this context.
//                          (All accessed via MR_eng_context.)
//
// hp               The saved hp for this context.
//                  (Accessed via abstract machine register.)
//
// min_hp_rec       This pointer marks the minimum value of MR_hp to which
//                  we can truncate the heap on backtracking. See comments
//                  before the macro MR_set_min_heap_reclamation_point below.
//                  (Accessed via abstract machine register.)
//
// thread_local_mutables
//                  The array of thread-local mutable values for this context.
//                  (Accessed via MR_eng_this_context.)

typedef struct MR_Context_Struct        MR_Context;

typedef enum {
    MR_CONTEXT_SIZE_REGULAR,
// Stack segment grades don't need differently sized contexts.

#ifndef MR_STACK_SEGMENTS
    MR_CONTEXT_SIZE_SMALL
#endif
} MR_ContextSize;

#ifdef MR_STACK_SEGMENTS
#define MR_CONTEXT_SIZE_FOR_SPARK               MR_CONTEXT_SIZE_REGULAR
#define MR_CONTEXT_SIZE_FOR_LOOP_CONTROL_WORKER MR_CONTEXT_SIZE_REGULAR
#else
#define MR_CONTEXT_SIZE_FOR_SPARK               MR_CONTEXT_SIZE_SMALL
#define MR_CONTEXT_SIZE_FOR_LOOP_CONTROL_WORKER MR_CONTEXT_SIZE_SMALL
#endif

#ifdef MR_THREAD_SAFE
typedef struct MR_ResumeStack_Struct    MR_ResumeStack;

struct MR_ResumeStack_Struct {
    MR_EngineId             MR_resume_engine;
    MR_Unsigned             MR_resume_c_depth;
    MR_ResumeStack          *MR_resume_stack_next;
};
#endif

#ifdef MR_LL_PARALLEL_CONJ
typedef struct MR_SyncTerm_Struct       MR_SyncTerm;
typedef struct MR_Spark_Struct          MR_Spark;
typedef struct MR_SparkDeque_Struct     MR_SparkDeque;
typedef struct MR_SparkArray_Struct     MR_SparkArray;

// A spark contains just enough information to begin execution of a parallel
// conjunct. A spark will either be executed in the same context (same
// detstack, etc.) as the code that generated the spark, or it may be stolen
// from its deque and executed by any idle engine in a different context.

struct MR_Spark_Struct {
    MR_SyncTerm             *MR_spark_sync_term;
    MR_Code                 *MR_spark_resume;
    MR_ThreadLocalMuts      *MR_spark_thread_local_mutables;
#ifdef MR_THREADSCOPE
    // XXX this is not wide enough for higher engine ids
    MR_uint_least32_t       MR_spark_id;
#endif
};

#define CACHE_LINE_SIZE 64
#define PAD_CACHE_LINE(s)                                               \
    ((CACHE_LINE_SIZE) > (s) ? (CACHE_LINE_SIZE) - (s) : 0)

struct MR_SparkDeque_Struct {
    // The top index is modified by thiefs; the other fields are modified by
    // the owner. Therefore we pad out the structure to reduce false
    // sharing.

    volatile MR_Integer     MR_sd_top;
    char padding[PAD_CACHE_LINE(sizeof(MR_Integer))];

    volatile MR_Integer     MR_sd_bottom;
    volatile MR_SparkArray  *MR_sd_active_array;
};
#endif  // !MR_LL_PARALLEL_CONJ

struct MR_Context_Struct {
    const char          *MR_ctxt_id;
#ifdef MR_THREADSCOPE
    MR_Unsigned         MR_ctxt_num_id;
#endif
    MR_ContextSize      MR_ctxt_size;
    MR_Context          *MR_ctxt_next;
#ifdef  MR_LL_PARALLEL_CONJ
    // The value of this field is used for synchronization.
    MR_Code * volatile  MR_ctxt_resume;
#else
    MR_Code             *MR_ctxt_resume;
#endif
#ifdef  MR_THREAD_SAFE
    MR_EngineId         MR_ctxt_exclusive_engine;
    MR_EngineId         MR_ctxt_resume_engine;
    MR_bool             MR_ctxt_resume_engine_required;
    MR_Unsigned         MR_ctxt_resume_c_depth;
    MR_ResumeStack      *MR_ctxt_resume_stack;
#endif

#ifndef MR_HIGHLEVEL_CODE
    MR_Code             *MR_ctxt_succip;

    MR_MemoryZone       *MR_ctxt_detstack_zone;
    MR_MemoryZones      *MR_ctxt_prev_detstack_zones;
    MR_Word             *MR_ctxt_sp;

    MR_MemoryZone       *MR_ctxt_nondetstack_zone;
    MR_MemoryZones      *MR_ctxt_prev_nondetstack_zones;
    MR_Word             *MR_ctxt_maxfr;
    MR_Word             *MR_ctxt_curfr;

  #ifdef MR_USE_MINIMAL_MODEL_STACK_COPY
    MR_MemoryZone       *MR_ctxt_genstack_zone;
    MR_Integer          MR_ctxt_gen_next;

    MR_MemoryZone       *MR_ctxt_cutstack_zone;
    MR_Integer          MR_ctxt_cut_next;

    MR_MemoryZone       *MR_ctxt_pnegstack_zone;
    MR_Integer          MR_ctxt_pneg_next;

  #endif // MR_USE_MINIMAL_MODEL_STACK_COPY
  #ifdef MR_USE_MINIMAL_MODEL_OWN_STACKS
    MR_Generator        *MR_ctxt_owner_generator;
  #endif // MR_USE_MINIMAL_MODEL_OWN_STACKS

  #ifdef MR_LL_PARALLEL_CONJ
    MR_Word             *MR_ctxt_parent_sp;
  #endif
#endif // !MR_HIGHLEVEL_CODE

#ifdef  MR_USE_TRAIL
    MR_MemoryZone       *MR_ctxt_trail_zone;
  #ifndef MR_USE_FIXED_SIZE_TRAIL
    MR_MemoryZones      *MR_ctxt_prev_trail_zones;
  #endif
    MR_TrailEntry       *MR_ctxt_trail_ptr;
    MR_ChoicepointId    MR_ctxt_ticket_counter;
    MR_ChoicepointId    MR_ctxt_ticket_high_water;
#endif

#ifndef MR_HIGHLEVEL_CODE
    MR_BackJumpHandler  *MR_ctxt_backjump_handler;
    MR_BackJumpChoiceId MR_ctxt_backjump_next_choice_id;
#endif

#ifndef MR_CONSERVATIVE_GC
    MR_Word             *MR_ctxt_hp;
    MR_Word             *MR_ctxt_min_hp_rec;
#endif

#ifdef  MR_EXEC_TRACE_INFO_IN_CONTEXT
    MR_Unsigned         MR_ctxt_call_seqno;
    MR_Unsigned         MR_ctxt_call_depth;
    MR_Unsigned         MR_ctxt_event_number;
#endif

    MR_ThreadLocalMuts  *MR_ctxt_thread_local_mutables;
};

// The runqueue is a linked list of contexts that are runnable.

extern      MR_Context  *MR_runqueue_head;
extern      MR_Context  *MR_runqueue_tail;
#ifdef  MR_THREAD_SAFE
  extern    MercuryLock MR_runqueue_lock;
  extern    MercuryCond MR_runqueue_cond;
#endif
#if defined(MR_LL_PARALLEL_CONJ) && defined(MR_HAVE_THREAD_PINNING)
  extern    MR_bool     MR_thread_pinning;
#endif

#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
extern MR_bool      MR_profile_parallel_execution;

// XXX: This is currently unused, we plan to use it in the future. -pbone
extern MR_Stats     MR_profile_parallel_executed_local_sparks;
#endif

// As well as the runqueue, we maintain a linked list of contexts
// and associated file descriptors that are suspended blocked for
// reads/writes/exceptions. When the runqueue becomes empty, if
// this list is not empty then we call select and block until one
// or more of the file descriptors become ready for I/O, then
// wake the appropriate context.
// In addition, we should periodically check to see if the list of blocked
// contexts is non-empty and if so, poll to wake any contexts that
// can unblock. This, while not yielding true fairness (since this
// requires the current context to perform some yield-like action),
// ensures that it is possible for programmers to write concurrent
// programs with continuous computation and interleaved I/O dependent
// computation in a straight-forward manner. This polling is not
// currently implemented.

typedef enum {
    MR_PENDING_READ  = 0x01,
    MR_PENDING_WRITE = 0x02,
    MR_PENDING_EXEC  = 0x04
} MR_WaitingMode;

typedef struct MR_PendingContext_Struct {
    struct MR_PendingContext_Struct *next;
    MR_Context                      *context;
    int                             fd;
    MR_WaitingMode                  waiting_mode;
} MR_PendingContext;

extern  MR_PendingContext   *MR_pending_contexts;
#ifdef  MR_THREAD_SAFE
  extern    MercuryLock     MR_pending_contexts_lock;
#endif

#ifdef  MR_LL_PARALLEL_CONJ
  // The number of work-stealing engines waiting for work.
  // We don't protect it with a separate lock, but updates to it are made while
  // holding the MR_runqueue_lock. Reads are made without the lock.
  // XXX We may need to use atomic instructions or memory fences on some
  // architectures.

  extern volatile MR_Integer    MR_num_idle_ws_engines;

  // Spark deques for work stealing,  These are made visible so that they can
  // be initialised by code in mercury_thread.c.

  extern MR_SparkDeque          **MR_spark_deques;
#endif  // !MR_LL_PARALLEL_CONJ

////////////////////////////////////////////////////////////////////////////

#ifdef MR_THREAD_SAFE
// Return the number of processors available to this process or 0 if unknown.
// This function is not directly related to contexts, but shares code with the
// code to count the number of Mercury engines to start.
extern unsigned     MR_get_num_processors(void);
#endif

////////////////////////////////////////////////////////////////////////////

// Allocates and initializes a new context structure, and gives it
// the given id. If gen is non-NULL, the context is for the given generator.
// The `MR_ctxt_thread_local_mutables' member must be initialised separately.

extern  MR_Context  *MR_create_context(const char *id,
                        MR_ContextSize ctxt_size, MR_Generator *gen);

// MR_release_context(context) returns the pointed-to context structure
// to the free list, and releases resources as necessary.
//
// VERY IMPORTANT: Call MR_save_context() before you call MR_destroy_context().
// Contexts are cached and calling MR_save_context() saves important
// book-keeping information, like the stack pointer and current stack segment.
// If you do not call these then an old, and since freed (or re-used elsewhere)
// stack segment may still be referenced by the context. If that context
// is reused later, then it will clobber another context's stack!

extern  void        MR_release_context(MR_Context *context);

// MR_init_context_stuff() initializes the lock structures for the runqueue,
// and detects the number of threads to use on the LLC backend.

extern  void        MR_init_context_stuff(void);

#if defined(MR_LL_PARALLEL_CONJ) && defined(MR_HAVE_THREAD_PINNING)
// MR_pin_thread() pins the current thread to the next available processor ID,
// if thread pinning is enabled.
// MR_pin_primordial_thread() is a special case for the primordial thread.
// It should only be executed once, and only by the primordial thread _before_
// the other threads are started.
//
// Both functions return the CPU number that the thread is pinned to or would
// be pinned to if pinning was both enabled and supported. That is a valid
// value is always returned even if the thread is not actually pinned.
extern int          MR_pin_primordial_thread(void);
extern int          MR_pin_thread(void);

// Free resources no longer required after thread pinning is done.
extern void         MR_done_thread_pinning(void);
#endif

#ifdef MR_LL_PARALLEL_CONJ
// Shutdown all the work-stealing engines.
// (Exclusive engines shut down by themselves.)
extern void         MR_shutdown_ws_engines(void);
#endif

// MR_finalize_context_stuff() finalizes the lock structures for the runqueue
// among other things setup by MR_init_context_stuff().

extern  void        MR_finalize_context_stuff(void);

// MR_flounder() aborts with a runtime error message. It is called if
// the runqueue becomes empty and none of the running processes are
// working, which means that the computation has floundered.

extern  void        MR_flounder(void);

// Relinquish the processor voluntarily without blocking.

extern  void        MR_sched_yield(void);

// Append the given context onto the end of the run queue.

extern  void        MR_schedule_context(MR_Context *ctxt);

#ifndef MR_HIGHLEVEL_CODE
// MR_idle() should be called by an engine without a context that is looking
// for more work.

  MR_declare_entry(MR_do_idle);
  #define MR_idle()                                                     \
    do {                                                                \
        MR_GOTO(MR_ENTRY(MR_do_idle));                                  \
    } while (0)
#endif

#ifndef MR_CONSERVATIVE_GC

  // To figure out the maximum amount of heap we can reclaim on backtracking,
  // we compare MR_hp with the MR_ctxt_hp.
  //
  // If MR_ctxt_hp == NULL then this is the first time this context has been
  // scheduled, so the furthest back down the heap we can reclaim is to the
  // current value of MR_hp.
  //
  // If MR_hp > MR_ctxt_hp, another context has allocated data on the heap
  // since we were last scheduled, so the furthest back that we can reclaim is
  // to the current value of MR_hp, so we set MR_min_hp_rec and the
  // field of the same name in our context structure.
  //
  // If MR_hp < MR_ctxt_hp, then another context has truncated the heap on
  // failure. For this to happen, it must be the case that last time we were
  // that other context was the last one to allocate data on the heap, and we
  // scheduled, did not allocate any heap during that period of execution.
  // That being the case, the furthest back to which we can reset the heap is
  // to the current value of hp. This is a conservative approximation - it is
  // possible that the current value of hp is the same as some previous value
  // that we held, and we are now contiguous with our older data, so this
  // algorithm will lead to holes in the heap, though GC will reclaim these.
  //
  // If hp == MR_ctxt_hp then no other process has allocated any heap since we
  // were last scheduled, so we can proceed as if we had not stopped, and the
  // furthest back that we can backtrack is the same as it was last time we
  // were executing.

  #define MR_set_min_heap_reclamation_point(ctxt)                        \
    do {                                                                 \
        if (MR_hp != (ctxt)->MR_ctxt_hp || (ctxt)->MR_ctxt_hp == NULL) { \
            MR_min_hp_rec = MR_hp;                                       \
            (ctxt)->MR_ctxt_min_hp_rec = MR_hp;                          \
        } else {                                                         \
            MR_min_hp_rec = (ctxt)->MR_ctxt_min_hp_rec;                  \
        }                                                                \
    } while (0)

  #define MR_save_hp_in_context(ctxt)                                   \
    do {                                                                \
        (ctxt)->MR_ctxt_hp = MR_hp;                                     \
        (ctxt)->MR_ctxt_min_hp_rec = MR_min_hp_rec;                     \
    } while (0)

#else

  #define MR_set_min_heap_reclamation_point(ctxt)   do { } while (0)

  #define MR_save_hp_in_context(ctxt)               do { } while (0)

#endif

#ifdef MR_USE_TRAIL
  #define MR_IF_USE_TRAIL(x) x
#else
  #define MR_IF_USE_TRAIL(x)
#endif

#ifdef MR_USE_MINIMAL_MODEL_STACK_COPY
  #define MR_IF_USE_MINIMAL_MODEL_STACK_COPY(x) x
#else
  #define MR_IF_USE_MINIMAL_MODEL_STACK_COPY(x)
#endif

#ifdef MR_EXEC_TRACE_INFO_IN_CONTEXT
  #define MR_IF_EXEC_TRACE_INFO_IN_CONTEXT(x) x
#else
  #define MR_IF_EXEC_TRACE_INFO_IN_CONTEXT(x)
#endif

#ifndef MR_HIGHLEVEL_CODE
  #define MR_IF_NOT_HIGHLEVEL_CODE(x) x
#else
  #define MR_IF_NOT_HIGHLEVEL_CODE(x)
#endif

#ifdef MR_THREADSCOPE
  #define MR_IF_THREADSCOPE(x) x
#else
  #define MR_IF_THREADSCOPE(x)
#endif

#ifdef MR_WORKSTEAL_POLLING
  #define MR_IF_NOT_WORKSTEAL_POLLING(x)
#else
  #define MR_IF_NOT_WORKSTEAL_POLLING(x) x
#endif

#define MR_load_context(cptr)                                                 \
    do {                                                                      \
        MR_Context  *load_context_c;                                          \
                                                                              \
        load_context_c = (cptr);                                              \
        MR_IF_NOT_HIGHLEVEL_CODE(                                             \
            MR_succip_word = (MR_Word) load_context_c->MR_ctxt_succip;        \
            MR_sp_word     = (MR_Word) load_context_c->MR_ctxt_sp;            \
            MR_maxfr_word  = (MR_Word) load_context_c->MR_ctxt_maxfr;         \
            MR_curfr_word  = (MR_Word) load_context_c->MR_ctxt_curfr;         \
            MR_IF_USE_MINIMAL_MODEL_STACK_COPY(                               \
                MR_gen_next = load_context_c->MR_ctxt_gen_next;               \
                MR_cut_next = load_context_c->MR_ctxt_cut_next;               \
                MR_pneg_next = load_context_c->MR_ctxt_pneg_next;             \
            )                                                                 \
            MR_IF_THREAD_SAFE(                                                \
                MR_parent_sp = load_context_c->MR_ctxt_parent_sp;             \
            )                                                                 \
        )                                                                     \
        MR_IF_USE_TRAIL(                                                      \
            MR_IF_NOT_THREAD_SAFE(                                            \
                MR_trail_zone = load_context_c->MR_ctxt_trail_zone;           \
            )                                                                 \
            MR_IF_THREAD_SAFE(                                                \
                MR_ENGINE(MR_eng_context).MR_ctxt_trail_zone =                \
                    load_context_c->MR_ctxt_trail_zone;                       \
            )                                                                 \
            MR_trail_ptr = load_context_c->MR_ctxt_trail_ptr;                 \
            MR_ticket_counter = load_context_c->MR_ctxt_ticket_counter;       \
            MR_ticket_high_water = load_context_c->MR_ctxt_ticket_high_water; \
        )                                                                     \
        MR_IF_NOT_HIGHLEVEL_CODE(                                             \
            MR_ENGINE(MR_eng_context).MR_ctxt_detstack_zone =                 \
                load_context_c->MR_ctxt_detstack_zone;                        \
            MR_ENGINE(MR_eng_context).MR_ctxt_prev_detstack_zones =           \
                load_context_c->MR_ctxt_prev_detstack_zones;                  \
            MR_ENGINE(MR_eng_context).MR_ctxt_nondetstack_zone =              \
                load_context_c->MR_ctxt_nondetstack_zone;                     \
            MR_ENGINE(MR_eng_context).MR_ctxt_prev_nondetstack_zones =        \
                load_context_c->MR_ctxt_prev_nondetstack_zones;               \
            MR_IF_USE_MINIMAL_MODEL_STACK_COPY(                               \
                MR_ENGINE(MR_eng_context).MR_ctxt_genstack_zone =             \
                    load_context_c->MR_ctxt_genstack_zone;                    \
                MR_ENGINE(MR_eng_context).MR_ctxt_cutstack_zone =             \
                    load_context_c->MR_ctxt_cutstack_zone;                    \
                MR_ENGINE(MR_eng_context).MR_ctxt_pnegstack_zone =            \
                    load_context_c->MR_ctxt_pnegstack_zone;                   \
                MR_gen_stack = (MR_GenStackFrame *)                           \
                    MR_ENGINE(MR_eng_context).MR_ctxt_genstack_zone->         \
                        MR_zone_min;                                          \
                MR_cut_stack = (MR_CutStackFrame *)                           \
                    MR_ENGINE(MR_eng_context).MR_ctxt_cutstack_zone->         \
                        MR_zone_min;                                          \
                MR_pneg_stack = (MR_PNegStackFrame *)                         \
                    MR_ENGINE(MR_eng_context).MR_ctxt_pnegstack_zone->        \
                        MR_zone_min;                                          \
            )                                                                 \
            MR_IF_EXEC_TRACE_INFO_IN_CONTEXT(                                 \
                MR_trace_call_seqno = load_context_c->MR_ctxt_call_seqno;     \
                MR_trace_call_depth = load_context_c->MR_ctxt_call_depth;     \
                MR_trace_event_number = load_context_c->MR_ctxt_event_number; \
            )                                                                 \
        )                                                                     \
        MR_set_min_heap_reclamation_point(load_context_c);                    \
    } while (0)

#define MR_save_context(cptr)                                                 \
    do {                                                                      \
        MR_Context  *save_context_c;                                          \
                                                                              \
        save_context_c = (cptr);                                              \
        MR_IF_NOT_HIGHLEVEL_CODE(                                             \
            save_context_c->MR_ctxt_succip  = MR_succip;                      \
            save_context_c->MR_ctxt_sp      = MR_sp;                          \
            save_context_c->MR_ctxt_maxfr   = MR_maxfr;                       \
            save_context_c->MR_ctxt_curfr   = MR_curfr;                       \
            MR_IF_USE_MINIMAL_MODEL_STACK_COPY(                               \
                save_context_c->MR_ctxt_gen_next = MR_gen_next;               \
                save_context_c->MR_ctxt_cut_next = MR_cut_next;               \
                save_context_c->MR_ctxt_pneg_next = MR_pneg_next;             \
            )                                                                 \
            MR_IF_THREAD_SAFE(                                                \
                save_context_c->MR_ctxt_parent_sp = MR_parent_sp;             \
            )                                                                 \
        )                                                                     \
        MR_IF_USE_TRAIL(                                                      \
            MR_IF_NOT_THREAD_SAFE(                                            \
                save_context_c->MR_ctxt_trail_zone = MR_trail_zone;           \
            )                                                                 \
            MR_IF_THREAD_SAFE(                                                \
                save_context_c->MR_ctxt_trail_zone =                          \
                    MR_ENGINE(MR_eng_context).MR_ctxt_trail_zone;             \
            )                                                                 \
            save_context_c->MR_ctxt_trail_ptr = MR_trail_ptr;                 \
            save_context_c->MR_ctxt_ticket_counter = MR_ticket_counter;       \
            save_context_c->MR_ctxt_ticket_high_water = MR_ticket_high_water; \
        )                                                                     \
        MR_IF_NOT_HIGHLEVEL_CODE(                                             \
            save_context_c->MR_ctxt_detstack_zone =                           \
                MR_ENGINE(MR_eng_context).MR_ctxt_detstack_zone;              \
            save_context_c->MR_ctxt_prev_detstack_zones =                     \
                MR_ENGINE(MR_eng_context).MR_ctxt_prev_detstack_zones;        \
            save_context_c->MR_ctxt_nondetstack_zone =                        \
                MR_ENGINE(MR_eng_context).MR_ctxt_nondetstack_zone;           \
            save_context_c->MR_ctxt_prev_nondetstack_zones =                  \
                MR_ENGINE(MR_eng_context).MR_ctxt_prev_nondetstack_zones;     \
            MR_IF_USE_MINIMAL_MODEL_STACK_COPY(                               \
                save_context_c->MR_ctxt_genstack_zone =                       \
                    MR_ENGINE(MR_eng_context).MR_ctxt_genstack_zone;          \
                save_context_c->MR_ctxt_cutstack_zone =                       \
                    MR_ENGINE(MR_eng_context).MR_ctxt_cutstack_zone;          \
                save_context_c->MR_ctxt_pnegstack_zone =                      \
                    MR_ENGINE(MR_eng_context).MR_ctxt_pnegstack_zone;         \
                MR_assert(MR_gen_stack == (MR_GenStackFrame *)                \
                    MR_ENGINE(MR_eng_context).MR_ctxt_genstack_zone->         \
                        MR_zone_min);                                         \
                MR_assert(MR_cut_stack == (MR_CutStackFrame *)                \
                    MR_ENGINE(MR_eng_context).MR_ctxt_cutstack_zone->         \
                        MR_zone_min);                                         \
                MR_assert(MR_pneg_stack == (MR_PNegStackFrame *)              \
                    MR_ENGINE(MR_eng_context).MR_ctxt_pnegstack_zone->        \
                        MR_zone_min);                                         \
            )                                                                 \
            MR_IF_EXEC_TRACE_INFO_IN_CONTEXT(                                 \
                save_context_c->MR_ctxt_call_seqno = MR_trace_call_seqno;     \
                save_context_c->MR_ctxt_call_depth = MR_trace_call_depth;     \
                save_context_c->MR_ctxt_event_number = MR_trace_event_number; \
            )                                                                 \
        )                                                                     \
        MR_save_hp_in_context(save_context_c);                                \
    } while (0)

#define MR_copy_eng_this_context_fields(to_cptr, from_cptr)                   \
    do {                                                                      \
        /* It wouldn't be appropriate to copy the resume field. */            \
        to_cptr->MR_ctxt_thread_local_mutables =                              \
            from_cptr->MR_ctxt_thread_local_mutables;                         \
        /* It wouldn't be appropriate to copy the spark_deque field. */       \
        /* It wouldn't be appropriate to copy the saved_owners field. */      \
    } while (0)

////////////////////////////////////////////////////////////////////////////

#ifdef MR_LL_PARALLEL_CONJ

  // If you change MR_SyncTerm_Struct you need to update configure.ac.
  //
  // MR_st_count is manipulated via atomic operations, therefore it is declared
  // as volatile.

  struct MR_SyncTerm_Struct {
    MR_Context              *MR_st_orig_context;
    MR_Word                 *MR_st_parent_sp;
    volatile MR_Unsigned    MR_st_count;
  };

  MR_STATIC_ASSERT(mercury_context,
    MR_SYNC_TERM_SIZE == MR_bytes_to_words(sizeof(struct MR_SyncTerm_Struct)));

#ifdef MR_THREADSCOPE
  #define MR_init_sync_term(sync_term, nbranches, static_conj_id)             \
    do {                                                                      \
        MR_SyncTerm *init_st = (MR_SyncTerm *) &(sync_term);                  \
                                                                              \
        init_st->MR_st_orig_context = MR_ENGINE(MR_eng_this_context);         \
        init_st->MR_st_parent_sp = MR_parent_sp;                              \
        init_st->MR_st_count = (nbranches);                                   \
        MR_threadscope_post_start_par_conj(&(sync_term), static_conj_id);     \
    } while (0)
#else
  #define MR_init_sync_term(sync_term, nbranches, static_conj_id)             \
    do {                                                                      \
        MR_SyncTerm *init_st = (MR_SyncTerm *) &(sync_term);                  \
                                                                              \
        init_st->MR_st_orig_context = MR_ENGINE(MR_eng_this_context);         \
        init_st->MR_st_parent_sp = MR_parent_sp;                              \
        init_st->MR_st_count = (nbranches);                                   \
    } while (0)
#endif

  // fork_new_child(MR_SyncTerm st, MR_Code *child):
  //
  // Create a new spark to execute the code at `child'. The new spark is put
  // on the context's spark queue. The current context resumes at `parent'.
  // MR_parent_sp must already be set appropriately before this instruction
  // is executed.

#define MR_fork_new_child(sync_term, child)                                  \
do {                                                                         \
    MR_Spark            fnc_spark;                                           \
    MR_SparkDeque       *fnc_deque;                                          \
    MR_EngineId         engine_id = MR_ENGINE(MR_eng_id);                    \
    MR_IF_THREADSCOPE(                                                       \
        MR_uint_least32_t   id;                                              \
    )                                                                        \
                                                                             \
    fnc_spark.MR_spark_sync_term = (MR_SyncTerm*) &(sync_term);              \
    fnc_spark.MR_spark_resume = (child);                                     \
    fnc_spark.MR_spark_thread_local_mutables = MR_THREAD_LOCAL_MUTABLES;     \
    MR_IF_THREADSCOPE(                                                       \
        id = MR_ENGINE(MR_eng_next_spark_id)++;                              \
        fnc_spark.MR_spark_id = (engine_id << 24)|(id & 0xFFFFFF);           \
    )                                                                        \
    fnc_deque = MR_ENGINE(MR_eng_spark_deque);                               \
    MR_wsdeque_push_bottom(fnc_deque, &fnc_spark);                           \
    MR_IF_THREADSCOPE(                                                       \
        MR_threadscope_post_sparking(&(sync_term), fnc_spark.MR_spark_id);   \
    )                                                                        \
    MR_IF_NOT_WORKSTEAL_POLLING(                                             \
        if (MR_ENGINE(MR_eng_this_context)->MR_ctxt_exclusive_engine         \
            == MR_ENGINE_ID_NONE && MR_num_idle_ws_engines > 0)              \
        {                                                                    \
            union MR_engine_wake_action_data action_data;                    \
            action_data.MR_ewa_worksteal_engine = MR_ENGINE(MR_eng_id);      \
            MR_try_wake_ws_engine(MR_ENGINE(MR_eng_id),                      \
                MR_ENGINE_ACTION_WORKSTEAL_ADVICE,                           \
                &action_data, NULL);                                         \
        }                                                                    \
    )                                                                        \
} while (0)

  // This macro may be used as conditions for runtime parallelism decisions.
  // They return nonzero when parallelism is recommended (because there are
  // enough CPUs to assign work to).
  //
  // This test calculates the length of a wsdeque each time it is called.
  // The test will usually execute more often than the length of the
  // queue changes. Therefore, it makes sense to update a protected counter
  // each time a spark is pushed, popped or stolen from the queue. However I
  // believe that these atomic operations could be more expensive than
  // necessary.
  //
  // The current implementation computes the length of the queue each time this
  // macro is evaluated, this requires no atomic operations and contains only
  // one extra memory dereference whose cache line is probably already hot in
  // the first-level cache.

  #define MR_par_cond_local_wsdeque_length                                    \
      (MR_wsdeque_length(MR_ENGINE(MR_eng_spark_deque)) <                     \
        MR_granularity_wsdeque_length)

extern MR_Code*
MR_do_join_and_continue(MR_SyncTerm *sync_term, MR_Code *join_label);

  #define MR_join_and_continue(sync_term, join_label)                         \
    do {                                                                      \
        MR_Code     *jump_target;                                             \
        jump_target =                                                         \
            MR_do_join_and_continue((MR_SyncTerm*) &(sync_term), join_label); \
        MR_GOTO(jump_target);                                                 \
    } while (0)

  // This needs to come after the definition of MR_SparkDeque_Struct.
  #include "mercury_wsdeque.h"

// This structure and function can be used to wake up a sleeping engine,
// it is exported here for use by the MR_fork_new_child macro above.

#define MR_ENGINE_ACTION_NONE               0x0000
// ACTION_CONTEXT applies when an engine is being given a context directly

#define MR_ENGINE_ACTION_CONTEXT            0x0001
#define MR_ENGINE_ACTION_SHUTDOWN           0x0002
#define MR_ENGINE_ACTION_WORKSTEAL_ADVICE   0x0004
// ACTION_CONTEXT_ADVICE applies when a context is on the run queue that
// this engine should check.

#define MR_ENGINE_ACTION_CONTEXT_ADVICE     0x0008

union MR_engine_wake_action_data {
    // This is provided for workstealing actions, to let the engine know
    // where to look for work to steal.

    MR_EngineId     MR_ewa_worksteal_engine;

    // This is provided for context actions.

    MR_Context      *MR_ewa_context;
};

// Try to wake a sleeping work-stealing engine.
//
// preferred_engine - The engine we'd like to wake up, a nearby engine will
//                    often be chosen so it's okay to name the current engine
//                    in this field.
//
// action           - The action to run, see the macros above.
//
// action_data      - Extra data for the action, if not applicable pass NULL.
//
// target_engine    - If the call succeeds and this parameter is non-null, the
//                    ID of the engine that received this message is written to
//                    this address.
//
// This returns MR_TRUE if successful, MR_FALSE otherwise.

extern  MR_bool     MR_try_wake_ws_engine(MR_EngineId perferred_engine,
                        int action,
                        union MR_engine_wake_action_data *action_data,
                        MR_EngineId *target_engine);

extern  void        MR_verify_initial_engine_sleep_sync(MR_EngineId id);

extern  void        MR_verify_final_engine_sleep_sync(MR_EngineId id,
                        MR_EngineType engine_type);

#ifdef MR_DEBUG_RUNTIME_GRANULARITY_CONTROL

  // These functions can be used to debug the runtime granularity control
  // methods implemented above.

  // decision is 1 if we choose to parallelise something and 0 if code should
  // be run sequentially.
  // This is not (yet) thread safe.

  extern void       MR_record_conditional_parallelism_decision(
                        MR_Unsigned decision);

  // flush and close the log of conditional parallelism decisions
  // This is not thread safe.
  // This is a no-op if no parallelism decisions have been recorded.

  extern void       MR_write_out_conditional_parallelism_log(void);

#endif // MR_DEBUG_RUNTIME_GRANULARITY_CONTROL

#endif // MR_LL_PARALLEL_CONJ

#endif // not MERCURY_CONTEXT_H
