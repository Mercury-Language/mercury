// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2009-2011 The University of Melbourne.
// Copyright (C) 2015-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_threadscope.h - defines Mercury threadscope profiling support.
//
// See "Parallel Performance Tuning for Haskell" - Don Jones Jr, Simon Marlow
// and Satnam Singh for information about threadscope.

#ifndef MERCURY_THREADSCOPE_H
#define MERCURY_THREADSCOPE_H

#include "mercury_types.h"      // for MR_Word, MR_Code, etc
#include "mercury_engine.h"
#include "mercury_context.h"

#ifdef MR_THREADSCOPE

// Reasons why a context has been stopped, not all of these apply to Mercury,
// for instance contexts don't yield.

#define MR_TS_STOP_REASON_HEAP_OVERFLOW     1
#define MR_TS_STOP_REASON_STACK_OVERFLOW    2
#define MR_TS_STOP_REASON_YIELDING          3
#define MR_TS_STOP_REASON_BLOCKED           4
#define MR_TS_STOP_REASON_FINISHED          5

typedef struct MR_threadscope_event_buffer MR_threadscope_event_buffer_t;

typedef MR_uint_least16_t   MR_ContextStopReason;
typedef MR_Integer          MR_ContextId;
typedef MR_uint_least32_t   MR_TS_StringId;
typedef MR_uint_least32_t   MR_SparkId;
typedef MR_uint_least32_t   MR_EngSetId;
typedef MR_uint_least16_t   MR_EngSetType;
typedef MR_uint_least32_t   MR_TS_Pid;

typedef struct MR_Threadscope_String {
    const char*     MR_tsstring_string;
    MR_TS_StringId  MR_tsstring_id;
} MR_Threadscope_String;

// Set this to true to use the CPU's time stamp counter.
//
// This is initially set in mercury_wrapper.c and may be reset by
// MR_setup_threadscope if the TSC can't be used.

extern MR_bool  MR_threadscope_use_tsc;

// This must be called by the primordial thread before starting any other
// threads but after the primordial thread has been pinned.

extern void     MR_setup_threadscope(void);

extern void     MR_finalize_threadscope(void);

extern void     MR_threadscope_setup_engine(MercuryEngine *eng);

extern void     MR_threadscope_finalize_engine(MercuryEngine *eng);

#if 0
// It looks like we don't need TSC synchronization code on modern x86(-64) CPUs
// including multi-socket systems (tested on goliath and taura). If we find
// systems where this is needed we can enable it via a runtime check.

// Synchronize a slave thread's TSC offset to the master's. The master thread
// (with an engine) should call MR_threadscope_sync_tsc_master() for each slave
// while each slave (with an engine) calls MR_threadscope_sync_tsc_slave().
// All master - slave pairs must be pinned to CPUs and setup their threadscope
// structures already (by calling MR_threadscope_setup_engine() above).
// Multiple slaves may call the _slave at the same time, a lock is used to
// synchronize only one at a time. Only the primordial thread may call
// MR_threadscope_sync_tsc_master().

extern void MR_threadscope_sync_tsc_master(void);
extern void MR_threadscope_sync_tsc_slave(void);
#endif

// Use the following functions to post messages. All messages will read the
// current engine's ID from the engine word, some messages will also read the
// current context id from the context loaded into the current engine.

// This context has been created,  The context must be passed as a parameter so
// that it doesn't have to be the current context.
//
// Using the MR_Context typedef here requires the inclusion of
// mercury_context.h, creating a circular dependency

extern void     MR_threadscope_post_create_context(
                    struct MR_Context_Struct *context);

// The given context was created in order to execute a spark. This event
// should be posted in addition to (and after) create_thread above.

extern void     MR_threadscope_post_create_context_for_spark(
                    struct MR_Context_Struct *ctxt);

// This context is being released (back into a pool of free contexts). We may
// see a new create_context or create_context_for_spark message with the same
// context ID, such a message indicates that the context is being re-used.

extern void     MR_threadscope_post_release_context(
                    struct MR_Context_Struct *context);

// This context is being reused (after being released). This event is an
// alternative to create_context above.

extern void     MR_threadscope_post_reuse_context(
                    struct MR_Context_Struct *context, MR_Unsigned old_id);

// This message says the context is now ready to run. Such as its being
// placed on the run queue after being blocked

extern void     MR_threadscope_post_context_runnable(
                    struct MR_Context_Struct *context);

// This message says we are now running the current context

extern void     MR_threadscope_post_run_context(void);

// This message says we have stopped executing the current context,
// a reason why should be provided.

extern void     MR_threadscope_post_stop_context(MR_ContextStopReason reason);

// This message says we are about to execute a spark from our local stack.

extern void     MR_threadscope_post_run_spark(MR_SparkId spark_id);

// This message says that we are about to execute a spark that was stolen from
// another's stack.

extern void     MR_threadscope_post_steal_spark(MR_SparkId spark_id);

// This message says that a spark is being created for the given computation.
// The spark's ID is given as an argument.

extern void     MR_threadscope_post_sparking(MR_Word* dynamic_conj_id,
                    MR_SparkId spark_id);

// Post this message just before invoking the main/2 predicate.

extern void     MR_threadscope_post_calling_main(void);

// Post this message when an engine begins looking for a context to run.

extern void     MR_threadscope_post_looking_for_global_context(void);

// Post this message when an engine begins trying to run a spark from its
// local stack.

extern void     MR_threadscope_post_looking_for_local_spark(void);

// Post this message when a thread is about to attempt work stealing.

extern void     MR_threadscope_post_work_stealing(void);

// Post this message before a parallel conjunction starts.

extern void     MR_threadscope_post_start_par_conj(MR_Word* dynamic_id,
                    MR_TS_StringId static_id);

// Post this message after a parallel conjunction stops.

extern void     MR_threadscope_post_end_par_conj(MR_Word* dynamic_id);

// Post this message when a parallel conjunct calls the barrier code.

extern void     MR_threadscope_post_end_par_conjunct(MR_Word* dynamic_id);

// Post this message when a future is created, this establishes the conjunction
// id to future id mapping. The conjunction id is inferred by context.
// The name of the future within the conjunction is given by 'name'.

extern void     MR_threadscope_post_new_future(MR_Future* future_id,
                    MR_TS_StringId name);

// Post either of these messages when waiting on a future. The first if the
// context had to be suspended because the future was not available, and the
// second when the context did not need to be suspended.

extern void     MR_threadscope_post_wait_future_nosuspend(
                    MR_Future* future_id);
extern void     MR_threadscope_post_wait_future_suspended(
                    MR_Future* future_id);

// Post this event when signaling the production of a future.

extern void     MR_threadscope_post_signal_future(MR_Future* future_id);

// Post this event when the engine is going to sleep.

extern void     MR_threadscope_post_engine_sleeping(void);

// Register all the strings in an array and save their IDs in the array.

extern void     MR_threadscope_register_strings_array(
                    MR_Threadscope_String *array, unsigned size);

// Post a user-defined log message.

extern void     MR_threadscope_post_log_msg(const char *message);

#endif // MR_THREADSCOPE

#endif // not MERCURY_THREADSCOPE_H
