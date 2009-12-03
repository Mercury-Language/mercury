/*
** vim:ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2009 The University of Melbourne.
**
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_threadscope.h - defines Mercury threadscope profiling support.
**
** See "Parallel Preformance Tuning for Haskell" - Don Jones Jr, Simon Marlow
** and Satnam Singh for information about threadscope.
*/

#ifndef MERCURY_THREADSCOPE_H
#define MERCURY_THREADSCOPE_H

#include "mercury_types.h"      /* for MR_Word, MR_Code, etc */
#include "mercury_engine.h"
#include "mercury_context.h"

#if defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ) && \
        defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT)

/*
** Reasons why a context has been stopped, not all of these apply to Mercury,
** for instance contexts don't yield.
*/
#define MR_TS_STOP_REASON_HEAP_OVERFLOW     1
#define MR_TS_STOP_REASON_STACK_OVERFLOW    2
#define MR_TS_STOP_REASON_YIELDING          3
#define MR_TS_STOP_REASON_BLOCKED           4
#define MR_TS_STOP_REASON_FINISHED          5

typedef struct MR_threadscope_event_buffer MR_threadscope_event_buffer_t;

typedef MR_uint_least16_t   MR_EngineId;
typedef MR_uint_least16_t   MR_ContextStopReason;
typedef MR_uint_least32_t   MR_ContextId;

/*
** This must be called by the primordial thread before starting any other
** threads but after the primordial thread has been pinned.
*/
extern void
MR_setup_threadscope(void);

extern void
MR_finalize_threadscope(void);

extern void
MR_threadscope_setup_engine(MercuryEngine *eng);

extern void
MR_threadscope_finalize_engine(MercuryEngine *eng);

#if 0
/*
** It looks like we don't need TSC synchronization code on modern x86(-64) CPUs
** including multi-socket systems (tested on goliath and taura).  If we find
** systems where this is needed we can enable it via a runtime check.
*/
/*
** Synchronize a slave thread's TSC offset to the master's.  The master thread
** (with an engine) should call MR_threadscope_sync_tsc_master() for each slave
** while each slave (with an engine) calls MR_threadscope_sync_tsc_slave().
** All master - slave pairs must be pinned to CPUs and setup their threadscope
** structures already (by calling MR_threadscope_setup_engine() above).
** Multiple slaves may call the _slave at the same time, a lock is used to
** synchronize only one at a time.  Only the primordial thread may call
** MR_threadscope_sync_tsc_master().
*/
extern void
MR_threadscope_sync_tsc_master(void);
extern void
MR_threadscope_sync_tsc_slave(void);
#endif

/*
** Use the following functions to post messages.  All messages will read the
** current engine's ID from the engine word, some messages will also read the
** current context id from the context loaded into the current engine.
*/

/*
** This context has been created,  The context must be passed as a parameter so
** that it doesn't have to be the current context.
**
** Using the MR_Context typedef here requires the inclusion of
** mercury_context.h, creating a circular dependency
*/
extern void
MR_threadscope_post_create_context(struct MR_Context_Struct *context);

/*
** The given context was created in order to execute a spark.  It's an
** alternative to the above event.
*/
extern void
MR_threadscope_post_create_context_for_spark(struct MR_Context_Struct *ctxt);

/*
** This message says the context is now ready to run.  Such as it's being
** placed on the run queue after being blocked
*/
extern void
MR_threadscope_post_context_runnable(struct MR_Context_Struct *context);

/*
** This message says we're now running the current context
*/
extern void
MR_threadscope_post_run_context(void);

/*
** This message says we've stopped executing the current context,
** a reason why should be provided.
*/
extern void
MR_threadscope_post_stop_context(MR_ContextStopReason reason);

/*
** Post this message just before invoking the main/2 predicate.
*/
extern void
MR_threadscope_post_calling_main(void);

#endif /* defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ) && \
        defined(MR_PROFILE_PARALLEL_EXECUTION_SUPPORT) */

#endif /* not MERCURY_THREADSCOPE_H */
