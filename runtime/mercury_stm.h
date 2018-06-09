// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2007-2009 The University of Melbourne.
// Copyright (C) 2015-2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_stm.h - runtime support for software transactional memory.

#ifndef MERCURY_STM_H
#define MERCURY_STM_H

#include "mercury_types.h"
#include "mercury_thread.h"
#include "mercury_conf.h"
#include "mercury_conf_param.h"
#include "mercury_context.h"
#include "mercury_engine.h"
#include <stdio.h>

typedef struct MR_STM_Waiter_Struct         MR_STM_Waiter;
typedef struct MR_STM_Var_Struct            MR_STM_Var;
typedef struct MR_STM_TransRecord_Struct    MR_STM_TransRecord;
typedef struct MR_STM_TransLog_Struct       MR_STM_TransLog;

// The type MR_ThreadId provides an abstract means of identifying a Mercury
// thread. Depending upon the grade we use one of three notions of thread
// identity.
//
// For high-level code with parallelism it is the value returned by a call
// to pthread_self().
//
// For high-level code without parallelism it is an MR_Integer - in this case
// concurrency is not supported so there is only ever one thread.
//
// For low-level code with use the context address as the thread id.
//
// The macro MR_THIS_THREAD_ID expands to a value of type MR_ThreadId.
// This value is the identity of the current thread.

#if defined(MR_HIGHLEVEL_CODE)

    #if defined(MR_THREAD_SAFE)
        typedef pthread_t       MR_ThreadId;

        #define MR_THIS_THREAD_ID pthread_self()
    #else
        typedef MR_Integer      MR_ThreadId;
        // Since these grades don't support concurrency there is only one
        // thread which we always give the id 0.

        #define MR_THIS_THREAD_ID 0
    #endif

#else // !MR_HIGHLEVEL_CODE

    typedef MR_Context      *MR_ThreadId;
    #define MR_THIS_THREAD_ID (MR_ENGINE(MR_eng_this_context))

#endif // !MR_HIGHLEVEL_CODE

// The type MR_STM_ConditionVar provides an abstract method of blocking and
// signalling threads based on conditions.

#if defined(MR_HIGHLEVEL_CODE)

    #if defined(MR_THREAD_SAFE)
        typedef MercuryCond  MR_STM_ConditionVar;

        #define MR_STM_condvar_init(x)        pthread_cond_init(x, MR_COND_ATTR)
        #define MR_STM_condvar_wait(x, y)     MR_cond_wait(x, y, "STM_condvar_wait")
        #define MR_STM_condvar_signal(x)      MR_cond_signal(x, "STM_condvar_signal")
    #else
        typedef MR_Integer      MR_STM_ConditionVar;
        // Since these grades don't support concurrency, there is no
        // need to block the thread.

        #define MR_STM_condvar_init(x)
        #define MR_STM_condvar_wait(x, y)
        #define MR_STM_condvar_signal(x)
    #endif

#else // !MR_HIGHLEVEL_CODE

    typedef MR_STM_TransLog  MR_STM_ConditionVar;

    // NOTE: The global STM lock (MR_STM_lock) must be held when calling this
    // function, as it manipulates the STM variable waiter lists.

    void MR_STM_condvar_signal(MR_STM_ConditionVar *cvar);

    #define MR_STM_context_from_condvar(x)      ((x)->MR_STM_tl_thread)

#endif // !MR_HIGHLEVEL_CODE

// A waiter is the identity of a thread that is blocking until the value
// of this transaction variable changes.

struct MR_STM_Waiter_Struct {
    MR_STM_ConditionVar *MR_STM_cond_var;
    MR_STM_Waiter       *MR_STM_waiter_next;
    MR_STM_Waiter       *MR_STM_waiter_prev;
};

// XXX This should also contain the type_info for the value, so we can
// print them out in the debugger.

struct MR_STM_Var_Struct {
    MR_Word         MR_STM_var_value;
    MR_STM_Waiter   *MR_STM_var_waiters;
};

struct MR_STM_TransRecord_Struct {
    MR_STM_Var          *MR_STM_tr_var;
    MR_Word             MR_STM_tr_old_value;
    MR_Word             MR_STM_tr_new_value;
    MR_STM_TransRecord  *MR_STM_tr_next;
};

struct MR_STM_TransLog_Struct {
    MR_STM_TransRecord  *MR_STM_tl_records;
    MR_ThreadId         MR_STM_tl_thread;
    MR_STM_TransLog     *MR_STM_tl_parent;
};

// The global STM lock. This lock must be acquired before validating or
// committing a transaction log.

#if defined(MR_THREAD_SAFE)
    extern MercuryLock  MR_STM_lock;
#endif

// Allocate a new transaction variable.

#define MR_STM_new_stm_var(value, var)                                  \
    do {                                                                \
        (var) = MR_GC_NEW_ATTRIB(MR_STM_Var, MR_ALLOC_SITE_STM);        \
        (var)->MR_STM_var_value = (value);                              \
        (var)->MR_STM_var_waiters = NULL;                               \
    } while (0)

// Create a new transaction log.
// If the log is for a nested transaction then the `parent' field points
// to the log of the enclosing transaction. It is NULL otherwise.

#define MR_STM_create_log(tlog, parent)                                 \
    do {                                                                \
        (tlog) = MR_GC_NEW_ATTRIB(MR_STM_TransLog, MR_ALLOC_SITE_STM);  \
        (tlog)->MR_STM_tl_records = NULL;                               \
        (tlog)->MR_STM_tl_thread = MR_THIS_THREAD_ID;                   \
        (tlog)->MR_STM_tl_parent = (parent);                            \
    } while (0)

// Discard a transaction log.
// XXX we should free the memory in nogc grades.

#define MR_STM_discard_log(tlog)                                        \
    do {                                                                \
        (tlog) = NULL;                                                  \
    } while (0)

// Record a change of state for transaction variable `var' in the
// given transaction log. `old_value' and `new_value' give the value
// of the transaction variable before and after the change of state.

extern  void        MR_STM_record_transaction(MR_STM_TransLog *tlog,
                        MR_STM_Var *var,
                        MR_Word old_value, MR_Word new_value);

// Add a waiter for the current thread to all of the transaction variables
// listed in the log.

extern  void        MR_STM_wait(MR_STM_TransLog *tlog,
                        MR_STM_ConditionVar *cvar);

// Detach waiters for the current thread from all of the transaction variables
// referenced by the given transaction log.

extern  void        MR_STM_unwait(MR_STM_TransLog *tlog,
                        MR_STM_ConditionVar *cvar);

// Attach a waiter for thread tid to the transaction variable. The condition
// variable should be a condition variable properly initialised and associated
// with the thread.

extern  void        MR_STM_attach_waiter(MR_STM_Var *var, MR_ThreadId tid,
                        MR_STM_ConditionVar *cvar);

// Detach any waiters for thread tid from the transaction variable.
// This will cause execution to abort if no waiter for thread tid can
// be found since it can only correctly be called in a situation where
// such a waiter exists.

extern  void        MR_STM_detach_waiter(MR_STM_Var *var,
                        MR_STM_ConditionVar *cvar);

extern  MR_Integer  MR_STM_validate(MR_STM_TransLog *tlog);

// Irrevocably write the changes stored in a transaction log to memory.

extern  void        MR_STM_commit(MR_STM_TransLog *tlog);

// Changes the value of transaction variable var in a transaction log.

extern  void        MR_STM_write_var(MR_STM_Var *var, MR_Word value,
                        MR_STM_TransLog *tlog);

// Returns the value of transaction variable var in a transaction log.
// If no entry for var exists, the actual value of the transaction variable
// var is returned (and added to the transaction log).

extern  MR_Word     MR_STM_read_var(MR_STM_Var *var, MR_STM_TransLog *tlog);

// Changes the value of the transaction variable var without going through
// the log.
//
// NOTE: This functions must only be used for debugging purposes and will
// eventually be removed. Please, DO NOT use it for normal operations.

extern  void        MR_STM_unsafe_write_var(MR_STM_Var *var, MR_Word value);

// Blocks a thread from execution. This method is called by the thread
// which is to be blocked. The STM lock MUST be acquired by the thread
// before this method is called and acquires the lock when the thread
// is signalled.

extern  void        MR_STM_block_thread(MR_STM_TransLog *tlog);

// Merges a transaction log with its parent. Do not merge it with any
// other ancestors. Aborts if the given transaction log does not have a
// parent.

extern  void        MR_STM_merge_transactions(MR_STM_TransLog *tlog);

// Reschedules all threads currently waiting on the given transaction
// variables.

extern  void        MR_STM_signal_vars(MR_STM_Var *tvar);

// These definitions need to be kept in sync with the definition of the type
// stm_validation_result/0 in library/stm_builtin.m. Changes here may need
// be reflected there.

#define MR_STM_TRANSACTION_VALID 0
#define MR_STM_TRANSACTION_INVALID 1

#endif // not MERCURY_STM_H
