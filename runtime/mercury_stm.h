/*
** vim:ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2007 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_stm.h - runtime support for software transactional memory.
*/

#ifndef MERCURY_STM_H
#define MERCURY_STM_H

#include "mercury_types.h"
#include "mercury_thread.h"
#include "mercury_conf.h"
#include "mercury_context.h"
#include "mercury_engine.h"

typedef struct MR_STM_Waiter_Struct         MR_STM_Waiter;
typedef struct MR_STM_Var_Struct            MR_STM_Var;
typedef struct MR_STM_TransRecord_Struct    MR_STM_TransRecord;
typedef struct MR_STM_TransLog_Struct       MR_STM_TransLog;

/*
** The type MR_ThreadId provides an abstract means of identifying a Mercury
** thread.  Depending upon the grade we use one of three notions of thread
** identity.
**
** For high-level code with parallelism it is the value returned by a call
** to pthread_self().
**
** For high-level code without parallelism it is an MR_Integer - in this case
** concurrency is not supported so there is only ever one thread.
**
** For low-level code with use the context address as the thread id.
**
** The macro MR_THIS_THREAD_ID expands to a value of type MR_ThreadId.
** This value is the identity of the current thread.
*/
#if defined(MR_HIGHLEVEL_CODE)

    #if defined(MR_THREAD_SAFE)
        typedef pthread_t   MR_ThreadId;
        #define MR_THIS_THREAD_ID pthread_self()
    #else
        typedef MR_Integer  MR_ThreadId;
        /*
        ** Since these grades don't support concurrency there is only one
        ** thread which we always give the id 0.
        */
        #define MR_THIS_THREAD_ID 0
    #endif

#else /* !MR_HIGHLEVEL_CODE */

    typedef MR_Context  *MR_ThreadId;
    #define MR_THIS_THREAD_ID (MR_ENGINE(MR_eng_this_context))

#endif /* !MR_HIGHLEVEL_CODE */

/*
** A waiter is the identity of a thread that is blocking until the value
** of this transaction variable changes.
*/
struct MR_STM_Waiter_Struct {
    MR_ThreadId     MR_STM_waiter_thread;
    MR_STM_Waiter   *MR_STM_waiter_next;
};

/*
** XXX this should also contain the type_info for the value, so we can
** print them out in the debugger.
*/
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
};

/*
** Allocate a new transaction variable.
*/
#define MR_STM_new_stm_var(value, var)                                  \
    do {                                                                \
        (var) = MR_GC_NEW(MR_STM_Var) ;                                 \
        (var)->MR_STM_var_value = (value);                              \
        (var)->MR_STM_var_waiters = NULL;                               \
    } while (0)

/*
** Create a new transaction log.
*/
#define MR_STM_create_log(log)                                          \
    do {                                                                \
        (log) = MR_GC_NEW(MR_STM_TransLog);                             \
        (log)->MR_STM_tl_records = NULL;                                \
        (log)->MR_STM_tl_thread = MR_THIS_THREAD_ID;                    \
    } while (0)

/*
** Discard a transaction log.
** XXX we should free the memory in nogc grades.
*/
#define MR_STM_discard_log(log)                                         \
    do {                                                                \
        (log) = NULL;                                                   \
    } while (0)

/*
** Record a change of state for transaction variable `var' in the
** given transaction log.  `old_value' and `new_value' give the value
** of the transaction variable before and after the change of state.
*/
extern void
MR_STM_record_transaction(MR_STM_TransLog *log, MR_STM_Var *var,
    MR_Word old_value, MR_Word new_value);

/*
** Add a waiter for the current thread to all of the transaction variables
** listed in the log.
*/
extern void
MR_STM_wait(MR_STM_TransLog *log);

/*
** Detach waiters for the current thread from all of the transaction variables
** referenced by the given transaction log.
*/
extern void
MR_STM_unwait(MR_STM_TransLog *log);

/*
** Attach a waiter for thread tid to the transaction variable.
*/
extern void
MR_STM_attach_waiter(MR_STM_Var *var, MR_ThreadId tid);

/*
** Detach any waiters for thread tid from the transaction variable.
** This will cause execution to abort if no waiter for thread tid can
** be found since it can only correctly be called in a situation where
** such a waiter exists.
*/
extern void
MR_STM_detach_waiter(MR_STM_Var *var, MR_ThreadId tid);

extern MR_Integer
MR_STM_validate(MR_STM_TransLog *log);

/*
** Irrevocably write the changes stored in a transaction log to memory.
*/
extern void
MR_STM_commit(MR_STM_TransLog *log);

extern void
MR_STM_write_var(MR_STM_Var *var, MR_Word value, MR_STM_TransLog *log);

extern MR_Word
MR_STM_read_var(MR_STM_Var *var, MR_STM_TransLog *log);

extern void
MR_STM_retry_impl(MR_STM_TransLog *log);

#if defined(MR_THREAD_SAFE)
    extern MercuryLock  MR_STM_lock;
#endif

/*
** These definitions need to be kept in sync with the definition of the type
** stm_validation_result/0 in library/stm_builtin.m.  Changes here may need
** be reflected there.
*/
#define MR_STM_TRANSACTION_VALID 0
#define MR_STM_TRANSACTION_INVALID 1

#endif /* not MERCURY_STM_H */
