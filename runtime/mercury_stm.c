// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2007-2009 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_stm.c - runtime support for software transactional memory.

#include "mercury_std.h"
#include "mercury_stm.h"
#include "mercury_memory.h"
#include "mercury_misc.h"

#if defined(MR_THREAD_SAFE)
MercuryLock MR_STM_lock;
#endif

void
MR_STM_record_transaction(MR_STM_TransLog *tlog, MR_STM_Var *var,
    MR_Word old_value, MR_Word new_value)
{
    MR_STM_TransRecord  *new_record;

    new_record = MR_GC_NEW_ATTRIB(MR_STM_TransRecord,
        MR_ALLOC_SITE_RUNTIME);
    new_record->MR_STM_tr_var = var;
    new_record->MR_STM_tr_old_value = old_value;
    new_record->MR_STM_tr_new_value = new_value;
    new_record->MR_STM_tr_next = tlog->MR_STM_tl_records;
    tlog->MR_STM_tl_records = new_record;
}

void
MR_STM_attach_waiter(MR_STM_Var *var, MR_ThreadId tid,
    MR_STM_ConditionVar *cvar)
{
    MR_STM_Waiter   *new_waiter;

    new_waiter = MR_GC_NEW_ATTRIB(MR_STM_Waiter, MR_ALLOC_SITE_RUNTIME);
    new_waiter->MR_STM_cond_var = cvar;

    if (var->MR_STM_var_waiters == NULL) {
        var->MR_STM_var_waiters = new_waiter;
        new_waiter->MR_STM_waiter_prev = NULL;
        new_waiter->MR_STM_waiter_next = NULL;
    } else {
        new_waiter->MR_STM_waiter_prev = NULL;
        new_waiter->MR_STM_waiter_next = var->MR_STM_var_waiters;
        var->MR_STM_var_waiters->MR_STM_waiter_prev = new_waiter;
        var->MR_STM_var_waiters = new_waiter;
    }
}

void
MR_STM_detach_waiter(MR_STM_Var *var, MR_STM_ConditionVar *cvar)
{
    MR_STM_Waiter   *curr_waiter;

    MR_assert(var != NULL);
    MR_assert(var->MR_STM_var_waiters != NULL);

    curr_waiter = var->MR_STM_var_waiters;
    while (curr_waiter != NULL) {
        if (curr_waiter->MR_STM_cond_var == cvar) {
            if (curr_waiter == var->MR_STM_var_waiters) {
                var->MR_STM_var_waiters =
                        var->MR_STM_var_waiters->MR_STM_waiter_next;
            }
            if (curr_waiter->MR_STM_waiter_prev != NULL) {
                curr_waiter->MR_STM_waiter_prev->MR_STM_waiter_next =
                        curr_waiter->MR_STM_waiter_next;
            }
            if (curr_waiter->MR_STM_waiter_next != NULL) {
                curr_waiter->MR_STM_waiter_next->MR_STM_waiter_prev =
                        curr_waiter->MR_STM_waiter_prev;
            }
            curr_waiter = NULL;
            return;
        }
        curr_waiter = curr_waiter->MR_STM_waiter_next;
    }

    MR_fatal_error("MR_STM_detach_waiter: Thread ID not in wait queue");
}

MR_Integer
MR_STM_validate(MR_STM_TransLog *tlog)
{
    MR_STM_TransRecord  *current;

    MR_assert(tlog != NULL);

#if defined(MR_STM_DEBUG)
    fprintf(stderr, "STM VALIDATE: validating log <0x%.8lx>\n",
        (MR_Word) tlog);
    fprintf(stderr, "\tRecords: <0x%.8lx>\n",
        (MR_Word) tlog->MR_STM_tl_records);
#endif

    while (tlog != NULL) {
        current = tlog->MR_STM_tl_records;

        while (current != NULL) {
            if (current->MR_STM_tr_var->MR_STM_var_value !=
                current->MR_STM_tr_old_value)
            {
#if defined(MR_STM_DEBUG)
                fprintf(stderr, "\ttransaction INVALID.\n");
#endif
                return MR_STM_TRANSACTION_INVALID;
            }

            current = current->MR_STM_tr_next;
        }

        tlog = tlog->MR_STM_tl_parent;
    }

#if defined(MR_STM_DEBUG)
    fprintf(stderr, "\ttransaction VALID.\n");
#endif

    return MR_STM_TRANSACTION_VALID;
}

void
MR_STM_signal_vars(MR_STM_Var *tvar)
{
    MR_STM_Waiter   *wait_queue;

    wait_queue = tvar->MR_STM_var_waiters;

    while (wait_queue != NULL) {
#if defined(MR_STM_DEBUG)
        fprintf(stderr, "STM SIGNAL: signalling log <0x%.8lx>\n",
            (MR_Word) wait_queue->MR_STM_cond_var);
#endif
        MR_STM_condvar_signal(wait_queue->MR_STM_cond_var);
        wait_queue = wait_queue->MR_STM_waiter_next;
    }
}

void
MR_STM_commit(MR_STM_TransLog *tlog)
{
    MR_STM_TransRecord  *current;

#if defined(MR_STM_DEBUG)
    fprintf(stderr, "STM COMMIT: committing log <0x%.8lx>\n",
        (MR_Word) tlog);
#endif

    current = tlog->MR_STM_tl_records;
    while (current != NULL) {
#if defined(MR_STM_DEBUG)
        fprintf(stderr,
            "\tSTM_Var <%.8lx>, changing value from %ld to %ld\n",
            (MR_Word) current->MR_STM_tr_var,
            current->MR_STM_tr_var->MR_STM_var_value,
            current->MR_STM_tr_new_value);
#endif
        current->MR_STM_tr_var->MR_STM_var_value =
            current->MR_STM_tr_new_value;

        MR_STM_signal_vars(current->MR_STM_tr_var);
        current = current->MR_STM_tr_next;
    }
}

void
MR_STM_wait(MR_STM_TransLog *tlog, MR_STM_ConditionVar *cvar)
{
    MR_STM_TransRecord  *current;
    MR_ThreadId         this_thread_id;

    this_thread_id = MR_THIS_THREAD_ID;

    current = tlog->MR_STM_tl_records;
    while (current != NULL) {
#if defined(MR_STM_DEBUG)
        fprintf(stderr, "STM WAIT: attaching waiter on log <0x%.8lx>\n",
            (MR_Word) tlog);
        fprintf(stderr, "\tSTM_Var: <0x%.8lx>\n",
            (MR_Word) current->MR_STM_tr_var);
#endif

        MR_STM_attach_waiter(current->MR_STM_tr_var, this_thread_id, cvar);
        current = current->MR_STM_tr_next;
    }
}

void
MR_STM_unwait(MR_STM_TransLog *tlog, MR_STM_ConditionVar *cvar)
{
    MR_STM_TransRecord  *current;
    MR_ThreadId         this_thread_id;

    this_thread_id = MR_THIS_THREAD_ID;
    current = tlog->MR_STM_tl_records;

    while (current != NULL) {
#if defined(MR_STM_DEBUG)
        fprintf(stderr, "STM UNWAIT: detaching waiter on log <0x%.8lx>\n",
            (MR_Word) tlog);
        fprintf(stderr, "\tSTM_Var: <0x%.8lx>\n",
            (MR_Word) current->MR_STM_tr_var);
#endif

        MR_STM_detach_waiter(current->MR_STM_tr_var, cvar);
        current = current->MR_STM_tr_next;
    }
}

void
MR_STM_unsafe_write_var(MR_STM_Var *var, MR_Word value)
{
#if defined(MR_STM_DEBUG)
    fprintf(stderr, "UNSAFE_WRITE_VAR:\n");
    fprintf(stderr, "\tSTM_Var <%.8lx>, changing value from %ld to %ld\n",
        (MR_Word) var, var->MR_STM_var_value, value);
#endif

    var->MR_STM_var_value = value;
}

void
MR_STM_write_var(MR_STM_Var *var, MR_Word value, MR_STM_TransLog *tlog)
{

    MR_STM_TransRecord  *current;
    MR_bool             has_existing_record = MR_FALSE;

    // Check to see if this transaction variable has an existing record in
    // transaction log; if so, update it.

    current = tlog->MR_STM_tl_records;
    while (current != NULL) {
        if (current->MR_STM_tr_var == var) {
            has_existing_record = MR_TRUE;
            current->MR_STM_tr_new_value = value;
            break;
        }

        current = current->MR_STM_tr_next;
    }

    // Add a new entry for the transaction variable if didn't already have one.

    if (!has_existing_record) {
        MR_STM_record_transaction(tlog, var, var->MR_STM_var_value, value);
    }
}

MR_Word
MR_STM_read_var(MR_STM_Var *var, MR_STM_TransLog *tlog)
{
    MR_STM_TransLog     *current_tlog;
    MR_STM_TransRecord  *current;

    current_tlog = tlog;

#if defined(MR_STM_DEBUG)
    fprintf(stderr, "STM Read: Log <%.8lx> -- var <%.8lx>\n",
        (MR_Word) tlog, (MR_Word) var);
#endif

    while (current_tlog != NULL) {
        current = current_tlog->MR_STM_tl_records;
        while (current != NULL) {
            if (current->MR_STM_tr_var == var) {
                return current->MR_STM_tr_new_value;
            }

            current = current->MR_STM_tr_next;
        }

        current_tlog = current_tlog->MR_STM_tl_parent;
    }

    // We will only get to this point if the transaction variable does not
    // currently have a record in the log or one of the enclosing logs,
    // i.e. if this is the first time that its value has been read during
    // this atomic scope.
    // Add an entry that indicates that it has been read and then return
    // the value that is stored in the transaction variable.

    MR_STM_record_transaction(tlog, var, var->MR_STM_var_value,
        var->MR_STM_var_value);

    return var->MR_STM_var_value;
}

void
MR_STM_merge_transactions(MR_STM_TransLog *tlog)
{
    MR_STM_TransLog     *parent_log;
    MR_STM_TransRecord  *parent_current;
    MR_STM_TransRecord  *current;
    MR_bool             found_tvar_in_parent;

    MR_assert(tlog != NULL);
    MR_assert(tlog->MR_STM_tl_parent != NULL);

    parent_log = tlog->MR_STM_tl_parent;

    current = tlog->MR_STM_tl_records;
    while (current != NULL) {
        found_tvar_in_parent = MR_NO;
        parent_current = parent_log->MR_STM_tl_records;

        while (parent_current != NULL) {
            if (current->MR_STM_tr_var == parent_current->MR_STM_tr_var) {
                parent_current->MR_STM_tr_new_value =
                    current->MR_STM_tr_new_value;
                found_tvar_in_parent = MR_YES;
                break;
            }

            parent_current = parent_current->MR_STM_tr_next;
        }

        if (! found_tvar_in_parent) {
            MR_STM_record_transaction(parent_log,
                current->MR_STM_tr_var, current->MR_STM_tr_old_value,
                current->MR_STM_tr_new_value);
        }

        current = current->MR_STM_tr_next;
    }

#if defined(MR_STM_DEBUG)
    fprintf(stderr, "STM: Merging log end: <0x%.8lx>\n",
        (MR_Word) tlog);
#endif

    // Deallocate child log.
#if !defined(MR_CONSERVATIVE_GC)
    // XXX -- Free tlog and log entries.
#endif
}

#if defined(MR_HIGHLEVEL_CODE)
// MR_STM_block_thread is called to block the thread in high level C grades,
// using POSIX thread facilities, as there is a POSIX thread for every
// Mercury thread in these grades. The low level C grade equivalent of this
// code is defined in the stm_builtin library module.

void
MR_STM_block_thread(MR_STM_TransLog *tlog)
{
#if defined(MR_THREAD_SAFE)
        MR_STM_ConditionVar     *thread_condvar;

        thread_condvar = MR_GC_NEW_ATTRIB(MR_STM_ConditionVar,
            MR_ALLOC_SITE_RUNTIME);
        MR_STM_condvar_init(thread_condvar);

        MR_STM_wait(tlog, thread_condvar);

#if defined(MR_STM_DEBUG)
        fprintf(stderr, "STM BLOCKING: log <0x%.8lx>\n", (MR_Word)tlog);
#endif
        MR_STM_condvar_wait(thread_condvar, &MR_STM_lock);

#if defined(MR_STM_DEBUG)
        fprintf(stderr, "STM RESCHEDULING: log <0x%.8lx>\n", (MR_Word)tlog);
#endif
        MR_STM_unwait(tlog, thread_condvar);

        MR_UNLOCK(&MR_STM_lock, "MR_STM_block_thread");

        MR_GC_free_attrib(thread_condvar);
#else
    MR_fatal_error("Blocking thread in non-parallel grade");
#endif
}
#endif  // MR_HIGHLEVEL_CODE

#if !defined(MR_HIGHLEVEL_CODE)
// In the low level C grades, the "condition variable" created when an STM
// transaction blocks is actually a pointer to the transaction log.
// "Signalling" it consists of going through the STM variables listed in the
// log and removing the waiters attached to them for the context listed
// in the log. After this, the context can be safely rescheduled.

void
MR_STM_condvar_signal(MR_STM_ConditionVar *cvar)
{
    // Calling MR_STM_unwait here should be safe, as this signalling is called
    // in response to a commit, while the committing thread holds the global
    // STM lock. Note that a MR_STM_ConditionVar IS a MR_STM_TransLog if
    // MR_HIGHLEVEL_CODE is not defined, which is why cvar is passed twice.

    MR_STM_unwait(cvar, cvar);

#if defined(MR_STM_DEBUG)
        fprintf(stderr, "STM RESCHEDULING: log <0x%.8lx>\n", (MR_Word)cvar);
#endif

    MR_schedule_context(MR_STM_context_from_condvar(cvar));
}

#endif  // !MR_HIGHLEVEL_CODE
