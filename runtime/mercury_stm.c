/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2007 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_stm.c - runtime support for software transactional memory. */

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

    new_record = MR_GC_NEW(MR_STM_TransRecord);
    new_record->MR_STM_tr_var = var;
    new_record->MR_STM_tr_old_value = old_value;
    new_record->MR_STM_tr_new_value = new_value;
    new_record->MR_STM_tr_next = tlog->MR_STM_tl_records;
    tlog->MR_STM_tl_records = new_record;
}

void
MR_STM_attach_waiter(MR_STM_Var *var, MR_ThreadId tid)
{
    MR_fatal_error("NYI MR_STM_attach_waiter");
}

void
MR_STM_detach_waiter(MR_STM_Var *var, MR_ThreadId tid)
{
    MR_fatal_error("NYI MR_STM_detach_waiter");
}

MR_Integer
MR_STM_validate(MR_STM_TransLog *tlog)
{
    MR_STM_TransRecord  *current;

    current = tlog->MR_STM_tl_records;
    while (current != NULL) {
        if (current->MR_STM_tr_var->MR_STM_var_value !=
            current->MR_STM_tr_old_value)
        {
            return MR_STM_TRANSACTION_INVALID;
        }
        current = current->MR_STM_tr_next;
    }

    return MR_STM_TRANSACTION_VALID;
}

void
MR_STM_commit(MR_STM_TransLog *tlog) {

    MR_STM_TransRecord  *current;

    current = tlog->MR_STM_tl_records;
    while (current != NULL) {
        current->MR_STM_tr_var->MR_STM_var_value
            = current->MR_STM_tr_new_value;
        current = current->MR_STM_tr_next;
    }
}

void
MR_STM_wait(MR_STM_TransLog *tlog)
{
    MR_STM_TransRecord  *current;
    MR_ThreadId         this_thread_id;

    this_thread_id = MR_THIS_THREAD_ID;

    current = tlog->MR_STM_tl_records;
    while (current != NULL) {
        MR_STM_attach_waiter(current->MR_STM_tr_var, this_thread_id);
        current = current->MR_STM_tr_next;
    }
}

void
MR_STM_unwait(MR_STM_TransLog *tlog)
{
    MR_STM_TransRecord  *current;
    MR_ThreadId         this_thread_id;

    this_thread_id = MR_THIS_THREAD_ID;
    current = tlog->MR_STM_tl_records;
    
    while (current != NULL) {
        MR_STM_detach_waiter(current->MR_STM_tr_var, this_thread_id);
        current = current->MR_STM_tr_next;
    }
}

void
MR_STM_write_var(MR_STM_Var *var, MR_Word value, MR_STM_TransLog *tlog)
{
    MR_STM_TransRecord  *current;
    MR_bool             has_existing_record = MR_FALSE;
    
    /*
    ** Check to see if this transaction variable has an existing record in
    ** transaction log; if so, update it.
    */
    current = tlog->MR_STM_tl_records;
    while (current != NULL) {
        if (current->MR_STM_tr_var == var) {
            has_existing_record = MR_TRUE;
            current->MR_STM_tr_new_value = value;
            break;
        }
        current = current->MR_STM_tr_next;
    }

    /*
    ** Add a new entry for the transaction variable if didn't already
    ** have one.
    */
    if (!has_existing_record) {
        MR_STM_record_transaction(tlog, var, var->MR_STM_var_value, value);
    }
}

MR_Word
MR_STM_read_var(MR_STM_Var *var, MR_STM_TransLog *tlog)
{
    MR_STM_TransRecord  *current;

    current = tlog->MR_STM_tl_records;
    while (current != NULL) {
        if (current->MR_STM_tr_var == var) {
            return current->MR_STM_tr_new_value;
        }
        current = current->MR_STM_tr_next;
    }

    /*
    ** We will only get to this point if the transaction variable does not
    ** currently have a record in the log, i.e. if this is the first time
    ** that its value has been read during this transaction.
    ** Add an entry that indicates that it has been read and then return
    ** the value that is stored in the transaction variable.
    */
    MR_STM_record_transaction(tlog, var, var->MR_STM_var_value,
        var->MR_STM_var_value);

    return var->MR_STM_var_value;
}

void
MR_STM_retry_impl(MR_STM_TransLog *tlog)
{
    MR_fatal_error("Sorry, STM retry not yet implemented.");
}    
