%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: stm_builtin.m.
% Main author: lm.
% Stability: low.
% 
% This file is automatically imported into every module.
% It contains the builtin datatypes and runtime support for
% the Software Memory Transactional system.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module stm_builtin.
:- interface.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

    % The Software Transactional Memory state. This is created for each
    % transaction when execution reaches a new atomic goal.
    % 
:- type stm.

    % A Transaction Variable of type T. This is available to all Memory
    % Transactions.  XXX Name, should be transaction_var?
    %
:- type tvar(T). 

%----------------------------------------------------------------------------%

    % Defines a new transaction variable. The type and initial value of
    % the transaction variable is determined by the first argument to
    % this predicate.
    % 
:- pred new_tvar(T::in, tvar(T)::out, io::di, io::uo) is det.

    % Adds a STM log entry indicating a write to a transaction variable.
    %
:- pred write_tvar(tvar(T)::in, T::in, stm::di, stm::uo) is det.

    % Adds a STM log entry indicating a read from a transaction variable.
    % 
:- pred read_tvar(tvar(T)::in, T::out, stm::di, stm::uo) is det.

%   % Blocks the execution of a transaction.
%   %
%:- pred atomic(stm::di) is failure.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- interface.

    % Type that is thrown when a rollback is required.  Currently, the
    % exception handling routines are being used to handle the unravelling
    % of stack frames.  An exception of this type indicates that the
    % current transaction is invalid and needs to be discarded (and retried).
    %
:- type rollback_exception
    --->    rollback_exception.

    % Creates a new stm state which will contain a transaction log
    % along with other information that is deemed important for
    % STM transactions.
    %
:- impure pred stm_create_state(stm::uo) is det.

    % Drops an stm state (simply used to assist the GC).
    %
:- impure pred stm_drop_state(stm::di) is det.

    % Locks the stm global mutex.
    %
:- impure pred stm_lock is det.

    % Unlocks the stm global mutex.
    %
:- impure pred stm_unlock is det.

%----------------------------------------------------------------------------%
% NOTE: The following predicates may only be called by a thread if it
% has aquired the global stm lock.
%----------------------------------------------------------------------------%

    % Determines whether or not a transaction is consistent with other
    % concurrently running transaction.
    %
:- impure pred stm_validate(stm::ui, bool::out) is det.

    % Commits the changes made to a log to memory.
    %
:- impure pred stm_commit(stm::ui) is det.

%-----------------------------------------------------------------------------%

    % Adds the thread ID to the wait list of all transaction variables
    % listed in the transaction log.
    %
:- impure pred stm_wait(stm::ui) is det.

    % Removes the thread ID to the wait list of all transaction variables
    % listed in the transaction log.
    %
:- impure pred stm_unwait(stm::ui) is det.

    % Blocks the thread from being rescheduled.
    %
:- impure pred stm_block_thread(stm::ui) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.

:- pragma foreign_decl("C", "

#if defined(MR_HIGHLEVEL_CODE)

    #if defined(MR_THREAD_SAFE)
        #include <pthread.h>

        typedef pthread_t   ML_ThreadId;
    #else
        typedef MR_Integer  ML_ThreadId;
       
    #endif /* !MR_THREAD_SAFE */

#else /* !MR_HIGHLEVEL_CODE */
    
    typedef MR_Context  *ML_ThreadId;

#endif /* !MR_HIGHLEVEL_CODE */

typedef struct ML_Stm_Wait_List_Struct {
    ML_ThreadId thread;
    struct ML_Stm_Wait_List_Struct *next;
} ML_Stm_Wait_List;

typedef struct {
    MR_Word tvar_val;
    ML_Stm_Wait_List *wait_list;
} ML_Stm_TVar;

typedef struct ML_Stm_TLog_Entry_Struct {
    ML_Stm_TVar    *tvar;           /* Transaction variable in question */
    MR_Word     old_value;       /* Old value of the transaction variable */
    MR_Word     new_value;       /* New value of the transaction variable */
    struct ML_Stm_TLog_Entry_Struct *next;     /* Next log entry */
} ML_Stm_TLog_Entry;

typedef struct {
    ML_Stm_TLog_Entry  *entrylist;     /* Log of transaction */
    ML_ThreadId        thread;         /* Current thread */
} ML_Stm_TLog;


/* ------------------------------------------------------------------------- */


extern void
ML_stm_add_new_log_entry(ML_Stm_TLog *slog, ML_Stm_TVar *tvar, 
        MR_Word old_value, MR_Word new_value);

extern void
ML_stm_add_new_wait_entry(ML_Stm_TVar *tvar, ML_ThreadId thread);

extern void
ML_stm_remove_wait_entry(ML_Stm_TVar *tvar, ML_ThreadId thread);

/* ------------------------------------------------------------------------- */

#define     ML_TRACE_STM(s)    \
    do {printf(\"STM: %s\\n\", (s)); fflush(stdout);} while (0)
").

    % Local C functions.
    %
:- pragma foreign_code("C", "
/*
** Adds a new log entry into a transaction log.
*/

void
ML_stm_add_new_log_entry(ML_Stm_TLog *slog, ML_Stm_TVar *tvar, 
        MR_Word old_value, MR_Word new_value) {
    ML_Stm_TLog_Entry  *new_entry;

    new_entry = MR_GC_NEW(ML_Stm_TLog_Entry);

    new_entry->tvar = tvar;
    new_entry->old_value = old_value;
    new_entry->new_value = new_value;
    
    new_entry->next = slog->entrylist;
    slog->entrylist = new_entry;
}


/*
** Adds a new wait entry to a transaction variable.
*/

void
ML_stm_add_new_wait_entry(ML_Stm_TVar *tvar, ML_ThreadId thread) {
    ML_Stm_Wait_List *wait_list;

    wait_list = MR_GC_NEW(ML_Stm_Wait_List);

    wait_list->thread = thread;
    wait_list->next = NULL;

    if (tvar->wait_list == NULL) {
        tvar->wait_list = wait_list;
    } else {
        tvar->wait_list->next = wait_list;
    }
}


/*
** Remove a wait entry from a transaction variable.
*/

void
ML_stm_remove_wait_entry(ML_Stm_TVar *tvar, ML_ThreadId thread) {
    ML_Stm_Wait_List *wait_list;
    ML_Stm_Wait_List *prev;

    prev = NULL;

    for (wait_list = tvar->wait_list; wait_list != NULL; 
            wait_list = wait_list->next) {
        if (wait_list->thread == thread) {
            if (prev == NULL) {
                tvar->wait_list = wait_list->next;
            } else {
                prev->next = wait_list->next;
            }
            break;
        }
        prev = wait_list;
    }

    /* If wait_list == NULL, the entry is being removed */

    if (wait_list != NULL) {
        wait_list = NULL;
    }
}
").

%----------------------------------------------------------------------------%

:- pragma foreign_type("C", tvar(T), "ML_Stm_TVar *", 
    [stable, can_pass_as_mercury_type]).

:- pragma foreign_type("C", stm, "ML_Stm_TLog *", [can_pass_as_mercury_type]).

    % Definitions for use with the other backends.
    %
:- type tvar(T)
    --->    tvar(c_pointer).

:- type stm
    --->    stm(c_pointer).

%----------------------------------------------------------------------------%

:- pragma foreign_decl("C",
"
    #ifdef MR_THREAD_SAFE
        extern MercuryLock  ML_STM_global_lock;
    #endif
").

:- pragma foreign_code("C",
"
    #ifdef MR_THREAD_SAFE
        MercuryLock ML_STM_global_lock;
    #endif
").

:- initialise ml_initialise_stm/0.

:- impure pred ml_initialise_stm is det.
:- pragma foreign_proc("C", ml_initialise_stm, [will_not_call_mercury],
"
    #ifdef MR_THREAD_SAFE
        pthread_mutex_init(&ML_STM_global_lock, MR_MUTEX_ATTR);
    #endif
").

% For non-C backends.
ml_initialise_stm :-
    impure impure_true.

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    new_tvar(T::in, TVar::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    TVar = MR_GC_NEW(ML_Stm_TVar);
    TVar->tvar_val = T;
    TVar->wait_list = NULL;
    IO = IO0;
").

:- pragma foreign_proc("C",
    write_tvar(TVar::in, Value::in, STM0::di, STM::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    ML_Stm_TLog_Entry  *current_entry;
    MR_bool         found_entry = MR_FALSE;

    /* Looks for entry for TVar and UPDATES it */
    for (current_entry = STM0->entrylist; current_entry != NULL;
            current_entry = current_entry->next) {
        if (current_entry->tvar == TVar) {
            /* Found write entry for tvar. */
            found_entry = MR_TRUE;
            current_entry->new_value = Value;
            break;
        }
    }

    /* Adds an entry if no record of the TVar is present */
    if (found_entry == MR_FALSE) {
        ML_stm_add_new_log_entry(STM0, TVar, TVar->tvar_val, Value);
    }
        
    STM = STM0;
").

:- pragma foreign_proc("C",
    read_tvar(TVar::in, Value::out, STM0::di, STM::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    ML_Stm_TLog_Entry  *current_entry;
    MR_bool         found_entry = MR_FALSE;

    /* Looks for entry for TVar and READS it */
    for (current_entry = STM0->entrylist; current_entry != NULL;
            current_entry = current_entry->next) {
        if (current_entry->tvar == TVar) {
            /* Found write entry for tvar. */
            found_entry = MR_TRUE;
            Value = current_entry->new_value;
            break;
        }
    }

    /* Add a default entry to indicate a read has been found */
    if (found_entry == MR_FALSE)
    {
        ML_stm_add_new_log_entry(STM0, TVar, TVar->tvar_val, TVar->tvar_val);
        Value = TVar->tvar_val;
    }
        
    STM = STM0;
").

:- pragma foreign_proc("C", stm_create_state(STM::uo),
    [will_not_call_mercury, thread_safe],
"
    ML_TRACE_STM(""Allocating new STM Log --- New Ver"");

    STM = MR_GC_NEW(ML_Stm_TLog);
    STM->entrylist = NULL;

    #if defined(MR_HIGHLEVEL_CODE)
        #if defined(MR_THREAD_SAFE)
            STM->thread = pthread_self();
        #else
            STM->thread = 0;
        #endif
    #else
        STM->thread = NULL;         /* current context */
    #endif
").

:- pragma foreign_proc("C", stm_drop_state(X::di),
    [will_not_call_mercury, thread_safe],
"
    ML_TRACE_STM(""Dropping STM Log"");
    X = NULL; 
").

:- pragma foreign_proc("C", stm_lock,
    [will_not_call_mercury, thread_safe],
"
    ML_TRACE_STM(""Locking STM Global Lock"");
    #ifdef MR_THREAD_SAFE
        MR_LOCK(&ML_STM_global_lock, \"stm_lock/0\");
    #endif
").

:- pragma foreign_proc("C", stm_unlock,
    [will_not_call_mercury, thread_safe],
"
    ML_TRACE_STM(""Unlocking STM Global Lock"");
    #ifdef MR_THREAD_SAFE
        MR_UNLOCK(&ML_STM_global_lock, \"stm_unlock/0\");
    #endif
").

:- pragma foreign_proc("C",
    stm_validate(STM::ui, Res::out),
    [will_not_call_mercury, thread_safe],
"
    ML_Stm_TLog_Entry  *current_entry;
    ML_TRACE_STM(""Validating STM log"");

    Res = MR_YES;

    for (current_entry = STM->entrylist; current_entry != NULL;
            current_entry = current_entry->next) {
        if (current_entry->tvar->tvar_val != current_entry->old_value) {
            ML_TRACE_STM(""STM LOG INVALID!"");

            Res = MR_NO;
            break;
        }
    }

").

:- pragma foreign_proc("C",
    stm_commit(STM::ui),
    [will_not_call_mercury, thread_safe],
"
    ML_Stm_TLog_Entry  *current_entry;
    ML_TRACE_STM(""Committing STM log"");

    for (current_entry = STM->entrylist; current_entry != NULL;
            current_entry = current_entry->next) {
        current_entry->tvar->tvar_val = current_entry->new_value;
    }
").

%-----------------------------------------------------------------------------%

    % Adds the thread ID to the wait list of all transaction variables
    % listed in the transaction log.
    %
:- pragma foreign_proc("C",
    stm_wait(STM::ui),
    [will_not_call_mercury, thread_safe],
"
    ML_Stm_TLog_Entry  *current_entry;

    ML_TRACE_STM(""Waiting on thread"");

    /* Go through each transaction log and add this thread to thir wait 
    ** list 
    */

    for (current_entry = STM->entrylist; current_entry != NULL;
            current_entry = current_entry->next) {
        ML_stm_add_new_wait_entry(current_entry->tvar, STM->thread);
    }
").

    % Removes the thread ID to the wait list of all transaction variables
    % listed in the transaction log.
    %
:- pragma foreign_proc("C",
    stm_unwait(STM::ui),
    [will_not_call_mercury, thread_safe],
"
    ML_Stm_TLog_Entry  *current_entry;
    
    ML_TRACE_STM(""Un-waiting on thread"");

    /* Go through each transaction log and add this thread to thir wait 
    ** list 
    */

    for (current_entry = STM->entrylist; current_entry != NULL;
            current_entry = current_entry->next) {
        ML_stm_remove_wait_entry(current_entry->tvar, STM->thread);
    }
").

    % Blocks the thread from being rescheduled.
    %
:- pragma foreign_proc("C",
    stm_block_thread(_STM::ui),
    [will_not_call_mercury, thread_safe],
"
#if defined(MR_HIGHLEVEL_CODE) && defined(MR_THREAD_SAFE)
    pthread_yield();
#else
    ML_TRACE_STM(""Yielding to thread"");
#endif
").

%-----------------------------------------------------------------------------%

/* To Implement:
**
**  retry(STM) :-
**      stm_lock,
**      stm_validate(STM, Valid),
**      (
**          Valid = yes,
**          stm_wait(STM),        % Add wait variables to TVars
**          stm_unlock,          
**          block_and_wait(STM)
**      ;   
**          Valid = no,
**          stm_unlock,
**          throw(RollbackException)
**      ).
**
**  :- pred block_and_wait(stm::di) is failure.
**
**  block_and_wait(STM) :-
**      block_thread(STM),      ***
**      stm_lock,
**      stm_validate(STM, Valid),
**      (
**          Valid = yes,
**          stm_unlock,
**          block_and_wait,
**      ;
**          Valid = no,
**          stm_unwait(STM),    % Remove wait variables from TVar
**          stm_unlock
**          throw(RollbackException)
**      ).
** 
**  Need to implement:
**
**  :- impure pred stm_wait(stm::ui) is det.
**      % Adds the current thread to the wait list of all TVars in the
**      % STM log so far.
**
**  :- impure pred stm_unwait(stm::ui) is det.
**      % Removes the current thread from the wait list of all TVars.
**
**  :- impure pred block_thread(stm::ui) is det.
**      % Blocks the current thread until another transaction is committed.
**      % XXX: What to do when there is only one thread?
*/

%----------------------------------------------------------------------------%
:- end_module stm_builtin.
%----------------------------------------------------------------------------%
