%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: stm_builtin.m.
% Main author: lmika.
% Stability: low.
% 
% This file is automatically imported into every module that uses software
% transactional memory (STM).  It defines the data types and predicates
% use to implement STM.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module stm_builtin.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%
%
% Transaction state
%

    % The STM transaction state type is used to store a log of (tentative)
    % updates to stm_vars (defined below) within an atomic block.
    % Within an atomic block each call that reads or writes an stm_var has
    % a pair of arguments of this type threaded through it.
    % These arguments are unique so that read or writes to stm_vars cannot
    % be backtracked over.
    %
    % Values of this type are implicitly created by the compiler at the
    % beginning of an atomic block and passed to the goal within that block.
    % User program should not create values of this type.
    %
:- type stm.

%----------------------------------------------------------------------------%
%
% Transaction variables
%
    
    % A transaction variable may contain a value of type T.
    % It may only be accessed from within an atomic scope.
    %
:- type stm_var(T).

    % new_stm_var(Value, TVar, !IO):
    %
    % Create a new transaction variable with initial value `Value'.
    %
    % XXX we need a version that works within atomic blocks as well.
    %
:- pred new_stm_var(T::in, stm_var(T)::out, io::di, io::uo) is det.

    % Update the value stored in a transaction variable.
    %
:- pred write_stm_var(stm_var(T)::in, T::in, stm::di, stm::uo) is det.

    % Read the current value stored in a transaction variable.
    %
:- pred read_stm_var(stm_var(T)::in, T::out, stm::di, stm::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- interface.

% The remainder of this file contains the builtin predicates that the compiler
% generates calls to when implementing software transactional memory.
% These predicates should not be called by user programs directly.
% This module also defines some types that are used by those predicates.

    % We throw exceptions of this type to indicate that a transaction is
    % being rolled back.
    %
:- type rollback_exception
    --->    rollback_exception.

    % Create a new transaction log.
    %
:- impure pred stm_create_state(stm::uo) is det.

    % Discard a transaction log.
    %
:- impure pred stm_drop_state(stm::di) is det.

    % Lock the STM global mutex.
    %
:- impure pred stm_lock is det.

    % Unlock the STM global mutex.
    %
:- impure pred stm_unlock is det.

    % Values of this type are returned by stm_validate/2 and indicate
    % whether a given transaction log is valid.
    %
:- type stm_validation_result
    --->    stm_transaction_valid
    ;       stm_transaction_invalid.

    % Record whether the (partial) transaction recorded in the given
    % transaction log is valid or not.
    %
:- impure pred stm_validate(stm::ui, stm_validation_result::out) is det.

    % Write the changes in the given log to memory.
    % 
    % NOTE: this predicate must *only* be called while the STM global mutex
    %       is locked.
    %
:- impure pred stm_commit(stm::ui) is det.

    % Add this thread's identity to the wait list of the transaction
    % variables referenced by the given log.
    %
    % NOTE: this predicate must *only* be called while the STM global mutex
    %       is locked.
    %
:- impure pred stm_wait(stm::ui) is det.

    % Remove the current thread identity to the wait list of the transaction
    % variables referenced by the given log.
    %
    % NOTE: this predicate must *only* be called while the STM global mutex
    %       is locked.
    %
:- impure pred stm_unwait(stm::ui) is det.

    % Cause the current thread to block.
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

#define ML_STM_TRANSACTION_VALID 0
#define ML_STM_TRANSACTION_INVALID 1

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

:- pragma foreign_type("C", stm_var(T), "ML_Stm_TVar *", 
    [stable, can_pass_as_mercury_type]).

:- pragma foreign_type("C", stm, "ML_Stm_TLog *", [can_pass_as_mercury_type]).

    % Definitions for use with the other backends.
    %
:- type stm_var(T)
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
    new_stm_var(T::in, TVar::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    TVar = MR_GC_NEW(ML_Stm_TVar);
    TVar->tvar_val = T;
    TVar->wait_list = NULL;
    IO = IO0;
").

:- pragma foreign_proc("C",
    write_stm_var(TVar::in, Value::in, STM0::di, STM::uo),
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
    read_stm_var(TVar::in, Value::out, STM0::di, STM::uo),
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

:- pragma foreign_proc("C",
    stm_create_state(STM::uo),
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

:- pragma foreign_proc("C",
    stm_drop_state(X::di),
    [will_not_call_mercury, thread_safe],
"
    ML_TRACE_STM(""Dropping STM Log"");
    X = NULL; 
").

:- pragma foreign_proc("C",
    stm_lock,
    [will_not_call_mercury, thread_safe],
"
    ML_TRACE_STM(""Locking STM Global Lock"");
    #ifdef MR_THREAD_SAFE
        MR_LOCK(&ML_STM_global_lock, \"stm_lock/0\");
    #endif
").

:- pragma foreign_proc("C",
    stm_unlock,
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

    Res = ML_STM_TRANSACTION_VALID;

    for (current_entry = STM->entrylist; current_entry != NULL;
            current_entry = current_entry->next) {
        if (current_entry->tvar->tvar_val != current_entry->old_value) {
            ML_TRACE_STM(""STM LOG INVALID!"");

            Res = ML_STM_TRANSACTION_INVALID;
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

    /*
    ** Add this thread id to each transaction var referenced by the log.
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

    /*
    ** Remove this thread id to each transaction var referenced by the log.
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
**  :- impure pred block_thread(stm::ui) is det.
**      % Blocks the current thread until another transaction is committed.
**      % XXX: What to do when there is only one thread?
*       % XXX: this needs to be fixed - juliensf.
*/

%----------------------------------------------------------------------------%
:- end_module stm_builtin.
%----------------------------------------------------------------------------%
