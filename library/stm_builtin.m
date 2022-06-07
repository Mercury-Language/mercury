%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2007-2009, 2011 The University of Melbourne.
% Copyright (C) 2014-2015, 2018, 2021-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: stm_builtin.m.
% Main author: lmika.
% Stability: low.
%
% This module defines types and predicates that can be used with the
% Software Transactional Memory constructs.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module stm_builtin.
:- interface.

:- import_module io.

%---------------------------------------------------------------------------%
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

%---------------------------------------------------------------------------%
%
% Transaction variables
%

    % A transaction variable may contain a value of type T.
    % It may only be accessed from within an atomic scope.
    %
:- type stm_var(T).

    % new_stm_var(Value, TVar, !IO):
    %
    % Create a new transaction variable with initial value Value.
    %
:- pred new_stm_var(T::in, stm_var(T)::out, io::di, io::uo) is det.

    % new_stm_var_atomic(Value, TVar, !STM):
    %
    % A version of new_stm_var which works within an atomic scope.
    %
:- pred new_stm_var_atomic(T::in, stm_var(T)::out, stm::di, stm::uo) is det.

    % Update the value stored in a transaction variable.
    %
:- pred write_stm_var(stm_var(T)::in, T::in, stm::di, stm::uo) is det.

    % Read the current value stored in a transaction variable.
    %
:- pred read_stm_var(stm_var(T)::in, T::out, stm::di, stm::uo) is det.

%---------------------------------------------------------------------------%
%
% Retry
%

    % Abort the current transaction and restart it from the beginning.
    % Operationally this causes the calling thread to block until the value
    % of at least one transaction variable read during the attempted
    % transaction is written by another thread.
    %
:- pred retry(stm::ui) is erroneous.

%---------------------------------------------------------------------------%
%
% Closure versions of atomic transactions.  These predicates can be used
% to perform Software Transactional Memory without using the atomic scope.
%

    % atomic_transaction(Closure, Result, !IO):
    %
    % Performs the Software Transactional Memory operations in Closure
    % atomically.  If the transaction is invalid, the Closure is
    % re-executed.
    %
:- pred atomic_transaction(pred(T, stm, stm)::in(pred(out, di, uo) is det),
    T::out, io::di, io::uo) is det.

    % or_else(AtomicClosure1, AtomicClosure2, Result, !STM):
    %
    % Performs the Software Transactional Memory operations in AtomicClosure1
    % atomically.  If a retry is thrown, AtomicClosure2 is executed atomically.
    %
:- pred or_else(pred(T, stm, stm)::in(pred(out, di, uo) is det),
    pred(T, stm, stm)::in(pred(out, di, uo) is det),
    T::out, stm::di, stm::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

:- interface.

% The remainder of this file contains the builtin predicates that the compiler
% generates calls to when implementing software transactional memory.
% These predicates should not be called by user programs directly.
% This module also defines some types that are used by those predicates.

    % We throw exceptions of this type to indicate that a transaction is
    % being rolled back.
    %
:- type rollback_exception
    --->    rollback_invalid_transaction
    ;       rollback_retry.

    % Create a new transaction log.
    %
:- impure pred stm_create_transaction_log(stm::uo) is det.

    % Discard a transaction log.
    %
:- impure pred stm_discard_transaction_log(stm::di) is det.

    % stm_create_nested_transaction_log(Parent, Child):
    %
    % Child is a new transaction log whose enclosing transaction's log
    % is given by Parent.
    %
:- impure pred stm_create_nested_transaction_log(stm::ui, stm::uo) is det.

    % Lock the STM global mutex.
    %
:- impure pred stm_lock is det.

    % Unlock the STM global mutex.
    %
:- impure pred stm_unlock is det.

    % Values of this type are returned by stm_validate/2 and indicate
    % whether a given transaction log is valid.
    %
    % NOTE: The definition of this type must be kept consistent with the
    % constants defined in runtime/mercury_stm.h.
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
    % NOTE: This predicate must *only* be called while the STM global mutex
    % is locked.
    %
:- impure pred stm_commit(stm::ui) is det.

    % Add this thread to the wait queues of the transaction variables referred
    % to by the given log and then block until another thread makes a commit
    % that involves one of those transaction variables.
    %
    % NOTE: This predicate must *only* be called while the STM global mutex
    % is locked.
    %
:- impure pred stm_block(stm::ui) is det.

    % This type is used in the case where an atomic_scope has no outputs
    % since the call to try_stm/3 introduced by the expansion of atomic
    % scopes needs to return at least one value.
    %
:- type stm_dummy_output
    --->    stm_dummy_output.

    % Used to enforce the uniqueness of outer and inner variables.
    % Will be removed before stm_expansion.
    %
:- pred stm_from_outer_to_inner(T::di, stm::uo) is det.
:- pred stm_from_inner_to_outer(stm::di, T::uo) is det.

    % Changes the value of a transaction variable without going through
    % the log. USE ONLY FOR DEBUGGING PURPOSES.
    %
:- pred unsafe_write_stm_var(stm_var(T)::in, T::in, stm::di, stm::uo) is det.

:- impure pred stm_merge_nested_logs(stm::di, stm::di, stm::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module univ.

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "#include \"mercury_stm.h\"").

:- pragma foreign_type("C", stm_var(T), "MR_STM_Var *",
    [stable, can_pass_as_mercury_type]).

:- pragma foreign_type("C", stm, "MR_STM_TransLog *",
    [can_pass_as_mercury_type]).

    % Definitions for use with the other backends.
    %
:- type stm_var(T)
    --->    tvar(c_pointer).

:- type stm
    --->    stm(c_pointer).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    new_stm_var(T::in, TVar::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    MR_STM_new_stm_var(T, TVar);
").

:- pragma foreign_proc("C",
    new_stm_var_atomic(T::in, TVar::out, STM0::di, STM::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    MR_STM_new_stm_var(T, TVar);
    STM = STM0;
").

:- pragma foreign_proc("C",
    write_stm_var(TVar::in, Value::in, STM0::di, STM::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    MR_STM_write_var(TVar, Value, STM0);
    STM = STM0;
").

:- pragma foreign_proc("C",
    unsafe_write_stm_var(TVar::in, Value::in, STM0::di, STM::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    MR_STM_unsafe_write_var(TVar, Value);
    STM = STM0;
").

:- pragma foreign_proc("C",
    read_stm_var(TVar::in, Value::out, STM0::di, STM::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Value = MR_STM_read_var(TVar, STM0);
    STM = STM0;
").

:- pragma foreign_proc("C",
    stm_create_transaction_log(STM::uo),
    [will_not_call_mercury, thread_safe],
"
    MR_STM_create_log(STM, NULL);

#if defined(MR_STM_DEBUG)
        fprintf(stderr, \"STM NEW LOG: log <0x%.8lx>\\n\",
            (MR_Word)(STM));
#endif
").

:- pragma foreign_proc("C",
    stm_create_nested_transaction_log(Parent::ui, Child::uo),
    [will_not_call_mercury, thread_safe],
"
    MR_STM_create_log(Child, Parent);
#ifdef  MR_STM_DEBUG
        fprintf(stderr,
                \"STM: Creating nested log <0x%.8lx>, parent <0x%.8lx>\\n\",
                (MR_Word)(Child), (MR_Word)(Parent));
#endif
").

:- pragma foreign_proc("C",
    stm_discard_transaction_log(STM::di),
    [will_not_call_mercury, thread_safe],
"
    MR_STM_discard_log(STM);
").

:- pragma foreign_proc("C",
    stm_merge_nested_logs(Child::di, Parent0::di, Parent::uo),
    [will_not_call_mercury, thread_safe],
"
    // Avoid a warning: Child, Parent0, Parent
#if defined(MR_STM_DEBUG)
    fprintf(stderr, \"STM Calling Merge Nested: log <0x%.8lx>\\n\",
        (MR_Word)(Child));
#endif
    MR_STM_merge_transactions(Child);
    Parent = Parent0;
").

:- pragma foreign_proc("C",
    stm_lock,
    [will_not_call_mercury, thread_safe],
"
    #if defined(MR_THREAD_SAFE)
        MR_LOCK(&MR_STM_lock, \"stm_lock/0\");
    #endif
    #if defined(MR_STM_DEBUG)
        fprintf(stderr, \"STM LOCKING\\n\");
    #endif
").

:- pragma foreign_proc("C",
    stm_unlock,
    [will_not_call_mercury, thread_safe],
"
    #if defined(MR_STM_DEBUG)
        fprintf(stderr, \"STM UNLOCKING\\n\");
    #endif
    #if defined(MR_THREAD_SAFE)
        MR_UNLOCK(&MR_STM_lock, \"stm_unlock/0\");
    #endif
").

:- pragma foreign_proc("C",
    stm_validate(STM::ui, IsValid::out),
    [will_not_call_mercury, thread_safe],
"
    IsValid = MR_STM_validate(STM);
").

:- pragma foreign_proc("C",
    stm_commit(STM::ui),
    [will_not_call_mercury, thread_safe],
"
    MR_STM_commit(STM);
").

:- pragma foreign_proc("C",
    stm_from_outer_to_inner(IO::di, STM::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    STM = NULL;
    // ignore IO
").

:- pragma foreign_proc("C",
    stm_from_inner_to_outer(STM0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    STM0 = NULL;
    IO = 0; // The value does not matter.
").

%---------------------------------------------------------------------------%


% In high level C grades, stm_block calls a C procedure in the runtime that
% blocks the POSIX thread until signalled, as there is one POSIX thread for
% every Mercury thread.
% In the low level C grades, this approach cannot be taken, as when the context
% blocks the engine running in the POSIX thread needs to be able to look for
% other work (there might only be a single engine/POSIX thread).

:- pragma foreign_proc("C",
    stm_block(STM::ui),
    [will_not_call_mercury, thread_safe],
"
#if defined(MR_HIGHLEVEL_CODE)
    MR_STM_block_thread(STM);
#else

    MR_STM_wait(STM, STM);

#if defined(MR_STM_DEBUG)
        fprintf(stderr, ""STM BLOCKING: log <0x%.8lx>\\n"", STM);
#endif

    MR_save_context(MR_ENGINE(MR_eng_this_context));

    MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume =
        MR_ENTRY(mercury__stm_builtin__block_thread_resume);

    MR_UNLOCK(&MR_STM_lock, ""MR_STM_block_thread"");

    MR_ENGINE(MR_eng_this_context) = NULL;
    MR_idle();
#endif
").


% block_thread_resume is the piece of code we jump to when a thread suspended
% after a failed STM transaction resumes
:- pragma foreign_decl("C",
"
/*
INIT mercury_sys_init_stm_builtin_modules
*/

#if !defined(MR_HIGHLEVEL_CODE)
    MR_define_extern_entry(mercury__stm_builtin__block_thread_resume);
#endif
").

:- pragma foreign_code("C",
"
#if !defined(MR_HIGHLEVEL_CODE)

    MR_BEGIN_MODULE(hand_written_stm_builtin_module)
        MR_init_entry_ai(mercury__stm_builtin__block_thread_resume);
    MR_BEGIN_CODE

    MR_define_entry(mercury__stm_builtin__block_thread_resume);
    {
        MR_proceed();
    }
    MR_END_MODULE

#endif

    // Forward decls to suppress gcc warnings.
    void mercury_sys_init_stm_builtin_modules_init(void);
    void mercury_sys_init_stm_builtin_modules_init_type_tables(void);
    #ifdef  MR_DEEP_PROFILING
    void mercury_sys_init_stm_builtin_modules_write_out_proc_statics(
        FILE *deep_fp, FILE *procrep_fp);
    #endif

    void mercury_sys_init_stm_builtin_modules_init(void)
    {
    #if !defined(MR_HIGHLEVEL_CODE)
        hand_written_stm_builtin_module();
    #endif
    }

    void mercury_sys_init_stm_builtin_modules_init_type_tables(void)
    {
        // No types to register.
    }

    #ifdef  MR_DEEP_PROFILING
    void mercury_sys_init_stm_builtin_modules_write_out_proc_statics(
        FILE *deep_fp, FILE *procrep_fp)
    {
        // No proc_statics to write out.
    }
    #endif
").


%---------------------------------------------------------------------------%
%
% Retry
%

retry(_) :-
    throw(rollback_retry).

%---------------------------------------------------------------------------%
%
% Atomic transactions
%

atomic_transaction(Goal, Result, !IO) :-
    promise_pure (
        impure atomic_transaction_impl(Goal, Result)
    ).

or_else(TransA, TransB, Result, OuterSTM0, OuterSTM) :-
    promise_pure (
        impure stm_create_nested_transaction_log(OuterSTM0, InnerSTM_A0),
        promise_equivalent_solutions [ResultA, InnerSTM_A] (
            unsafe_try_stm(TransA, ResultA,
                InnerSTM_A0, InnerSTM_A)
        ),
        (
            ResultA = succeeded(Result),
            impure stm_merge_nested_logs(InnerSTM_A, OuterSTM0, OuterSTM)
        ;
            ResultA = exception(ExcpA),

            % If transaction A retried, then we should attempt transaction B.
            % Otherwise we just propagate the exception upwards.

            ( if ExcpA = univ(rollback_retry) then
                impure
                    stm_create_nested_transaction_log(OuterSTM0, InnerSTM_B0),
                promise_equivalent_solutions [ResultB, InnerSTM_B] (
                    unsafe_try_stm(TransB, ResultB,
                        InnerSTM_B0, InnerSTM_B)
                ),
                (
                    ResultB = succeeded(Result),
                    impure
                        stm_merge_nested_logs(InnerSTM_B, OuterSTM0, OuterSTM)
                ;
                    ResultB = exception(ExcpB),
                    ( if ExcpB = univ(rollback_retry) then
                        impure stm_lock,
                        impure stm_validate(InnerSTM_A, IsValidA),
                        impure stm_validate(InnerSTM_B, IsValidB),
                        ( if
                            IsValidA = stm_transaction_valid,
                            IsValidB = stm_transaction_valid
                        then
                            % We want to wait on the union of the transaction
                            % variables accessed during both alternatives.
                            % We merge the transaction logs (the order does not
                            % matter) and then propagate the retry upwards.
                            impure stm_merge_nested_logs(InnerSTM_A, OuterSTM0,
                                OuterSTM1),
                            impure stm_merge_nested_logs(InnerSTM_B, OuterSTM1,
                                OuterSTM),
                            impure stm_unlock,
                            retry(OuterSTM)
                        else
                            impure stm_unlock,
                            throw(rollback_invalid_transaction)
                        )
                    else
                        impure stm_discard_transaction_log(InnerSTM_B),
                        rethrow(ResultB)
                    )
                )
            else
                impure stm_discard_transaction_log(InnerSTM_A),
                rethrow(ResultA)
            )
        )
    ).

:- impure pred atomic_transaction_impl(
    pred(T, stm, stm)::in(pred(out, di, uo) is det), T::out) is det.

atomic_transaction_impl(Goal, Result) :-
    impure stm_create_transaction_log(STM0),
    promise_equivalent_solutions [Result0, STM] (
        unsafe_try_stm(call_atomic_goal(Goal), Result0, STM0, STM)
    ),
    (
        Result0 = succeeded(Result)
    ;
        Result0 = exception(Excp),
        ( if Excp = univ(rollback_invalid_transaction) then
            impure stm_discard_transaction_log(STM),
            impure atomic_transaction_impl(Goal, Result)
        else if Excp = univ(rollback_retry) then
            impure stm_lock,
            impure stm_validate(STM, IsValid),
            (
                IsValid = stm_transaction_valid,
                % NOTE: stm_block is responsible for releasing the STM lock.
                impure stm_block(STM)
            ;
                IsValid = stm_transaction_invalid,
                impure stm_unlock
            ),
            impure stm_discard_transaction_log(STM),
            impure atomic_transaction_impl(Goal, Result)
        else
            impure stm_lock,
            impure stm_validate(STM, IsValid),
            impure stm_unlock,
            (
                IsValid = stm_transaction_valid,
                rethrow(Result0)
            ;
                IsValid = stm_transaction_invalid,
                impure stm_discard_transaction_log(STM),
                impure atomic_transaction_impl(Goal, Result)
            )
        )
    ).

:- pred call_atomic_goal(pred(T, stm, stm)::in(pred(out, di, uo) is det),
    T::out, stm::di, stm::uo) is det.

call_atomic_goal(Goal, Result, !STM) :-
    promise_pure (
        Goal(Result, !STM),
        impure stm_lock,
        impure stm_validate(!.STM, IsValid),
        (
            IsValid = stm_transaction_valid,
            impure stm_commit(!.STM),
            impure stm_unlock
        ;
            IsValid = stm_transaction_invalid,
            impure stm_unlock,
            throw(rollback_invalid_transaction)
        )
    ).

%---------------------------------------------------------------------------%
:- end_module stm_builtin.
%---------------------------------------------------------------------------%
