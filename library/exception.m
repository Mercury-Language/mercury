%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2008, 2010-2011 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: exception.m.
% Main author: fjh.
% Stability: medium.
%
% This module defines the Mercury interface for exception handling.
%
% Note that throwing an exception across the C interface won't work.
% That is, if a Mercury procedure that is exported to C using
% `pragma foreign_export' throws an exception which is not caught within that
% procedure, then you will get undefined behaviour.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module exception.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module store.
:- import_module univ.

%---------------------------------------------------------------------------%

    % Exceptions of this type are used by many parts of the Mercury
    % implementation to indicate an internal error.
    %
:- type software_error
    --->    software_error(string).

    % A domain error exception, which indicates that the inputs
    % to a predicate or function were outside the domain of that
    % predicate or function. The string indicates where the error occurred.
    %
:- type domain_error
    --->    domain_error(string).

%---------------------------------------------------------------------------%

    % throw(Exception):
    %
    % Throw the specified exception.
    %
:- func throw(T) = _ is erroneous.
:- pred throw(T::in) is erroneous.

    % rethrow(ExceptionResult):
    %
    % Rethrows the specified exception result (which should be
    % of the form `exception(_)', not `succeeded(_)' or `failed'.).
    %
:- pred rethrow(exception_result(T)).
:- mode rethrow(in(bound(exception(ground)))) is erroneous.

:- func rethrow(exception_result(T)) = _.
:- mode rethrow(in(bound(exception(ground)))) = out is erroneous.

% The following type and inst are used by try/3 and try/5.

:- type exception_result(T)
    --->    succeeded(T)
    ;       failed
    ;       exception(univ).

:- inst cannot_fail for exception_result/1
    --->    succeeded(ground)
    ;       exception(ground).

    % try(Goal, Result):
    %
    % Operational semantics:
    %
    %   Call Goal(R).
    %   If Goal(R) fails, succeed with Result = failed.
    %   If Goal(R) succeeds, succeed with Result = succeeded(R).
    %   If Goal(R) throws an exception E, succeed with Result = exception(E).
    %
    % Declarative semantics:
    %
    %   try(Goal, Result) <=>
    %       ( Goal(R), Result = succeeded(R)
    %       ; not Goal(_), Result = failed
    %       ; Result = exception(_)
    %       ).
    %
:- pred try(pred(T),                exception_result(T)).
:- mode try(pred(out) is det,       out(cannot_fail)) is cc_multi.
:- mode try(pred(out) is semidet,   out)              is cc_multi.
:- mode try(pred(out) is cc_multi,  out(cannot_fail)) is cc_multi.
:- mode try(pred(out) is cc_nondet, out)              is cc_multi.

    % try_io(Goal, Result, IO_0, IO):
    %
    % Operational semantics:
    %
    %   Call Goal(R, IO_0, IO_1).
    %   If it succeeds, succeed with Result = succeeded(R) and IO = IO_1.
    %   If it throws an exception E, succeed with Result = exception(E)
    %   and with the final IO state being whatever state resulted from
    %   the partial computation from IO_0.
    %
    % Declarative semantics:
    %
    %   try_io(Goal, Result, IO_0, IO) <=>
    %       ( Goal(R, IO_0, IO), Result = succeeded(R)
    %       ; Result = exception(_)
    %       ).
    %
:- pred try_io(pred(T, io, io), exception_result(T), io, io).
:- mode try_io(pred(out, di, uo) is det,
    out(cannot_fail), di, uo) is cc_multi.
:- mode try_io(pred(out, di, uo) is cc_multi,
    out(cannot_fail), di, uo) is cc_multi.

    % try_store(Goal, Result, Store_0, Store):
    %
    % Just like try_io, but for stores rather than io.states.
    %
:- pred try_store(pred(T, store(S), store(S)),
    exception_result(T), store(S), store(S)).
:- mode try_store(pred(out, di, uo) is det,
    out(cannot_fail), di, uo) is cc_multi.
:- mode try_store(pred(out, di, uo) is cc_multi,
    out(cannot_fail), di, uo) is cc_multi.

    % try_all(Goal, MaybeException, Solutions):
    %
    % Operational semantics:
    %
    %   Try to find all solutions to Goal(S), using backtracking.
    %   Collect the solutions found in Solutions, until the goal either
    %   throws an exception or fails. If it throws an exception E,
    %   then set MaybeException = yes(E), otherwise set MaybeException = no.
    %
    % Declaratively it is equivalent to:
    %
    %   all [S] (list.member(S, Solutions) => Goal(S)),
    %   (
    %       MaybeException = yes(_)
    %   ;
    %       MaybeException = no,
    %       all [S] (Goal(S) => list.member(S, Solutions))
    %   ).
    %
:- pred try_all(pred(T), maybe(univ), list(T)).
:- mode try_all(pred(out) is det,     out, out(nil_or_singleton_list))
    is cc_multi.
:- mode try_all(pred(out) is semidet, out, out(nil_or_singleton_list))
    is cc_multi.
:- mode try_all(pred(out) is multi,   out, out) is cc_multi.
:- mode try_all(pred(out) is nondet,  out, out) is cc_multi.

:- inst [] for list/1
    --->    [].
:- inst nil_or_singleton_list for list/1
    --->    []
    ;       [ground].

    % incremental_try_all(Goal, AccumulatorPred, Acc0, Acc):
    %
    % Declaratively it is equivalent to:
    %
    %   try_all(Goal, MaybeException, Solutions),
    %   list.map(wrap_success, Solutions, Results),
    %   list.foldl(AccumulatorPred, Results, Acc0, Acc1),
    %   (
    %       MaybeException = no,
    %       Acc = Acc1
    %   ;
    %       MaybeException = yes(Exception),
    %       AccumulatorPred(exception(Exception), Acc1, Acc)
    %   )
    %
    % where (wrap_success(S, R) <=> R = succeeded(S)).
    %
    % Operationally, however, incremental_try_all/5 will call
    % AccumulatorPred for each solution as it is obtained, rather than
    % first building a list of the solutions.
    %
:- pred incremental_try_all(pred(T), pred(exception_result(T), A, A), A, A).
:- mode incremental_try_all(pred(out) is nondet,
    pred(in, di, uo) is det, di, uo) is cc_multi.
:- mode incremental_try_all(pred(out) is nondet,
    pred(in, in, out) is det, in, out) is cc_multi.

    % finally(P, PRes, Cleanup, CleanupRes, !IO).
    %
    % Call P and ensure that Cleanup is called afterwards,
    % no matter whether P succeeds or throws an exception.
    % PRes is bound to the output of P.
    % CleanupRes is bound to the output of Cleanup.
    % A exception thrown by P will be rethrown after Cleanup
    % is called, unless Cleanup throws an exception.
    % This predicate performs the same function as the `finally'
    % clause (`try {...} finally {...}') in languages such as Java.
    %
:- pred finally(pred(T, io, io), T, pred(io.res, io, io), io.res, io, io).
:- mode finally(pred(out, di, uo) is det, out,
    pred(out, di, uo) is det, out, di, uo) is det.
:- mode finally(pred(out, di, uo) is cc_multi, out,
    pred(out, di, uo) is cc_multi, out, di, uo) is cc_multi.

    % throw_if_near_stack_limits checks if the program is near
    % the limits of the Mercury stacks, and throws an exception
    % (near_stack_limits) if this is the case.
    %
    % This predicate works only in low level C grades; in other grades,
    % it never throws an exception.
    %
    % The predicate is impure instead of semipure because its effect depends
    % not only on the execution of other impure predicates, but all calls.
    %
:- type near_stack_limits
    --->    near_stack_limits.

:- impure pred throw_if_near_stack_limits is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

:- interface.

:- import_module stm_builtin.

    % XXX Once STM is stable, this predicate should be moved into the
    % documented interface of this module.
    %
:- pred try_stm(pred(A, stm, stm), exception_result(A), stm, stm).
:- mode try_stm(in(pred(out, di, uo) is det),
    out(cannot_fail), di, uo) is cc_multi.
:- mode try_stm(in(pred(out, di, uo) is cc_multi),
    out(cannot_fail), di, uo) is cc_multi.

    % This is the version is called by code introduced by the source-to-source
    % transformation for atomic scopes. This predicate should not be called
    % by user code.
    %
    % It is unsafe in the sense that it does not guarantee that rollback
    % exceptions are always rethrown.
    %
:- pred unsafe_try_stm(pred(A, stm, stm),
    exception_result(A), stm, stm).
:- mode unsafe_try_stm(in(pred(out, di, uo) is det),
    out(cannot_fail), di, uo) is cc_multi.
:- mode unsafe_try_stm(in(pred(out, di, uo) is cc_multi),
    out(cannot_fail), di, uo) is cc_multi.

%---------------------------------------------------------------------------%

    % This is used in the implementation of `try' goals. It should never be
    % called in any other context.
    %
:- pred magic_exception_result(exception_result({})::out(cannot_fail))
    is cc_multi.

    % This is used in the implementation of `try' goals. It should never be
    % called in any other context.
    %
:- pred unreachable is erroneous.

    % Forwarding predicates so we don't need to implicitly import `univ'
    % in the implementation of `try' goals.
    %
:- pred exc_univ_to_type(univ, T).
:- mode exc_univ_to_type(in, out) is semidet.
:- mode exc_univ_to_type(out, in) is det.
:- mode exc_univ_to_type(uo, di) is det.

:- some [T] func exc_univ_value(univ) = T.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module solutions.
:- import_module string.
:- import_module unit.

%---------------------------------------------------------------------------%

% These are not worth inlining, since they will (presumably) not be called
% frequently, and so any increase in speed from inlining is not worth the
% increase in code size.
:- pragma no_inline(func(throw/1)).
:- pragma no_inline(pred(throw/1)).
:- pragma no_inline(func(rethrow/1)).
:- pragma no_inline(pred(rethrow/1)).

% The termination analyzer can infer termination of throw/1 itself but
% declaring it to be terminating here means that all of the standard library
% will treat it as terminating as well.
:- pragma terminates(func(throw/1)).
:- pragma terminates(pred(throw/1)).

throw(Exception) = _ :-
    throw(Exception).

throw(Exception) :-
    type_to_univ(Exception, Univ),
    throw_impl(Univ).

rethrow(exception(Univ)) :-
    throw_impl(Univ).

rethrow(ExceptionResult) = _ :-
    rethrow(ExceptionResult).

%---------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(pred((try)/2)).

try(Goal::pred(out) is det, Result::out(cannot_fail)) :-
    catch_impl(wrap_success_or_failure(Goal), wrap_exception, Result0),
    (
        Result0 = succeeded(_),
        Result = Result0
    ;
        Result0 = failed,
        Result = exception(univ("det goal failed"))
    ;
        Result0 = exception(E),
        ( Result = exception(E)
        ; Result = exception(E) % force cc_multi
        )
    ).
try(Goal::pred(out) is semidet, Result::out) :-
    catch_impl(wrap_success_or_failure(Goal), wrap_exception, Result0),
    (
        Result0 = succeeded(_),
        Result = Result0
    ;
        Result0 = failed,
        Result = failed
    ;
        Result0 = exception(E),
        ( Result = exception(E)
        ; Result = exception(E) % force cc_multi
        )
    ).
try(Goal::pred(out) is cc_multi, Result::out(cannot_fail)) :-
    catch_impl(wrap_success_or_failure(Goal), wrap_exception, Result0),
    (
        Result0 = succeeded(_),
        Result = Result0
    ;
        Result0 = failed,
        Result = exception(univ("cc_multi goal failed"))
    ;
        Result0 = exception(E),
        Result = exception(E)
    ).
try(Goal::pred(out) is cc_nondet, Result::out) :-
    catch_impl(wrap_success_or_failure(Goal), wrap_exception, Result).

%---------------------------------------------------------------------------%

try_io(IO_Goal, Result, IO0, IO) :-
    try(unsafe_call_io_goal(IO_Goal, IO0), Result0),
    (
        Result0 = succeeded({Res, IO1}),
        Result = succeeded(Res),
        % IO1 is now unique because the only other reference to
        % the I/O state was from IO0, which we're throwing away here.
        unsafe_promise_unique(IO1, IO)
    ;
        Result0 = exception(E),
        Result = exception(E),
        % IO0 is now unique because the only other reference to
        % it was from the goal which just threw an exception.
        unsafe_promise_unique(IO0, IO)
    ).

:- pred unsafe_call_io_goal(pred(T, io, io), io, {T, io}).
:- mode unsafe_call_io_goal(pred(out, di, uo) is det, in, out) is det.
:- mode unsafe_call_io_goal(pred(out, di, uo) is cc_multi, in, out)
    is cc_multi.

unsafe_call_io_goal(Goal, IO0, {Result, IO}) :-
    unsafe_promise_unique(IO0, IO1),
    Goal(Result, IO1, IO).

%---------------------------------------------------------------------------%

try_store(StoreGoal, Result, Store0, Store) :-
    try(unsafe_call_store_goal(StoreGoal, Store0), Result0),
    (
        Result0 = succeeded({Res, NewStore}),
        Result = succeeded(Res),
        % NewStore is now unique because the only other reference to
        % the store was from Store0, which we're throwing away here.
        unsafe_promise_unique(NewStore, Store)
    ;
        Result0 = exception(E0),
        % We need to make a copy of the exception object, in case
        % it contains a value returned from store.extract_ref_value.
        % See tests/hard_coded/exceptions/tricky_try_store.m.
        copy(E0, E),
        Result = exception(E),
        % Store0 is now unique because the only other reference to
        % the store was from the goal which just threw an exception.
        unsafe_promise_unique(Store0, Store)
    ).

:- pred unsafe_call_store_goal(pred(T, store(S), store(S)),
    store(S), {T, store(S)}).
:- mode unsafe_call_store_goal(pred(out, di, uo) is det, in, out) is det.
:- mode unsafe_call_store_goal(pred(out, di, uo) is cc_multi, in, out)
    is cc_multi.

unsafe_call_store_goal(Goal, Store0, {Result, Store}) :-
    % Store0 is not really unique, but it is safe to treat it as if it were
    % unique because the other reference is only used in the case when an
    % exception is thrown, and in that case the declarative semantics of
    % try_store say that the final store returned is unspecified.
    unsafe_promise_unique(Store0, Store1),
    Goal(Result, Store1, Store).

%---------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(pred(try_all/3)).

try_all(Goal::pred(out) is det,
        MaybeException::out, Solutions::out(nil_or_singleton_list)) :-
    try(Goal, Result),
    (
        Result = succeeded(Solution),
        Solutions = [Solution],
        MaybeException = no
    ;
        Result = exception(Exception),
        Solutions = [],
        MaybeException = yes(Exception)
    ).
try_all(Goal::pred(out) is semidet,
        MaybeException::out, Solutions::out(nil_or_singleton_list)) :-
    try(Goal, Result),
    (
        Result = failed,
        Solutions = [],
        MaybeException = no
    ;
        Result = succeeded(Solution),
        Solutions = [Solution],
        MaybeException = no
    ;
        Result = exception(Exception),
        Solutions = [],
        MaybeException = yes(Exception)
    ).
try_all(Goal::pred(out) is multi,
        MaybeException::out, Solutions::out) :-
    unsorted_solutions(catch_impl(wrap_success(Goal), wrap_exception),
        ResultList),
    list.foldl2(process_one_exception_result, ResultList,
        no, MaybeException, [], Solutions).
try_all(Goal::pred(out) is nondet,
        MaybeException::out, Solutions::out) :-
    unsorted_solutions(catch_impl(wrap_success(Goal), wrap_exception),
        ResultList),
    list.foldl2(process_one_exception_result, ResultList,
        no, MaybeException, [], Solutions).

:- pred process_one_exception_result(exception_result(T)::in,
    maybe(univ)::in, maybe(univ)::out, list(T)::in, list(T)::out) is det.

process_one_exception_result(exception(E), !MaybeException, !Solutions) :-
    % Ignore all but the last exception that is in the list. This is okay
    % since there should never be more than one.
    !.MaybeException = _,
    !:MaybeException = yes(E).
process_one_exception_result(succeeded(S), !MaybeException, !Solutions) :-
    !:Solutions = [S | !.Solutions].
process_one_exception_result(failed, !MaybeException, !Solutions) :-
    throw(software_error("process_one_exception_result: unexpected failure")).

%---------------------------------------------------------------------------%

incremental_try_all(Goal, AccPred, !Acc) :-
    unsorted_aggregate(catch_impl(wrap_success(Goal), wrap_exception),
        AccPred, !Acc).

%---------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(pred(finally/6)).
finally(P::(pred(out, di, uo) is det), PRes::out,
        Cleanup::(pred(out, di, uo) is det), CleanupRes::out,
        !.IO::di, !:IO::uo) :-
    promise_equivalent_solutions [!:IO, PRes, CleanupRes] (
        finally_2(P, Cleanup, PRes, CleanupRes, !IO)
    ).
finally(P::(pred(out, di, uo) is cc_multi), PRes::out,
        Cleanup::(pred(out, di, uo) is cc_multi), CleanupRes::out,
        !.IO::di, !:IO::uo) :-
    finally_2(P, Cleanup, PRes, CleanupRes, !IO).

:- pred finally_2(pred(T, io, io), pred(io.res, io, io), T, io.res,
    io, io).
:- mode finally_2(pred(out, di, uo) is det,
    pred(out, di, uo) is det, out, out, di, uo) is cc_multi.
:- mode finally_2(pred(out, di, uo) is cc_multi,
    pred(out, di, uo) is cc_multi, out, out, di, uo) is cc_multi.

:- pragma promise_pure(pred(finally_2/6)).

finally_2(P, Cleanup, PRes, CleanupRes, !IO) :-
    try_io(P, ExcpResult, !IO),
    (
        ExcpResult = succeeded(PRes),
        Cleanup(CleanupRes, !IO)
    ;
        ExcpResult = exception(_),
        Cleanup(_, !IO),
        % The I/O state resulting from Cleanup cannot possibly be used, so we
        % have to trick the compiler into not removing the call.
        ( if
            semidet_succeed,
            impure use(!.IO)
        then
            rethrow(ExcpResult)
        else
            throw(software_error("exception.finally_2"))
        )
    ).

:- impure pred use(T::in) is det.

:- pragma foreign_proc("C",
    use(_T::in),
    [will_not_call_mercury, thread_safe, no_sharing],
    ";").
:- pragma foreign_proc("C#",
    use(_T::in),
    [will_not_call_mercury, thread_safe],
    ";").
:- pragma foreign_proc("Java",
    use(_T::in),
    [will_not_call_mercury, thread_safe],
    ";").

%---------------------------------------------------------------------------%

:- pragma no_inline(pred(throw_if_near_stack_limits/0)).

throw_if_near_stack_limits :-
    ( if impure now_near_stack_limits then
        throw(near_stack_limits)
    else
        true
    ).

:- impure pred now_near_stack_limits is semidet.
:- pragma no_inline(pred(now_near_stack_limits/0)).

:- pragma foreign_proc("C",
    now_near_stack_limits,
    [will_not_call_mercury, thread_safe, no_sharing],
"
#ifdef  MR_HIGHLEVEL_CODE
    // In high level code grades, I don't know of any portable way
    // to check whether we are near the limits of the C stack.
    SUCCESS_INDICATOR = MR_FALSE;
#else
    int slack = 1024;

    if (((MR_maxfr + slack) <
        MR_CONTEXT(MR_ctxt_nondetstack_zone)->MR_zone_top)
    && ((MR_sp + slack) <
        MR_CONTEXT(MR_ctxt_detstack_zone)->MR_zone_top))
    {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        SUCCESS_INDICATOR = MR_TRUE;
    }
#endif
").

now_near_stack_limits :-
    semidet_fail.

%---------------------------------------------------------------------------%

try_stm(Goal, Result, !STM) :-
    unsafe_try_stm(Goal, Result0, !STM),
    (
        Result0 = succeeded(_),
        Result = Result0
    ;
        Result0 = exception(Exception),
        % If the exception is an STM rollback exception rethrow it since
        % the handler at the beginning of the atomic scope should deal with
        % it; otherwise let the user deal with it.
        ( if
            ( Exception = univ(stm_builtin.rollback_invalid_transaction)
            ; Exception = univ(stm_builtin.rollback_retry)
            )
        then
            rethrow(Result0)
        else
            Result = Result0
        )
    ).

unsafe_try_stm(Goal, Result, STM0, STM) :-
    try(unsafe_call_transaction_goal(Goal, STM0), Result0),
    (
        Result0 = succeeded({Res, NewSTM}),
        Result = succeeded(Res),
        unsafe_promise_unique(NewSTM, STM)
    ;
        Result0 = exception(E0),
        % XXX is this copy necessary?
        copy(E0, E),
        Result = exception(E),
        unsafe_promise_unique(STM0, STM)
    ).

:- pred unsafe_call_transaction_goal(pred(T, stm, stm), stm, {T, stm}).
:- mode unsafe_call_transaction_goal(pred(out, di, uo) is det, in, out) is det.
:- mode unsafe_call_transaction_goal(pred(out, di, uo) is cc_multi, in, out)
    is cc_multi.

unsafe_call_transaction_goal(Goal, STM0, {Result, STM}) :-
    unsafe_promise_unique(STM0, STM1),
    Goal(Result, STM1, STM).

%---------------------------------------------------------------------------%

:- pred wrap_success(pred(T), exception_result(T)).
:- mode wrap_success(pred(out) is det, out(cannot_fail)) is det.
:- mode wrap_success(pred(out) is semidet, out(cannot_fail)) is semidet.
:- mode wrap_success(pred(out) is multi, out(cannot_fail)) is multi.
:- mode wrap_success(pred(out) is nondet, out(cannot_fail)) is nondet.
:- mode wrap_success(pred(out) is cc_multi, out(cannot_fail)) is cc_multi.
:- mode wrap_success(pred(out) is cc_nondet, out(cannot_fail)) is cc_nondet.

wrap_success(Goal, succeeded(R)) :-
    Goal(R).

:- pred wrap_success_or_failure(pred(T), exception_result(T)).
:- mode wrap_success_or_failure(pred(out) is det, out) is det.
:- mode wrap_success_or_failure(pred(out) is semidet, out) is det.
% :- mode wrap_success_or_failure(pred(out) is multi, out) is multi. (unused)
% :- mode wrap_success_or_failure(pred(out) is nondet, out) is multi. (unused)
:- mode wrap_success_or_failure(pred(out) is cc_multi, out) is cc_multi.
:- mode wrap_success_or_failure(pred(out) is cc_nondet, out) is cc_multi.

wrap_success_or_failure(Goal, Result) :-
    (if Goal(R) then Result = succeeded(R) else Result = failed).

:- pred wrap_exception(univ::in, exception_result(T)::out) is det.

wrap_exception(Exception, exception(Exception)).

%---------------------------------------------------------------------------%

magic_exception_result(Res) :-
    ( if semidet_true then
        throw("magic_exception_result: should never be called")
    else
        % Avoid determinism warning.
        ( Res = succeeded({})
        ; Res = succeeded({})
        )
    ).

unreachable :-
    throw("unreachable code reached").

exc_univ_to_type(Univ, Object) :-
    univ.univ_to_type(Univ, Object).

exc_univ_value(Univ) = univ.univ_value(Univ).

%---------------------------------------------------------------------------%

:- pred throw_impl(univ::in) is erroneous.

% By default, we call the external implementation, but specific backends
% can provide their own definition using foreign_proc.

throw_impl(Univ::in) :-
    builtin_throw(Univ).

:- pragma foreign_proc("C#",
    throw_impl(T::in),
    [will_not_call_mercury, promise_pure],
"
    exception.ssdb_hooks.on_throw_impl(T);
    throw new runtime.Exception(T);
").

:- pragma foreign_proc("Java",
    throw_impl(T::in),
    [may_call_mercury, promise_pure],
"
    exception.ssdb_hooks.on_throw_impl(T);
    throw new jmercury.runtime.Exception(T);
").

%---------------------%

:- type handler(T) == pred(univ, T).
:- inst handler == (pred(in, out) is det).

% catch_impl/3 is actually impure. If the call tree of p(...) contains more
% than one throw, it returns just ONE of the exceptions p(...) can throw,
% and the declarative semantics does not say WHICH one it returns.
%
% XXX We don't declare catch_impl as impure because there do not yet exist
% unsorted_solutions/2 and unsorted_aggregate/2 predicates that take impure
% higher-order pred terms.
%
% The C# and Java backends implement catch_impl/3 using mode-specific
% foreign_procs -- the following pragma is necessary in order to avoid
% compilation errors in the presence of the fib about purity.
:- pragma promise_equivalent_clauses(pred(catch_impl/3)).
:- /* impure */
   pred catch_impl(pred(T), handler(T), T).
:- mode catch_impl(pred(out) is det,       in(handler), out) is det.
:- mode catch_impl(pred(out) is semidet,   in(handler), out) is semidet.
:- mode catch_impl(pred(out) is cc_multi,  in(handler), out) is cc_multi.
:- mode catch_impl(pred(out) is cc_nondet, in(handler), out) is cc_nondet.
:- mode catch_impl(pred(out) is multi,     in(handler), out) is multi.
:- mode catch_impl(pred(out) is nondet,    in(handler), out) is nondet.

% By default, we call the external implementation, but specific backends
% can provide their own definition using foreign_proc.
%
% NOTE: The subterm dependency tracking algorithm in the declarative debugger
% expects builtin_catch to only be called from catch_impl. If catch_impl
% is modified for a backend that supports debugging, or builtin_catch
% is called from somewhere else, then the code in browser/declarative_tree.m
% will need to be modified.

catch_impl(Pred, Handler, T) :-
    builtin_catch(Pred, Handler, T).

:- pragma foreign_proc("C#",
    catch_impl(Pred::pred(out) is det, Handler::in(handler), T::out),
    [will_not_call_mercury, promise_pure],
"
    int CSN = exception.ssdb_hooks.on_catch_impl();
    try {
        T = exception.ML_call_goal_det(TypeInfo_for_T, Pred);
    }
    catch (runtime.Exception ex) {
        exception.ssdb_hooks.on_catch_impl_exception(CSN);
        T = exception.ML_call_handler_det(TypeInfo_for_T, Handler,
            (univ.Univ_0) ex.exception);
    }
").

:- pragma foreign_proc("C#",
    catch_impl(Pred::pred(out) is cc_multi, Handler::in(handler), T::out),
    [will_not_call_mercury, promise_pure],
"
    int CSN = exception.ssdb_hooks.on_catch_impl();
    try {
        T = exception.ML_call_goal_det(TypeInfo_for_T, Pred);
    }
    catch (runtime.Exception ex) {
        exception.ssdb_hooks.on_catch_impl_exception(CSN);
        T = exception.ML_call_handler_det(TypeInfo_for_T, Handler,
            (univ.Univ_0) ex.exception);
    }
").

:- pragma foreign_proc("C#",
    catch_impl(_Pred::pred(out) is semidet, _Handler::in(handler), T::out),
    [will_not_call_mercury, promise_pure],
"
    runtime.Errors.SORRY(""catch_impl(semidet)"");
    T = null;
    SUCCESS_INDICATOR = false;
").

:- pragma foreign_proc("C#",
    catch_impl(_Pred::pred(out) is cc_nondet, _Handler::in(handler), T::out),
    [will_not_call_mercury, promise_pure],
"
    runtime.Errors.SORRY(""catch_impl(cc_nondet)"");
    T = null;
    SUCCESS_INDICATOR = false;
").

:- pragma foreign_proc("C#",
    catch_impl(Pred::pred(out) is multi, Handler::in(handler), _T::out),
    [will_not_call_mercury, promise_pure, ordinary_despite_detism],
"
    int CSN = exception.ssdb_hooks.on_catch_impl();
    try {
        runtime.MethodPtr3_r0<object, object, object> pred =
            (runtime.MethodPtr3_r0<object, object, object>) Pred[1];
        pred(Pred, cont, cont_env_ptr);
    }
    catch (runtime.Exception ex) {
        exception.ssdb_hooks.on_catch_impl_exception(CSN);
        object T = exception.ML_call_handler_det(TypeInfo_for_T, Handler,
            (univ.Univ_0) ex.exception);
        ((runtime.MethodPtr2_r0<object, object>) cont)(T, cont_env_ptr);
    }

    // Not really used.
    SUCCESS_INDICATOR = false;
").

:- pragma foreign_proc("C#",
    catch_impl(Pred::pred(out) is nondet, Handler::in(handler), _T::out),
    [will_not_call_mercury, promise_pure, ordinary_despite_detism],
"
    int CSN = exception.ssdb_hooks.on_catch_impl();
    try {
        runtime.MethodPtr3_r0<object, object, object> pred =
            (runtime.MethodPtr3_r0<object, object, object>) Pred[1];
        pred(Pred, cont, cont_env_ptr);
    }
    catch (runtime.Exception ex) {
        exception.ssdb_hooks.on_catch_impl_exception(CSN);
        object T = exception.ML_call_handler_det(TypeInfo_for_T, Handler,
            (univ.Univ_0) ex.exception);
        ((runtime.MethodPtr2_r0<object, object>) cont)(T, cont_env_ptr);
    }

    // Not really used.
    SUCCESS_INDICATOR = false;
").

:- pragma foreign_proc("Java",
    catch_impl(Pred::pred(out) is det, Handler::in(handler), T::out),
    [may_call_mercury, promise_pure],
"
    int CSN = ssdb_hooks.on_catch_impl();
    try {
        T = exception.ML_call_goal_det(TypeInfo_for_T, Pred);
    }
    catch (jmercury.runtime.Exception ex) {
        exception.ssdb_hooks.on_catch_impl_exception(CSN);
        T = exception.ML_call_handler_det(TypeInfo_for_T, Handler,
            (univ.Univ_0) ex.exception);
    }
").

:- pragma foreign_proc("Java",
    catch_impl(_Pred::pred(out) is semidet, _Handler::in(handler), T::out),
    [will_not_call_mercury, promise_pure, may_not_duplicate],
"
    // This predicate isn't called anywhere.
    // The shenanigans with `if (always)' are to avoid errors from
    // the Java compiler about unreachable code.
    boolean always = true;
    if (always) {
        throw new java.lang.Error(
            ""catch_impl (semidet) not yet implemented"");
    }
    T = null;
    SUCCESS_INDICATOR = false;
").

:- pragma foreign_proc("Java",
    catch_impl(Pred::pred(out) is cc_multi, Handler::in(handler), T::out),
    [will_not_call_mercury, promise_pure],
"
    int CSN = ssdb_hooks.on_catch_impl();
    try {
        T = exception.ML_call_goal_det(TypeInfo_for_T, Pred);
    }
    catch (jmercury.runtime.Exception ex) {
        exception.ssdb_hooks.on_catch_impl_exception(CSN);
        T = exception.ML_call_handler_det(TypeInfo_for_T, Handler,
            (univ.Univ_0) ex.exception);
    }
").

:- pragma foreign_proc("Java",
    catch_impl(_Pred::pred(out) is cc_nondet, _Handler::in(handler), T::out),
    [will_not_call_mercury, promise_pure],
"
    // This predicate isn't called anywhere.
    // The shenanigans with `if (always)' are to avoid errors from
    // the Java compiler about unreachable code.
    boolean always = true;
    if (always) {
        throw new java.lang.Error(
            ""catch_impl (cc_nondet) not yet implemented"");
    }
    T = null;
    SUCCESS_INDICATOR = false;
").

:- pragma foreign_proc("Java",
    catch_impl(Pred::pred(out) is multi, Handler::in(handler), _T::out),
    [will_not_call_mercury, promise_pure, ordinary_despite_detism],
"
    int CSN = ssdb_hooks.on_catch_impl();
    try {
        jmercury.runtime.MethodPtr3 pred =
            (jmercury.runtime.MethodPtr3) Pred[1];
        pred.call___0_0(Pred, cont, cont_env_ptr);
    }
    catch (jmercury.runtime.Exception ex) {
        ssdb_hooks.on_catch_impl_exception(CSN);
        Object T = exception.ML_call_handler_det(TypeInfo_for_T, Handler,
            (univ.Univ_0) ex.exception);
        ((jmercury.runtime.MethodPtr2) cont).call___0_0(T, cont_env_ptr);
    }

    // Not really used.
    SUCCESS_INDICATOR = false;
").

:- pragma foreign_proc("Java",
    catch_impl(Pred::pred(out) is nondet, Handler::in(handler), _T::out),
    [will_not_call_mercury, promise_pure, ordinary_despite_detism],
"
    int CSN = ssdb_hooks.on_catch_impl();
    try {
        jmercury.runtime.MethodPtr3 pred =
            (jmercury.runtime.MethodPtr3) Pred[1];
        pred.call___0_0(Pred, cont, cont_env_ptr);
    }
    catch (jmercury.runtime.Exception ex) {
        ssdb_hooks.on_catch_impl_exception(CSN);
        Object T = exception.ML_call_handler_det(TypeInfo_for_T, Handler,
            (univ.Univ_0) ex.exception);
        ((jmercury.runtime.MethodPtr2) cont).call___0_0(T, cont_env_ptr);
    }

    // Not really used.
    SUCCESS_INDICATOR = false;
").

%---------------------%

:- pred builtin_throw(univ::in) is erroneous.
:- pragma terminates(pred(builtin_throw/1)).

:- /* impure */
   pred builtin_catch(pred(T), handler(T), T).
:- mode builtin_catch(pred(out) is det, in(handler), out) is det.
:- mode builtin_catch(pred(out) is semidet, in(handler), out) is semidet.
:- mode builtin_catch(pred(out) is cc_multi, in(handler), out) is cc_multi.
:- mode builtin_catch(pred(out) is cc_nondet, in(handler), out) is cc_nondet.
:- mode builtin_catch(pred(out) is multi, in(handler), out) is multi.
:- mode builtin_catch(pred(out) is nondet, in(handler), out) is nondet.

% builtin_throw and builtin_catch are implemented below using
% hand-coded low-level C code.
%
% IMPORTANT: any changes or additions to external predicates should be
% reflected in the definition of pred_is_external in
% mdbcomp/program_representation.m. The debugger needs to know what predicates
% are defined externally, so that it knows not to expect events for those
% predicates.
:- pragma external_pred(builtin_throw/1).
:- pragma external_pred(builtin_catch/3).

%---------------------------------------------------------------------------%
%
% The --high-level-code implementation.
%

:- pragma foreign_decl("C", "
// protect against multiple inclusion
#ifndef ML_HLC_EXCEPTION_GUARD
#define ML_HLC_EXCEPTION_GUARD

  #ifdef MR_HIGHLEVEL_CODE

    #define MR_CONT_PARAMS      MR_Cont cont, void *cont_env
    #define MR_CONT_PARAM_TYPES MR_Cont, void *
    #define MR_CONT_ARGS        cont, cont_env

    // det
    void MR_CALL
    mercury__exception__builtin_catch_3_p_0(MR_Mercury_Type_Info type_info,
        MR_Pred pred, MR_Pred handler_pred, MR_Box *output);

    // semidet
    MR_bool MR_CALL
    mercury__exception__builtin_catch_3_p_1(MR_Mercury_Type_Info type_info,
        MR_Pred pred, MR_Pred handler_pred, MR_Box *output);

    // cc_multi
    void MR_CALL
    mercury__exception__builtin_catch_3_p_2(MR_Mercury_Type_Info type_info,
        MR_Pred pred, MR_Pred handler_pred, MR_Box *output);

    // cc_nondet
    MR_bool MR_CALL
    mercury__exception__builtin_catch_3_p_3(MR_Mercury_Type_Info type_info,
        MR_Pred pred, MR_Pred handler_pred, MR_Box *output);

    // multi
    void MR_CALL
    mercury__exception__builtin_catch_3_p_4(MR_Mercury_Type_Info type_info,
        MR_Pred pred, MR_Pred handler_pred, MR_Box *output,
        MR_CONT_PARAMS);

    // nondet
    void MR_CALL
    mercury__exception__builtin_catch_3_p_5(MR_Mercury_Type_Info type_info,
        MR_Pred pred, MR_Pred handler_pred, MR_Box *output,
        MR_CONT_PARAMS);

    // det ==> model_det
    #define mercury__exception__builtin_catch_3_p_0 \
        mercury__exception__builtin_catch_model_det

    // semidet ==> model_semi
    #define mercury__exception__builtin_catch_3_p_1 \
        mercury__exception__builtin_catch_model_semi

    // cc_multi ==> model_det
    #define mercury__exception__builtin_catch_3_p_2 \
        mercury__exception__builtin_catch_model_det

    // cc_nondet ==> model_semi
    #define mercury__exception__builtin_catch_3_p_3 \
        mercury__exception__builtin_catch_model_semi

    // multi ==> model_non
    #define mercury__exception__builtin_catch_3_p_4 \
        mercury__exception__builtin_catch_model_non

    // nondet ==> model_non
    #define mercury__exception__builtin_catch_3_p_5 \
        mercury__exception__builtin_catch_model_non

    void MR_CALL mercury__exception__builtin_throw_1_p_0(MR_Univ exception);

    void MR_CALL mercury__exception__builtin_catch_model_det(
        MR_Mercury_Type_Info type_info, MR_Pred pred,
        MR_Pred handler_pred, MR_Box *output);
    MR_bool MR_CALL mercury__exception__builtin_catch_model_semi(
        MR_Mercury_Type_Info type_info, MR_Pred pred,
        MR_Pred handler_pred, MR_Box *output);
    void MR_CALL mercury__exception__builtin_catch_model_non(
        MR_Mercury_Type_Info type_info, MR_Pred pred,
        MR_Pred handler_pred, MR_Box *output,
        MR_CONT_PARAMS);

  #else  // MR_HIGHLEVEL_CODE
    #ifdef MR_DEEP_PROFILING
      #include ""mercury_profiling_builtin.h""
      #include ""profiling_builtin.mh""
    #endif
  #endif // MR_HIGHLEVEL_CODE

#endif // ML_HLC_EXCEPTION_GUARD
").

:- pragma foreign_code("C", "
#ifdef MR_HIGHLEVEL_CODE

// We also need to provide definitions of these builtins
// as functions rather than as macros. This is needed
// (a) in case we take their address, and (b) for the
// GCC back-end interface.

#undef mercury__exception__builtin_catch_3_p_0
#undef mercury__exception__builtin_catch_3_p_1
#undef mercury__exception__builtin_catch_3_p_2
#undef mercury__exception__builtin_catch_3_p_3
#undef mercury__exception__builtin_catch_3_p_4
#undef mercury__exception__builtin_catch_3_p_5

// det ==> model_det
void MR_CALL
mercury__exception__builtin_catch_3_p_0(MR_Mercury_Type_Info type_info,
    MR_Pred pred, MR_Pred handler_pred, MR_Box *output)
{
    mercury__exception__builtin_catch_model_det(type_info,
        pred, handler_pred, output);
}

// semidet ==> model_semi
MR_bool MR_CALL
mercury__exception__builtin_catch_3_p_1(MR_Mercury_Type_Info type_info,
    MR_Pred pred, MR_Pred handler_pred, MR_Box *output)
{
    return mercury__exception__builtin_catch_model_semi(type_info,
        pred, handler_pred, output);
}

// cc_multi ==> model_det
void MR_CALL
mercury__exception__builtin_catch_3_p_2(MR_Mercury_Type_Info type_info,
    MR_Pred pred, MR_Pred handler_pred, MR_Box *output)
{
    mercury__exception__builtin_catch_model_det(type_info,
        pred, handler_pred, output);
}

// cc_nondet ==> model_semi
MR_bool MR_CALL
mercury__exception__builtin_catch_3_p_3(MR_Mercury_Type_Info type_info,
    MR_Pred pred, MR_Pred handler_pred, MR_Box *output)
{
    return mercury__exception__builtin_catch_model_semi(type_info,
        pred, handler_pred, output);
}

// multi ==> model_non
void MR_CALL
mercury__exception__builtin_catch_3_p_4(MR_Mercury_Type_Info type_info,
    MR_Pred pred, MR_Pred handler_pred, MR_Box *output,
    MR_CONT_PARAMS)
{
    mercury__exception__builtin_catch_model_non(type_info,
        pred, handler_pred, output, MR_CONT_ARGS);
}

// multi ==> model_non
void MR_CALL
mercury__exception__builtin_catch_3_p_5(MR_Mercury_Type_Info type_info,
    MR_Pred pred, MR_Pred handler_pred, MR_Box *output,
    MR_CONT_PARAMS)
{
    mercury__exception__builtin_catch_model_non(type_info,
        pred, handler_pred, output, MR_CONT_ARGS);
}

//---------------------------------------------------------------------------

static void
ML_call_goal_det_handcoded(MR_Mercury_Type_Info type_info,
    MR_Pred closure, MR_Box *result)
{
    typedef void MR_CALL DetFuncType(void *, MR_Box *);
    DetFuncType *code = (DetFuncType *)
        MR_field(MR_mktag(0), closure, (MR_Integer) 1);
    (*code)((void *) closure, result);
}

static MR_bool
ML_call_goal_semi_handcoded(MR_Mercury_Type_Info type_info,
    MR_Pred closure, MR_Box *result)
{
    typedef MR_bool MR_CALL SemidetFuncType(void *, MR_Box *);
    SemidetFuncType *code = (SemidetFuncType *)
        MR_field(MR_mktag(0), closure, (MR_Integer) 1);
    return (*code)((void *) closure, result);
}

static void
ML_call_goal_non_handcoded(MR_Mercury_Type_Info type_info,
    MR_Pred closure, MR_Box *result, MR_CONT_PARAMS)
{
    typedef void MR_CALL NondetFuncType(void *, MR_Box *,
        MR_CONT_PARAM_TYPES);
    NondetFuncType *code = (NondetFuncType *)
        MR_field(MR_mktag(0), closure, (MR_Integer) 1);
    (*code)((void *) closure, result, MR_CONT_ARGS);
}

//---------------------------------------------------------------------------

static void
ML_call_handler_det_handcoded(MR_Mercury_Type_Info type_info,
    MR_Pred closure, MR_Univ exception, MR_Box *result)
{
    typedef void MR_CALL HandlerFuncType(void *, MR_Box, MR_Box *);
    HandlerFuncType *code = (HandlerFuncType *)
        MR_field(MR_mktag(0), closure, (MR_Integer) 1);
    (*code)((void *) closure, (MR_Box) exception, result);
}

//---------------------------------------------------------------------------

#include <stdlib.h>
#include <setjmp.h>

typedef struct ML_ExceptionHandler_struct {
    struct ML_ExceptionHandler_struct *prev;
    jmp_buf     handler;
    MR_Univ     exception;
} ML_ExceptionHandler;

  #ifndef MR_THREAD_SAFE
  ML_ExceptionHandler   *ML_exception_handler;
  #endif

  #ifdef MR_THREAD_SAFE

    #define ML_GET_EXCEPTION_HANDLER()      \
        MR_GETSPECIFIC(MR_exception_handler_key)
    #define ML_SET_EXCEPTION_HANDLER(val)   \
    pthread_setspecific(MR_exception_handler_key, (val))

  #else  // !MR_THREAD_SAFE

    #define ML_GET_EXCEPTION_HANDLER()      ML_exception_handler
    #define ML_SET_EXCEPTION_HANDLER(val)   ML_exception_handler = (val)

  #endif // !MR_THREAD_SAFE

void MR_CALL
mercury__exception__builtin_throw_1_p_0(MR_Univ exception)
{
    ML_ExceptionHandler *exception_handler = ML_GET_EXCEPTION_HANDLER();

    if (exception_handler == NULL) {
        ML_report_uncaught_exception((MR_Word) exception);
        exit(EXIT_FAILURE);
    } else {
  #ifdef MR_DEBUG_JMPBUFS
        fprintf(stderr, ""throw longjmp %p\\n"", exception_handler->handler);
  #endif
        exception_handler->exception = exception;
        longjmp(exception_handler->handler, 1);
    }
}

  #ifdef MR_NATIVE_GC

// The following code is needed to trace the local variables
// in the builtin_catch_* functions for accurate GC.

struct mercury__exception__builtin_catch_locals {
    // Fixed fields, from struct MR_StackChain.
    struct MR_StackChain *prev;
    void (*trace)(void *this_frame);
    // Locals for this function.
    MR_Mercury_Type_Info type_info;
    MR_Pred handler_pred;
};

static void
mercury__exception__builtin_catch_gc_trace(void *frame)
{
    struct mercury__exception__builtin_catch_locals *agc_locals = frame;
    // Construct a type_info for the type `pred(univ, T)',
    // which is the type of the handler_pred.
    MR_VAR_ARITY_TYPEINFO_STRUCT(s, 2) type_info_for_handler_pred;
    type_info_for_handler_pred.MR_ti_type_ctor_info =
        &mercury__builtin__builtin__type_ctor_info_pred_0;
    type_info_for_handler_pred.MR_ti_var_arity_arity = 2;
    type_info_for_handler_pred.MR_ti_var_arity_arg_typeinfos[0] =
        (MR_TypeInfo)
        &mercury__univ__univ__type_ctor_info_univ_0;
    type_info_for_handler_pred.MR_ti_var_arity_arg_typeinfos[1] =
        (MR_TypeInfo) agc_locals->type_info;
    // Call gc_trace/1 to trace the two local variables in this frame.
    mercury__private_builtin__gc_trace_1_p_0(
        (MR_Word)
        &mercury__type_desc__type_desc__type_ctor_info_type_desc_0,
        (MR_Word) &agc_locals->type_info);
    mercury__private_builtin__gc_trace_1_p_0(
        (MR_Word) &type_info_for_handler_pred,
        (MR_Word) &agc_locals->handler_pred);
}

 #define ML_DECLARE_AGC_HANDLER                                             \
    struct mercury__exception__builtin_catch_locals agc_locals;

 #define ML_INSTALL_AGC_HANDLER(TYPE_INFO, HANDLER_PRED)                    \
    do {                                                                    \
        agc_locals.prev = mercury__private_builtin__stack_chain;            \
        agc_locals.trace = mercury__exception__builtin_catch_gc_trace;      \
        agc_locals.type_info = (TYPE_INFO);                                 \
        agc_locals.handler_pred = (HANDLER_PRED);                           \
        mercury__private_builtin__stack_chain = &agc_locals;                \
    } while(0)

 #define ML_UNINSTALL_AGC_HANDLER()                                         \
    do {                                                                    \
        mercury__private_builtin__stack_chain = agc_locals.prev;            \
    } while (0)

  #define ML_AGC_LOCAL(NAME) (agc_locals.NAME)

  #else // !MR_NATIVE_GC

  // If accurate GC is not enabled, we define all of these as NOPs.
  #define ML_DECLARE_AGC_HANDLER
  #define ML_INSTALL_AGC_HANDLER(type_info, handler_pred)
  #define ML_UNINSTALL_AGC_HANDLER()
  #define ML_AGC_LOCAL(name) (name)

  #endif // !MR_NATIVE_GC

void MR_CALL
mercury__exception__builtin_catch_model_det(MR_Mercury_Type_Info type_info,
    MR_Pred pred, MR_Pred handler_pred, MR_Box *output)
{
    ML_ExceptionHandler this_handler;
    ML_DECLARE_AGC_HANDLER

    this_handler.prev = ML_GET_EXCEPTION_HANDLER();
    ML_SET_EXCEPTION_HANDLER(&this_handler);

    ML_INSTALL_AGC_HANDLER(type_info, handler_pred);

  #ifdef MR_DEBUG_JMPBUFS
    fprintf(stderr, ""detcatch setjmp %p\\n"", this_handler.handler);
  #endif

    if (setjmp(this_handler.handler) == 0) {
        ML_call_goal_det_handcoded(type_info, pred, output);
        ML_SET_EXCEPTION_HANDLER(this_handler.prev);
        ML_UNINSTALL_AGC_HANDLER();
    } else {
  #ifdef MR_DEBUG_JMPBUFS
        fprintf(stderr, ""detcatch caught jmp %p\\n"", this_handler.handler);
  #endif

        ML_SET_EXCEPTION_HANDLER(this_handler.prev);
        ML_UNINSTALL_AGC_HANDLER();
        ML_call_handler_det_handcoded(
            ML_AGC_LOCAL(type_info), ML_AGC_LOCAL(handler_pred),
            this_handler.exception, output);
    }
}

MR_bool MR_CALL
mercury__exception__builtin_catch_model_semi(MR_Mercury_Type_Info type_info,
    MR_Pred pred, MR_Pred handler_pred, MR_Box *output)
{
    ML_ExceptionHandler this_handler;
    ML_DECLARE_AGC_HANDLER

    this_handler.prev = ML_GET_EXCEPTION_HANDLER();
    ML_SET_EXCEPTION_HANDLER(&this_handler);

    ML_INSTALL_AGC_HANDLER(type_info, handler_pred);

  #ifdef MR_DEBUG_JMPBUFS
    fprintf(stderr, ""semicatch setjmp %p\\n"", this_handler.handler);
  #endif

    if (setjmp(this_handler.handler) == 0) {
        MR_bool result = ML_call_goal_semi_handcoded(type_info, pred,
            output);
        ML_SET_EXCEPTION_HANDLER(this_handler.prev);
        ML_UNINSTALL_AGC_HANDLER();
        return result;
    } else {
  #ifdef MR_DEBUG_JMPBUFS
        fprintf(stderr, ""semicatch caught jmp %p\\n"", this_handler.handler);
  #endif

        ML_SET_EXCEPTION_HANDLER(this_handler.prev);
        ML_UNINSTALL_AGC_HANDLER();
        ML_call_handler_det_handcoded(
            ML_AGC_LOCAL(type_info), ML_AGC_LOCAL(handler_pred),
            this_handler.exception, output);
        return MR_TRUE;
    }
}

struct ML_catch_env {
    ML_ExceptionHandler this_handler;
    MR_Cont             cont;
    void                *cont_env;
};

static void MR_CALL
ML_catch_success_cont(void *env_ptr)
{
    struct ML_catch_env *env = (struct ML_catch_env *) env_ptr;

    // If we reach here, it means that the nondet goal has succeeded, so we
    // need to restore the previous exception handler before calling its
    // continuation.
    ML_SET_EXCEPTION_HANDLER(env->this_handler.prev);
    (*env->cont)(env->cont_env);

    // If we get here, it means that the continuation has failed, and so we
    // are about to redo the nondet goal. Thus we need to re-establish
    // its exception handler.
    ML_SET_EXCEPTION_HANDLER(&env->this_handler);
}

void MR_CALL
mercury__exception__builtin_catch_model_non(MR_Mercury_Type_Info type_info,
    MR_Pred pred, MR_Pred handler_pred, MR_Box *output,
    MR_Cont cont, void *cont_env)
{
    ML_DECLARE_AGC_HANDLER
    struct ML_catch_env locals;
    locals.cont = cont;
    locals.cont_env = cont_env;

    locals.this_handler.prev = ML_GET_EXCEPTION_HANDLER();
    ML_SET_EXCEPTION_HANDLER(&locals.this_handler);

    ML_INSTALL_AGC_HANDLER(type_info, handler_pred);

    #ifdef MR_DEBUG_JMPBUFS
    fprintf(stderr, ""noncatch setjmp %p\\n"", locals.this_handler.handler);
    #endif

    if (setjmp(locals.this_handler.handler) == 0) {
        ML_call_goal_non_handcoded(type_info, pred, output,
            ML_catch_success_cont, &locals);

        // If we reach here, it means that the nondet goal has failed, so we
        // need to restore the previous exception handler.
        ML_SET_EXCEPTION_HANDLER(locals.this_handler.prev);
        ML_UNINSTALL_AGC_HANDLER();
        return;
    } else {
        // We caught an exception. Restore the previous exception handler,
        // and then invoke the handler predicate for this handler.

    #ifdef MR_DEBUG_JMPBUFS
        fprintf(stderr, ""noncatch caught jmp %p\\n"",
            locals.this_handler.handler);
    #endif

        ML_SET_EXCEPTION_HANDLER(locals.this_handler.prev);
        ML_UNINSTALL_AGC_HANDLER();
        ML_call_handler_det_handcoded(
            ML_AGC_LOCAL(type_info), ML_AGC_LOCAL(handler_pred),
            locals.this_handler.exception, output);
        cont(cont_env);
    }
}

#endif // MR_HIGHLEVEL_CODE
").

%---------------------------------------------------------------------------%

:- pragma foreign_code("C#", "
// The ssdb module may supply its implementation of these methods at runtime.
public class SsdbHooks {
    public virtual void on_throw_impl(univ.Univ_0 univ) {}
    public virtual int on_catch_impl() { return 0; }
    public virtual void on_catch_impl_exception(int CSN) {}
}

public static SsdbHooks ssdb_hooks = new SsdbHooks();
").

%---------------------------------------------------------------------------%

:- pred call_goal(pred(T), T).
:- mode call_goal(pred(out) is det, out) is det.
:- mode call_goal(pred(out) is semidet, out) is semidet.
% :- mode call_goal(pred(out) is nondet, out) is nondet. % see comments below

call_goal(Goal, Result) :-
    Goal(Result).

:- pred call_handler(pred(univ, T), univ, T).
:- mode call_handler(pred(in, out) is det, in, out) is det.
% :- mode call_handler(pred(in, out) is semidet, in, out) is semidet. % unused
% :- mode call_handler(pred(in, out) is nondet, in, out) is nondet.   % unused

call_handler(Handler, Exception, Result) :-
    Handler(Exception, Result).

:- pragma foreign_export("C", call_goal(pred(out) is det, out),
    "ML_call_goal_det").
:- pragma foreign_export("C#", call_goal(pred(out) is det, out),
    "ML_call_goal_det").
:- pragma foreign_export("Java", call_goal(pred(out) is det, out),
    "ML_call_goal_det").

% This causes problems because the LLDS back-end does not let you export
% code with determinism `nondet'. Instead for C backends we hand-code it...
% see below. Hand-coding it also avoids the casting needed to use MR_Word
% (which `pragma export' procedures use for polymorphically typed arguments)
% rather than MR_Box.

% :- pragma export(call_goal(pred(out) is nondet,  out),
%   "ML_call_goal_nondet").

:- pragma foreign_export("C", call_handler(pred(in, out) is det, in, out),
    "ML_call_handler_det").
:- pragma foreign_export("C#", call_handler(pred(in, out) is det, in, out),
    "ML_call_handler_det").
:- pragma foreign_export("Java", call_handler(pred(in, out) is det, in, out),
    "ML_call_handler_det").

%---------------------------------------------------------------------------%

:- pragma foreign_code("Java", "
// The ssdb module may supply its implementation of these methods at runtime.
public static class SsdbHooks {
    public void on_throw_impl(univ.Univ_0 univ) {}
    public int on_catch_impl() { return 0; }
    public void on_catch_impl_exception(int CSN) {}
}

public static SsdbHooks ssdb_hooks;
static {
    if (ssdb_hooks == null) {
        ssdb_hooks = new SsdbHooks();
    }
}
").

%---------------------------------------------------------------------------%
%
% The --no-high-level-code implementation.
%

:- pragma foreign_decl("C", "
#ifndef MR_HIGHLEVEL_CODE
    #include <assert.h>
    #include <stdio.h>
    #include ""mercury_deep_copy.h""
    #include ""mercury_trace_base.h""
    #include ""mercury_stack_trace.h""
    #include ""mercury_layout_util.h""
    #include ""mercury_deep_profiling_hand.h""

    MR_DECLARE_TYPE_CTOR_INFO_STRUCT(mercury_data_univ__type_ctor_info_univ_0);
#endif
").

:- pragma foreign_code("C", "
// forward decls, to suppress gcc -Wmissing-decl warnings
void mercury_sys_init_exceptions_init(void);
void mercury_sys_init_exceptions_init_type_tables(void);
#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_exceptions_write_out_proc_statics(FILE *deep_fp,
    FILE *procrep_fp);
#endif

#ifndef MR_HIGHLEVEL_CODE

// MR_throw_walk_stack():
// Unwind the stack as far as possible, until we reach a frame
// with an exception handler. As we go, invoke either or both
// of two actions.
//
// (1) If MR_debug_enabled is set, then invoke
//     `MR_trace(..., MR_PORT_EXCEPTION, ...)' for each stack frame,
//     to signal to the debugger that the procedure has exited via
//     an exception. This allows to user to use the `retry' command
//     to restart a goal which exited via an exception.
//
//     Note that if MR_STACK_TRACE is not defined, then we may not be
//     able to traverse the stack all the way; in that case, we just
//     print a warning and then continue. It might be better to just
//     `#ifdef' out all this code (and the code in builtin_throw which
//     calls it) if MR_STACK_TRACE is not defined.
//
// (2) In deep profiling grades, execute the actions appropriate for
//     execution leaving the procedure invocation via the exception port.
//     (Deep profiling grades always set MR_STACK_TRACE, so in such grades
//     we *will* be able to traverse the stack all the way.)
//
// The arguments base_sp and base_curfr always hold MR_sp and MR_curfr.
// They exist only because we cannot take the addresses of MR_sp and MR_curfr.

  #ifdef MR_DEEP_PROFILING
    #define WARNING(msg)                                            \\
    do {                                                            \\
        MR_fatal_error(""cannot update exception counts: %s\\n"",   \\
            msg);                                                   \\
    } while (0)
  #else
    #define WARNING(msg)                                            \\
    do {                                                            \\
        fflush(stdout);                                             \\
        fprintf(stderr, ""mdb: warning: %s\\n""                     \\
            ""This may result in some exception events\\n""         \\
            ""being omitted from the trace.\\n"", (msg));           \\
    } while (0)
  #endif

static MR_Code *
ML_throw_walk_stack(MR_Code *success_pointer, MR_Word *base_sp,
    MR_Word *base_curfr)
{
    const MR_Internal               *label;
    const MR_LabelLayout            *return_label_layout;

    // Find the layout info for the stack frame pointed to by MR_succip.

    label = MR_lookup_internal_by_addr(success_pointer);
    if (label == NULL) {
        WARNING(""internal label not found\\n"");
        return NULL;
    }
    return_label_layout = label->MR_internal_layout;

    while (return_label_layout != NULL) {
        const MR_ProcLayout         *entry_layout;
        MR_Code                     *MR_jumpaddr;
        MR_StackWalkStepResult      result;
        const char                  *problem;
        MR_Unsigned                 reused_frames;
  #ifdef MR_DEEP_PROFILING
        MR_CallSiteDynamic          *csd;
        const MR_ProcLayout         *pl;
        MR_ProcStatic               *ps;
        MR_ProcStatic               *proc_static;
        int                         top_csd_slot;
        int                         middle_csd_slot;
        MR_CallSiteDynamic          *top_csd;
        MR_CallSiteDynamic          *middle_csd;
    #ifndef MR_USE_ACTIVATION_COUNTS
        int                         old_outermost_slot;
        MR_ProcDynamic              *old_outermost;
    #endif
  #endif

        // Check if we have reached a frame with an exception handler.

        entry_layout = return_label_layout->MR_sll_entry;
        if (!MR_DETISM_DET_STACK(entry_layout->MR_sle_detism)
            && MR_redoip_slot(base_curfr) ==
                MR_ENTRY(MR_exception_handler_do_fail))
        {
            return NULL;
        }

  #ifdef MR_DEEP_PROFILING

        // The following code is based on the logic of
        // runtime/mercury_deep_leave_port_body.h, differing
        // in getting its parameters directly from stack frames
        // guided by RTTI data and in having the additional error
        // handling required by this. Any changes here may need to be
        // reflected there and vice versa.

    #ifdef MR_EXEC_TRACE
        if (! MR_disable_deep_profiling_in_debugger) {
        // The matching parenthesis is near the end of the loop.
    #endif

        MR_enter_instrumentation();

        proc_static = entry_layout->MR_sle_proc_static;
        top_csd_slot = proc_static->MR_ps_cur_csd_stack_slot;
        middle_csd_slot = proc_static->MR_ps_next_csd_stack_slot;

        if (top_csd_slot <= 0) {
            MR_fatal_error(""builtin_throw: no top csd slot"");
        }

        if (middle_csd_slot <= 0) {
            MR_fatal_error(""builtin_throw: no middle csd slot"");
        }

    #ifndef MR_USE_ACTIVATION_COUNTS
        old_outermost_slot = proc_static->MR_ps_old_outermost_stack_slot;

        if (old_outermost_slot <= 0) {
            MR_fatal_error(""builtin_throw: no old_outer slot"");
        }
    #endif

        if (MR_DETISM_DET_STACK(entry_layout->MR_sle_detism)) {
            top_csd = (MR_CallSiteDynamic *)
                MR_based_stackvar(base_sp, top_csd_slot);
            middle_csd = (MR_CallSiteDynamic *)
                MR_based_stackvar(base_sp, middle_csd_slot);
    #ifndef MR_USE_ACTIVATION_COUNTS
            old_outermost = (MR_ProcDynamic *)
                MR_based_stackvar(base_sp, old_outermost_slot);
    #endif
        } else {
            top_csd = (MR_CallSiteDynamic *)
                MR_based_framevar(base_curfr, top_csd_slot);
            middle_csd = (MR_CallSiteDynamic *)
                MR_based_framevar(base_curfr, middle_csd_slot);
    #ifndef MR_USE_ACTIVATION_COUNTS
            old_outermost = (MR_ProcDynamic *)
                MR_based_framevar(base_curfr, old_outermost_slot);
    #endif
        }

        csd = middle_csd;
        MR_deep_assert(csd, NULL, NULL, csd == MR_current_call_site_dynamic);

    #ifdef MR_DEEP_PROFILING_PORT_COUNTS
        csd->MR_csd_own.MR_own_excps++;
    #endif

        MR_deep_assert(csd, NULL, NULL, csd->MR_csd_callee_ptr != NULL);
        pl = csd->MR_csd_callee_ptr->MR_pd_proc_layout;
        MR_deep_assert(csd, pl, NULL, pl != NULL);
        ps = pl->MR_sle_proc_static;
        MR_deep_assert(csd, pl, ps, ps != NULL);

    #ifdef MR_USE_ACTIVATION_COUNTS
        // Decrement activation count.
        ps->MR_ps_activation_count--;
        MR_deep_assert(csd, pl, ps, ps->MR_ps_activation_count >= 0);
    #else
        // Set outermost activation pointer.
        ps->MR_ps_outermost_activation_ptr = old_outermost;
    #endif

        // Set current csd.
        MR_current_call_site_dynamic = top_csd;

        MR_leave_instrumentation();
    #ifdef MR_EXEC_TRACE
        // The matching parenthesis is near the start of the loop
        }
    #endif

  #endif

        if (MR_debug_enabled) {
            // Invoke MR_trace() to trace the exception.
            if (return_label_layout->MR_sll_port != MR_PORT_EXCEPTION) {
                MR_fatal_error(""return layout port is not exception"");
            }

            MR_jumpaddr = MR_trace(return_label_layout);
            if (MR_jumpaddr != NULL) {
                return MR_jumpaddr;
            }
        }

        // Unwind the stacks back to the previous stack frame.
        // Note that we don't care whether the frame has been reused.
        result = MR_stack_walk_step(entry_layout, &return_label_layout,
            &base_sp, &base_curfr, &reused_frames, &problem);
        if (result != MR_STEP_OK) {
            WARNING(problem);
            return NULL;
        }
        MR_restore_transient_registers();
        MR_sp_word = (MR_Word) base_sp;
        MR_curfr_word = (MR_Word) base_curfr;
        MR_save_transient_registers();
    }
    return NULL;
}

// Swap the heap with the solutions heap.
#define swap_heaps()                                                \\
{                                                                   \\
    /* save the current heap */                                     \\
    MR_Word         *swap_heaps_temp_hp;                            \\
    MR_MemoryZone   *swap_heaps_temp_hp_zone;                       \\
                                                                    \\
    swap_heaps_temp_hp = MR_hp;                                     \\
    swap_heaps_temp_hp_zone = MR_ENGINE(MR_eng_heap_zone);          \\
                                                                    \\
    /* set heap to solutions heap */                                \\
    MR_hp_word = (MR_Word) MR_sol_hp;                               \\
    MR_ENGINE(MR_eng_heap_zone) =                                   \\
        MR_ENGINE(MR_eng_solutions_heap_zone);                      \\
                                                                    \\
    /* set the solutions heap to be the old heap */                 \\
    MR_sol_hp = swap_heaps_temp_hp;                                 \\
    MR_ENGINE(MR_eng_solutions_heap_zone) = swap_heaps_temp_hp_zone;\\
}

MR_define_extern_entry(mercury__exception__builtin_catch_3_0); // det
MR_define_extern_entry(mercury__exception__builtin_catch_3_1); // semidet
MR_define_extern_entry(mercury__exception__builtin_catch_3_2); // cc_multi
MR_define_extern_entry(mercury__exception__builtin_catch_3_3); // cc_nondet
MR_define_extern_entry(mercury__exception__builtin_catch_3_4); // multi
MR_define_extern_entry(mercury__exception__builtin_catch_3_5); // nondet

MR_define_extern_entry(mercury__exception__builtin_throw_1_0);

// The following is defined in runtime/mercury_ho_call.c.
MR_declare_entry(mercury__do_call_closure_compact);

// The following is defined in runtime/mercury_trace_base.c.
MR_declare_entry(MR_do_trace_redo_fail);

#ifdef  MR_DEEP_PROFILING
MR_declare_label(mercury__exception__builtin_catch_3_0_i1);
MR_declare_label(mercury__exception__builtin_catch_3_1_i1);
MR_declare_label(mercury__exception__builtin_catch_3_2_i1);
MR_declare_label(mercury__exception__builtin_catch_3_3_i1);
MR_declare_label(mercury__exception__builtin_catch_3_4_i1);
MR_declare_label(mercury__exception__builtin_catch_3_5_i1);
#endif

MR_declare_label(mercury__exception__builtin_catch_3_0_i2);
MR_declare_label(mercury__exception__builtin_catch_3_1_i2);
MR_declare_label(mercury__exception__builtin_catch_3_2_i2);
MR_declare_label(mercury__exception__builtin_catch_3_3_i2);
MR_declare_label(mercury__exception__builtin_catch_3_4_i2);
MR_declare_label(mercury__exception__builtin_catch_3_5_i2);

#ifdef  MR_DEEP_PROFILING
MR_declare_label(mercury__exception__builtin_catch_3_0_i3);
MR_declare_label(mercury__exception__builtin_catch_3_1_i3);
MR_declare_label(mercury__exception__builtin_catch_3_2_i3);
MR_declare_label(mercury__exception__builtin_catch_3_3_i3);
MR_declare_label(mercury__exception__builtin_catch_3_4_i3);
MR_declare_label(mercury__exception__builtin_catch_3_5_i3);

MR_declare_label(mercury__exception__builtin_catch_3_4_i4);
MR_declare_label(mercury__exception__builtin_catch_3_5_i4);
MR_declare_label(mercury__exception__builtin_catch_3_4_i5);
MR_declare_label(mercury__exception__builtin_catch_3_5_i5);
#endif

#if defined(MR_USE_TRAIL) || defined(MR_DEEP_PROFILING)
  MR_declare_label(mercury__exception__builtin_catch_3_4_i6);
  MR_declare_label(mercury__exception__builtin_catch_3_5_i6);
#endif

#ifdef  MR_DEEP_PROFILING
MR_declare_label(mercury__exception__builtin_catch_3_4_i7);
MR_declare_label(mercury__exception__builtin_catch_3_5_i7);
#endif

#ifdef  MR_DEEP_PROFILING
MR_declare_label(mercury__exception__builtin_catch_3_0_i8);
MR_declare_label(mercury__exception__builtin_catch_3_1_i8);
MR_declare_label(mercury__exception__builtin_catch_3_2_i8);
MR_declare_label(mercury__exception__builtin_catch_3_3_i8);
MR_declare_label(mercury__exception__builtin_catch_3_4_i8);
MR_declare_label(mercury__exception__builtin_catch_3_5_i8);
#endif

MR_declare_label(mercury__exception__builtin_throw_1_0_i1);

#define MR_DUMMY_LINE   0

MR_call_sites_user_one_ho(exception, builtin_catch, 3, 0, MR_DUMMY_LINE);
MR_proc_static_user_one_site(exception, builtin_catch, 3, 0,
    ""exception.m"", MR_DUMMY_LINE, MR_TRUE);
MR_call_sites_user_one_ho(exception, builtin_catch, 3, 1, MR_DUMMY_LINE);
MR_proc_static_user_one_site(exception, builtin_catch, 3, 1,
    ""exception.m"", MR_DUMMY_LINE, MR_TRUE);
MR_call_sites_user_one_ho(exception, builtin_catch, 3, 2, MR_DUMMY_LINE);
MR_proc_static_user_one_site(exception, builtin_catch, 3, 2,
    ""exception.m"", MR_DUMMY_LINE, MR_TRUE);
MR_call_sites_user_one_ho(exception, builtin_catch, 3, 3, MR_DUMMY_LINE);
MR_proc_static_user_one_site(exception, builtin_catch, 3, 3,
    ""exception.m"", MR_DUMMY_LINE, MR_TRUE);
MR_call_sites_user_one_ho(exception, builtin_catch, 3, 4, MR_DUMMY_LINE);
MR_proc_static_user_one_site(exception, builtin_catch, 3, 4,
    ""exception.m"", MR_DUMMY_LINE, MR_TRUE);
MR_call_sites_user_one_ho(exception, builtin_catch, 3, 5, MR_DUMMY_LINE);
MR_proc_static_user_one_site(exception, builtin_catch, 3, 5,
    ""exception.m"", MR_DUMMY_LINE, MR_TRUE);

// The various procedures of builtin_catch all allocate their stack frames
// on the nondet stack, so for the purposes of doing stack traces we say
// they have MR_DETISM_NON, even though they are not actually nondet.
//
// The fields of the MR_EXTERN_USER_PROC_STATIC_PROC_LAYOUT macro are
// the following:
//
// MR_EXTERN_USER_PROC_STATIC_PROC_LAYOUT(detism, slots, succip_locn,
//  pred_or_func, module, name, arity, mode)
//
// We must use MR_EXTERN_USER_PROC_STATIC_PROC_LAYOUT instead of the
// MR_STATIC_USER_PROC_STATIC_PROC_LAYOUT version, because with intermodule
// optimization, the caller of builtin_catch may be inlined in other modules
// (e.g. browser/declarative_debugger.m), and deep profiling may therefore
// need the address of the proc_layout structure for the call's
// call_site_static structure.
//
// Additionally, the compiler generated declaration for the proc_layout
// structure will be declared extern if the address is required in other
// modules. GCC 4 and above consider a static definition and a non-static
// declaration to be an error.

MR_EXTERN_USER_PROC_STATIC_PROC_LAYOUT(
    MR_DETISM_NON, MR_PROC_NO_SLOT_COUNT, -1,
    MR_PREDICATE, exception, builtin_catch, 3, 0);
MR_EXTERN_USER_PROC_STATIC_PROC_LAYOUT(
    MR_DETISM_NON, MR_PROC_NO_SLOT_COUNT, -1,
    MR_PREDICATE, exception, builtin_catch, 3, 1);
MR_EXTERN_USER_PROC_STATIC_PROC_LAYOUT(
    MR_DETISM_NON, MR_PROC_NO_SLOT_COUNT, -1,
    MR_PREDICATE, exception, builtin_catch, 3, 2);
MR_EXTERN_USER_PROC_STATIC_PROC_LAYOUT(
    MR_DETISM_NON, MR_PROC_NO_SLOT_COUNT, -1,
    MR_PREDICATE, exception, builtin_catch, 3, 3);
MR_EXTERN_USER_PROC_STATIC_PROC_LAYOUT(
    MR_DETISM_NON, MR_PROC_NO_SLOT_COUNT, -1,
    MR_PREDICATE, exception, builtin_catch, 3, 4);
MR_EXTERN_USER_PROC_STATIC_PROC_LAYOUT(
    MR_DETISM_NON, MR_PROC_NO_SLOT_COUNT, -1,
    MR_PREDICATE, exception, builtin_catch, 3, 5);

#ifdef  MR_DEEP_PROFILING
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 0, 1);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 1, 1);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 2, 1);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 3, 1);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 4, 1);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 5, 1);
#endif

MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 0, 2);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 1, 2);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 2, 2);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 3, 2);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 4, 2);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 5, 2);

#ifdef  MR_DEEP_PROFILING
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 0, 3);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 1, 3);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 2, 3);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 3, 3);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 4, 3);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 5, 3);
#endif

#ifdef  MR_DEEP_PROFILING
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 4, 4);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 5, 4);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 4, 5);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 5, 5);
#endif

#if defined(MR_USE_TRAIL) || defined(MR_DEEP_PROFILING)
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 4, 6);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 5, 6);
#endif

#ifdef  MR_DEEP_PROFILING
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 4, 7);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 5, 7);
#endif

#ifdef  MR_DEEP_PROFILING
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 0, 8);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 1, 8);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 2, 8);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 3, 8);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 4, 8);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_catch, 3, 5, 8);
#endif

MR_proc_static_user_no_site(exception, builtin_throw, 1, 0,
    ""exception.m"", MR_DUMMY_LINE, MR_TRUE);

// See the above comments regarding builtin_catch for the reason we
// must use MR_EXTERN_USER_PROC_STATIC_PROC_LAYOUT instead of
// MR_STATIC_USER_PROC_STATIC_PROC_LAYOUT here.

MR_EXTERN_USER_PROC_STATIC_PROC_LAYOUT(
        MR_DETISM_DET, 1, MR_LONG_LVAL_STACKVAR_INT(1),
        MR_PREDICATE, exception, builtin_throw, 1, 0);
MR_MAKE_USER_INTERNAL_LAYOUT(exception, builtin_throw, 1, 0, 1);

MR_BEGIN_MODULE(hand_written_exception_module)
    MR_init_entry_sl(mercury__exception__builtin_catch_3_0);
    MR_init_entry_sl(mercury__exception__builtin_catch_3_1);
    MR_init_entry_sl(mercury__exception__builtin_catch_3_2);
    MR_init_entry_sl(mercury__exception__builtin_catch_3_3);
    MR_init_entry_sl(mercury__exception__builtin_catch_3_4);
    MR_init_entry_sl(mercury__exception__builtin_catch_3_5);

#ifdef  MR_DEEP_PROFILING
    MR_init_label(mercury__exception__builtin_catch_3_0_i1);
    MR_init_label(mercury__exception__builtin_catch_3_1_i1);
    MR_init_label(mercury__exception__builtin_catch_3_2_i1);
    MR_init_label(mercury__exception__builtin_catch_3_3_i1);
    MR_init_label(mercury__exception__builtin_catch_3_4_i1);
    MR_init_label(mercury__exception__builtin_catch_3_5_i1);
#endif

    MR_init_label_sl(mercury__exception__builtin_catch_3_0_i2);
    MR_init_label_sl(mercury__exception__builtin_catch_3_1_i2);
    MR_init_label_sl(mercury__exception__builtin_catch_3_2_i2);
    MR_init_label_sl(mercury__exception__builtin_catch_3_3_i2);
    MR_init_label_sl(mercury__exception__builtin_catch_3_4_i2);
    MR_init_label_sl(mercury__exception__builtin_catch_3_5_i2);

#ifdef  MR_DEEP_PROFILING
    MR_init_label_sl(mercury__exception__builtin_catch_3_0_i3);
    MR_init_label_sl(mercury__exception__builtin_catch_3_1_i3);
    MR_init_label_sl(mercury__exception__builtin_catch_3_2_i3);
    MR_init_label_sl(mercury__exception__builtin_catch_3_3_i3);
    MR_init_label_sl(mercury__exception__builtin_catch_3_4_i3);
    MR_init_label_sl(mercury__exception__builtin_catch_3_5_i3);
#endif

#ifdef  MR_DEEP_PROFILING
    MR_init_label_sl(mercury__exception__builtin_catch_3_4_i4);
    MR_init_label_sl(mercury__exception__builtin_catch_3_5_i4);
    MR_init_label_sl(mercury__exception__builtin_catch_3_4_i5);
    MR_init_label_sl(mercury__exception__builtin_catch_3_5_i5);
#endif

#if defined(MR_USE_TRAIL) || defined(MR_DEEP_PROFILING)
    MR_init_label_sl(mercury__exception__builtin_catch_3_4_i6);
    MR_init_label_sl(mercury__exception__builtin_catch_3_5_i6);
#endif

#ifdef  MR_DEEP_PROFILING
    MR_init_label_sl(mercury__exception__builtin_catch_3_4_i7);
    MR_init_label_sl(mercury__exception__builtin_catch_3_5_i7);
#endif

#ifdef  MR_DEEP_PROFILING
    MR_init_label(mercury__exception__builtin_catch_3_0_i8);
    MR_init_label(mercury__exception__builtin_catch_3_1_i8);
    MR_init_label(mercury__exception__builtin_catch_3_2_i8);
    MR_init_label(mercury__exception__builtin_catch_3_3_i8);
    MR_init_label(mercury__exception__builtin_catch_3_4_i8);
    MR_init_label(mercury__exception__builtin_catch_3_5_i8);
#endif

    MR_init_entry_sl(mercury__exception__builtin_throw_1_0);
    MR_init_label_sl(mercury__exception__builtin_throw_1_0_i1);
MR_BEGIN_CODE

// builtin_catch(Goal, Handler, Result)
//  call Goal(R).
//  if succeeds, set Result = R.
//  if fails, fail.
//  if throws an exception, call Handler(Exception, Result).
//
// On entry, we have a type_info (which we don't use) in MR_r1,
// the Goal to execute in MR_r2 and the Handler in MR_r3.
// On exit, we should put Result in MR_r1.
//
// There are slight differences between the versions of the code
// for the different determinisms.

#define save_r1                 do {                                    \
                                    MR_framevar(1) = MR_r1;             \
                                } while (0)
#define save_r1r2               do {                                    \
                                    MR_framevar(1) = MR_r1;             \
                                    MR_framevar(2) = MR_r2;             \
                                } while (0)
#define restore_r1              do {                                    \
                                    MR_r1 = MR_framevar(1);             \
                                } while (0)
#define restore_r1r2            do {                                    \
                                    MR_r1 = MR_framevar(1);             \
                                    MR_r2 = MR_framevar(2);             \
                                } while (0)

// mercury__exception__builtin_catch_3_0: the det version
#define proc_label              mercury__exception__builtin_catch_3_0
#define proc_layout             MR_proc_layout_user_name(exception,     \
                                    builtin_catch, 3, 0)
#define excp_handler            MR_MODEL_DET_HANDLER
#define model                   ""[model det]""
#define save_results()          save_r1
#define restore_results()       restore_r1
#define handle_ticket_on_exit() do {                                    \
                                    MR_prune_ticket();                  \
                                } while (0)

#include ""mercury_exception_catch_body.h""

#undef  proc_layout
#undef  proc_label

// mercury__exception__builtin_catch_3_2: the cc_multi version
// identical to mercury__exception__builtin_catch_3_0 except for label names
#define proc_label              mercury__exception__builtin_catch_3_2
#define proc_layout             MR_proc_layout_user_name(exception, \
                                    builtin_catch, 3, 2)

#include ""mercury_exception_catch_body.h""

#undef  handle_ticket_on_exit
#undef  restore_results
#undef  save_results
#undef  model
#undef  excp_handler
#undef  proc_layout
#undef  proc_label

// mercury__exception__builtin_catch_3_1: the semidet version
#define proc_label              mercury__exception__builtin_catch_3_1
#define proc_layout             MR_proc_layout_user_name(exception, \
                                    builtin_catch, 3, 1)
#define excp_handler            MR_MODEL_SEMI_HANDLER
#define model                   ""[model semi]""
#define save_results()          save_r1r2
#define restore_results()       restore_r1r2
#define handle_ticket_on_exit() do {                                    \
                                    if (MR_r1) {                        \
                                        MR_prune_ticket();              \
                                    } else {                            \
                                        MR_discard_ticket();            \
                                    }                                   \
                                } while (0)

#include ""mercury_exception_catch_body.h""

#undef  proc_layout
#undef  proc_label

// mercury__exception__builtin_catch_3_3: the cc_nondet version
// identical to mercury__exception__builtin_catch_3_1 except for label names
#define proc_label              mercury__exception__builtin_catch_3_3
#define proc_layout             MR_proc_layout_user_name(exception, \
                                    builtin_catch, 3, 3)

#include ""mercury_exception_catch_body.h""

#undef  handle_ticket_on_exit
#undef  restore_results
#undef  save_results
#undef  model
#undef  excp_handler
#undef  proc_layout
#undef  proc_label

// mercury__exception__builtin_catch_3_4: the multi version
#define proc_label              mercury__exception__builtin_catch_3_4
#define proc_layout             MR_proc_layout_user_name(exception, \
                                    builtin_catch, 3, 4)
#define excp_handler            MR_MODEL_NON_HANDLER
#define model                   ""[model non]""
#define save_results()          save_r1
#define restore_results()       restore_r1
#define version_model_non       MR_TRUE
#define handle_ticket_on_exit() ((void) 0)
#define handle_ticket_on_fail() do {                            \
                                    MR_prune_ticket();          \
                                } while (0)

#include ""mercury_exception_catch_body.h""

#undef  proc_layout
#undef  proc_label

// mercury__exception__builtin_catch_3_5: the nondet version
// identical to mercury__exception__builtin_catch_3_4 except for label names
#define proc_label              mercury__exception__builtin_catch_3_5
#define proc_layout             MR_proc_layout_user_name(exception,     \
                                    builtin_catch, 3, 5)

#include ""mercury_exception_catch_body.h""

#undef  handle_ticket_on_fail
#undef  handle_ticket_on_exit
#undef  version_model_non
#undef  restore_results
#undef  save_results
#undef  model
#undef  excp_handler
#undef  proc_layout
#undef  proc_label

// builtin_throw(Exception):
//
// Throw the specified exception.
// That means unwinding the nondet stack until we find a handler,
// unwinding all the other Mercury stacks, and then
// calling longjmp() to unwind the C stack.
// The longjmp() will branch to builtin_catch which will then
// call Handler(Exception, Result).
//
// On entry, we have Exception in MR_r1.

MR_define_entry(mercury__exception__builtin_throw_1_0);
{
    MR_Word                     exception;
    MR_Word                     handler;
    enum MR_HandlerCodeModel    catch_code_model;
    MR_bool                     trace_from_full;
    MR_Word                     *orig_curfr;
    MR_Unsigned                 exception_event_number;
    MR_bool                     walk_stack;

    exception = MR_r1;
    exception_event_number = MR_trace_event_number;

    // Let the debugger and/or the deep profiler trace exception throwing.

#ifdef  MR_DEEP_PROFILING
    walk_stack = MR_TRUE;
#else
    walk_stack = MR_debug_enabled;
#endif

    if (walk_stack) {
        MR_Code     *MR_jumpaddr;

        MR_trace_set_exception_value(exception);
        MR_save_transient_registers();
        MR_jumpaddr = ML_throw_walk_stack(MR_succip, MR_sp, MR_curfr);
        MR_restore_transient_registers();

        if (MR_jumpaddr != NULL) {
            MR_GOTO(MR_jumpaddr);
        }
    }

    // Search the nondet stack for an exception handler,
    // i.e. a frame whose redoip is `MR_exception_handler_do_fail'
    // (one created by `builtin_catch').
    // N.B. We search down the `succfr' chain, not the `prevfr' chain;
    // this ensures that we only find handlers installed by our callers,
    // not handlers installed by procedures that we called but which
    // are still on the nondet stack because they left choice points behind.

    orig_curfr = MR_curfr;
    while (MR_redoip_slot(MR_curfr)
        != MR_ENTRY(MR_exception_handler_do_fail))
    {
        MR_curfr_word = MR_succfr_slot_word(MR_curfr);
        if (MR_curfr < MR_CONTEXT(MR_ctxt_nondetstack_zone)->MR_zone_min) {
            MR_Word save_succip_word;

            // There was no exception handler.
            //
            // We restore the original value of MR_curfr, print out some
            // diagnostics, and then terminate execution.
            //
            // We need to save the registers to the fake_reg array using
            // MR_save_registers() before calling ML_report_uncaught_exception,
            // since that is Mercury code and the C->Mercury interface expects
            // the registers to be saved. We also need to save & restore
            // the MR_succip across that call, since any call to Mercury code
            // may clobber MR_succip (and also the Mercury registers MR_r1,
            // MR_r2, MR_r3, etc., but for those we don't care, since we don't
            // use them). Note that the MR_save_registers() alone is not
            // sufficient since the Mercury code may clobber the copy of
            // MR_succip in the fake_reg.

            MR_curfr_word = (MR_Word) orig_curfr;
            fflush(stdout);
            save_succip_word = MR_succip_word;
            MR_save_registers();
            ML_report_uncaught_exception(exception);
            MR_succip_word = save_succip_word;
            MR_trace_report(stderr);
            if (exception_event_number > 0) {
                if (MR_standardize_event_details) {
                    fprintf(stderr,
                        ""Last trace event before the unhandled exception""
                        "" was event #E%ld.\\n"",
                        (long)
                            MR_standardize_event_num(exception_event_number));
                } else {
                    fprintf(stderr,
                        ""Last trace event before the unhandled exception""
                        "" was event #%ld.\\n"",
                        (long) exception_event_number);
                }
            }
            if (walk_stack) {
                // The stack has already been unwound by ML_throw_walk_stack(),
                // so we can't dump it. (In fact, if we tried to dump the
                // now-empty stack, we'd get incorrect results, since
                // ML_throw_walk_stack() does not restore MR_succip
                // to the appropriate value.)
            } else {
                MR_dump_stack(MR_succip, MR_sp, MR_curfr, MR_FALSE);
            }

            MR_perform_registered_exception_cleanups();
            exit(1);
        }
    }

    // Save the handler we found.
    catch_code_model = MR_EXCEPTION_STRUCT->MR_excp_code_model;
    handler = MR_EXCEPTION_STRUCT->MR_excp_handler;
    trace_from_full = (MR_bool) MR_EXCEPTION_STRUCT->MR_excp_full_trace;

    // Reset the success ip (i.e. return address).
    // This ensures that when we return from this procedure,
    // we will return to the caller of `builtin_catch'.
    MR_succip_word = MR_succip_slot_word(MR_curfr);

    // Reset the det stack.
    MR_sp_word = (MR_Word) MR_EXCEPTION_STRUCT->MR_excp_stack_ptr;

#ifdef MR_USE_TRAIL
    // Reset the trail.
    MR_reset_ticket(MR_EXCEPTION_STRUCT->MR_excp_trail_ptr,
        MR_exception);
    MR_discard_tickets_to(MR_EXCEPTION_STRUCT->MR_excp_ticket_counter);
#endif
#ifdef MR_RECLAIM_HP_ON_FAILURE
    // Reset the heap. But we need to be careful to preserve the
    // thrown exception object.
    //
    // The following algorithm uses the `solutions heap', and will work
    // with non-conservative gc. We copy the exception object to the
    // solutions_heap, reset the heap pointer, and then copy it back.
    //
    // An improvement to this would be to copy the exception object to the
    // solutions heap, but have deep_copy add an offset to the pointers
    // (at least, those that would otherwise point to the solutions heap),
    // so that, when finished, a block move of the solutions heap back to
    // the real heap will leave all the pointers in the correct place.
{
    MR_Word * saved_solns_heap_ptr;

    // Switch to the solutions heap.
    if (MR_ENGINE(MR_eng_heap_zone) ==
        MR_EXCEPTION_STRUCT->MR_excp_heap_zone)
    {
        swap_heaps();
    }

    saved_solns_heap_ptr = MR_hp;

    // MR_deep_copy() the exception to the solutions heap.
    // Note that we need to save/restore the hp register, if it is transient,
    // before/after calling MR_deep_copy().
    assert(MR_EXCEPTION_STRUCT->MR_excp_heap_ptr <=
        MR_EXCEPTION_STRUCT->MR_excp_heap_zone->MR_zone_top);
    MR_save_transient_registers();
    exception = MR_deep_copy(exception,
        (MR_TypeInfo) &mercury_data_univ__type_ctor_info_univ_0,
        MR_EXCEPTION_STRUCT->MR_excp_heap_ptr,
        MR_EXCEPTION_STRUCT->MR_excp_heap_zone->MR_zone_top);
    MR_restore_transient_registers();

    // Switch back to the ordinary heap.
    swap_heaps();

    // Reset the heap.
    assert(MR_EXCEPTION_STRUCT->MR_excp_heap_ptr <= MR_hp);
    MR_hp_word = (MR_Word) MR_EXCEPTION_STRUCT->MR_excp_heap_ptr;

    // MR_deep_copy the exception back to the ordinary heap.
    assert(MR_EXCEPTION_STRUCT->MR_excp_solns_heap_ptr <=
        MR_ENGINE(MR_eng_solutions_heap_zone)->MR_zone_top);
    MR_save_transient_registers();
    exception = MR_deep_copy(exception,
        (MR_TypeInfo) &mercury_data_univ__type_ctor_info_univ_0,
        saved_solns_heap_ptr,
        MR_ENGINE(MR_eng_solutions_heap_zone)->MR_zone_top);
    MR_restore_transient_registers();

    // Reset the solutions heap.
    assert(MR_EXCEPTION_STRUCT->MR_excp_solns_heap_ptr
        <= saved_solns_heap_ptr);
    assert(saved_solns_heap_ptr <= MR_sol_hp);
    if (catch_code_model == MR_MODEL_NON_HANDLER) {
        // If the code inside the try (catch) was nondet, then its caller
        // (which may be solutions/2) may have put some more stuff on the
        // solutions-heap after the goal succeeded; the goal may have only
        // thrown after being re-entered on backtracking. Thus we can only
        // reset the solutions heap to where it was before copying the
        // exception object to it.
        MR_sol_hp = saved_solns_heap_ptr;
    } else {
        // If the code inside the try (catch) was det or semidet,
        // we can safely reset the solutions heap to where
        // it was when it try (catch) was entered.
        MR_sol_hp = MR_EXCEPTION_STRUCT->MR_excp_solns_heap_ptr;
    }
}
#endif // !defined(MR_CONSERVATIVE_GC)

    // Pop the final exception handler frame off the nondet stack,
    // and reset the nondet stack top. (This must be done last,
    // since it invalidates all the framevars.)
    MR_maxfr_word = MR_prevfr_slot_word(MR_curfr);
    MR_curfr_word = MR_succfr_slot_word(MR_curfr);

    // Now longjmp to the catch, which will invoke the handler that we found.

#ifdef  MR_DEBUG_JMPBUFS
    fprintf(stderr, ""throw catch_code_model %d\\n"", catch_code_model);
#endif

    if (catch_code_model == MR_C_LONGJMP_HANDLER) {
#ifdef  MR_DEBUG_JMPBUFS
        fprintf(stderr, ""throw longjmp %p\\n"",
            *(MR_ENGINE(MR_eng_jmp_buf)));
#endif

        MR_ENGINE(MR_eng_exception) = (MR_Word *) exception;
        MR_save_registers();
        longjmp(*(MR_ENGINE(MR_eng_jmp_buf)), 1);
    }

    // Otherwise, the handler is a Mercury closure.
    // Invoke the handler as `Handler(Exception, Result)'.

#ifdef  MR_DEEP_PROFILING
    MR_fatal_error(""builtin_throw cannot (yet) invoke""
        "" Mercury handlers in deep profiling grades"");
#endif

    MR_r1 = handler;    // get the Handler closure
    MR_r2 = 1;          // One additional input argument
    MR_r3 = exception;  // This is our one input argument

    // Restore the value of MR_trace_from_full that we saved at the
    // start of builtin_catch.
    MR_trace_from_full = trace_from_full;

    // If the catch was semidet, we need to set the success indicator
    // MR_r1 to MR_TRUE and return the result in MR_r2; otherwise, we return
    // the result in MR_r1, which is where mercury__do_call_closure_compact
    // puts it, so we can do a tailcall.
    if (catch_code_model != MR_MODEL_SEMI_HANDLER) {
        MR_tailcall(MR_ENTRY(mercury__do_call_closure_compact),
            MR_ENTRY(mercury__exception__builtin_throw_1_0));
    }
    MR_incr_sp_push_msg(1, ""pred builtin_throw/1"");
    MR_stackvar(1) = MR_succip_word;
    MR_call(MR_ENTRY(mercury__do_call_closure_compact),
        MR_LABEL(mercury__exception__builtin_throw_1_0_i1),
        MR_ENTRY(mercury__exception__builtin_throw_1_0));
}
MR_define_label(mercury__exception__builtin_throw_1_0_i1);
    MR_update_prof_current_proc(
        MR_LABEL(mercury__exception__builtin_throw_1_0));
    // We have just returned from mercury__do_call_closure_compact.
    MR_r2 = MR_r1;
    MR_r1 = MR_TRUE;
    MR_succip_word = MR_stackvar(1);
    MR_decr_sp_pop_msg(1);
    MR_proceed(); // Return to the caller of `builtin_catch'.

MR_END_MODULE

#endif // ! MR_HIGHLEVEL_CODE

// Ensure that the initialization code for the above module gets run.
/*
INIT mercury_sys_init_exceptions
*/

void
mercury_sys_init_exceptions_init(void)
{
#ifndef MR_HIGHLEVEL_CODE
    hand_written_exception_module();
#endif
}

void
mercury_sys_init_exceptions_init_type_tables(void)
{
    // No types to register.
}

#ifdef  MR_DEEP_PROFILING
void
mercury_sys_init_exceptions_write_out_proc_statics(FILE *deep_fp,
    FILE *procrep_fp)
{
    MR_write_out_user_proc_static(deep_fp, procrep_fp,
        &MR_proc_layout_user_name(exception, builtin_catch, 3, 0));
    MR_write_out_user_proc_static(deep_fp, procrep_fp,
        &MR_proc_layout_user_name(exception, builtin_catch, 3, 1));
    MR_write_out_user_proc_static(deep_fp, procrep_fp,
        &MR_proc_layout_user_name(exception, builtin_catch, 3, 2));
    MR_write_out_user_proc_static(deep_fp, procrep_fp,
        &MR_proc_layout_user_name(exception, builtin_catch, 3, 3));
    MR_write_out_user_proc_static(deep_fp, procrep_fp,
        &MR_proc_layout_user_name(exception, builtin_catch, 3, 4));
    MR_write_out_user_proc_static(deep_fp, procrep_fp,
        &MR_proc_layout_user_name(exception, builtin_catch, 3, 5));
    MR_write_out_user_proc_static(deep_fp, procrep_fp,
        &MR_proc_layout_user_name(exception, builtin_throw, 1, 0));
}
#endif

").

%---------------------------------------------------------------------------%

:- pragma foreign_export("C", report_uncaught_exception(in, di, uo),
    "ML_report_uncaught_exception").
:- pragma foreign_export("C#", report_uncaught_exception(in, di, uo),
    "ML_report_uncaught_exception").
:- pragma foreign_export("Java", report_uncaught_exception(in, di, uo),
    "ML_report_uncaught_exception").

:- pred report_uncaught_exception(univ::in, io::di, io::uo) is cc_multi.

report_uncaught_exception(Exception, !IO) :-
    try_io(report_uncaught_exception_2(Exception), Result, !IO),
    (
        Result = succeeded(_)
    ;
        Result = exception(_)
        % If we got a further exception while trying to report
        % the uncaught exception, just ignore it.
    ).

:- pred report_uncaught_exception_2(univ::in, unit::out,
    io::di, io::uo) is det.

report_uncaught_exception_2(Exception, unit, !IO) :-
    io.output_stream(CurOutStream, !IO),
    io.flush_output(CurOutStream, !IO),
    io.stderr_stream(StdErrStream, !IO),
    io.write_string(StdErrStream, "Uncaught Mercury exception:\n", !IO),
    io.write_string(StdErrStream, exception_to_string(Exception), !IO),
    io.nl(StdErrStream, !IO),
    io.flush_output(StdErrStream, !IO).

:- func exception_to_string(univ) = string.

:- pragma foreign_export("Java", exception_to_string(in) = out,
    "ML_exception_to_string").

exception_to_string(Exception) = Message :-
    ( if univ_to_type(Exception, software_error(MessagePrime)) then
        Message = "Software Error: " ++ MessagePrime
    else
        Message = string(univ_value(Exception))
    ).

:- initialise(set_get_message_hook/2).

:- pred set_get_message_hook(io::di, io::uo) is det.

set_get_message_hook(!IO).

:- pragma foreign_proc("Java",
    set_get_message_hook(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    jmercury.runtime.Exception.getMessageHook =
        new jmercury.runtime.MethodPtr1() {
            public java.lang.Object call___0_0(java.lang.Object arg1) {
                return ML_exception_to_string((univ.Univ_0) arg1);
            }
        };
    IO = IO0;
").

%---------------------------------------------------------------------------%

    % The Java runtime system sometimes wants to report exceptions. Create
    % a reference that it can use to call library code to report exceptions.
    %
:- pragma foreign_code("Java", "
public static class ReportUncaughtException
        implements jmercury.runtime.JavaInternal.ExceptionReporter
{
    public void reportUncaughtException(jmercury.runtime.Exception e)
    {
        ML_report_uncaught_exception((univ.Univ_0) e.exception);
    }
}

static {
    jmercury.runtime.JavaInternal.setExceptionReporter(
        new ReportUncaughtException());
}
").

%---------------------------------------------------------------------------%
:- end_module exception.
%---------------------------------------------------------------------------%
