%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% any_util.m
% Ralph Becket <rafe@cs.mu.OZ.AU>
%
% Utility predicates for use with inst any values.
%
%---------------------------------------------------------------------------%

:- module any_util.

:- interface.

:- import_module list.

    % This predicate is useful for converting polymorphic non-solver type
    % values with inst any to inst ground (the compiler recognises that inst
    % any is equivalent to ground for non-polymorphic non-solver types).
    %
    % DON'T call this on solver type values unless you are absolutely sure they
    % are semantically ground.
    %
:- pred unsafe_cast_to_ground(T::(any >> ground)) is det.

    % impure_unsorted_solutions is similar to std_util.unsorted_solutions.
    % The closure argument is impure because in most cases it will be
    % placing constraints on non-local variables.
    %
:- impure pred impure_unsorted_solutions(impure pred(T), list(T)).
:-        mode impure_unsorted_solutions(pred(oa) is nondet, oa) is cc_multi.
:-        mode impure_unsorted_solutions(pred(oa) is multi,  oa) is cc_multi.

    % impure_unsorted_aggregate is similar to std_util.unsorted_aggregate.
    % The closure argument is impure because in most cases it will be
    % placing constraints on non-local variables.
    %
:- impure pred impure_unsorted_aggregate(impure pred(T),
    impure pred(T, U, U), U, U).
:- mode impure_unsorted_aggregate(pred(out) is nondet,
    pred(in, in, out) is det, in, out) is cc_multi.
:- mode impure_unsorted_aggregate(pred(oa) is nondet,
    pred(ia, ia, oa) is det, ia, oa) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- use_module    any_list.
:- import_module solutions.

%---------------------------------------------------------------------------%

impure_unsorted_solutions(P0::in(pred(oa) is nondet), Xs::oa) :-
    P = ( pred(X::out) is nondet :-
            promise_pure impure P0(X),
            unsafe_cast_to_ground(X)
        ),
    unsorted_solutions(P, Xs).
impure_unsorted_solutions(P0::in(pred(oa) is multi), Xs::oa) :-
    P = ( pred(X::out) is multi :-
            promise_pure impure P0(X),
            unsafe_cast_to_ground(X)
        ),
    unsorted_solutions(P, Xs).

impure_unsorted_aggregate(P0::in(pred(out) is nondet),
        A0::in(pred(in, in, out) is det), In::in, Out::out) :-
    P = ( pred(T::out) is nondet :-
            promise_pure impure P0(T)
        ),
    A = ( pred(T::in, U0::in, U::out) is det :-
            promise_pure impure A0(T, U0, U)
        ),
    unsorted_aggregate(P, A, In, Out).
impure_unsorted_aggregate(P0::in(pred(oa) is nondet),
        A0::in(pred(ia, ia, oa) is det), In::ia, Out::oa) :-
    P = ( pred(T::out) is nondet :-
            promise_pure impure P0(T),
            unsafe_cast_to_ground(T)
        ),
    A = ( pred(T::in, U0::in, U::out) is det :-
            promise_pure impure A0(T, U0, U),
            unsafe_cast_to_ground(U)
        ),
    unsafe_cast_to_ground(In),
    unsorted_aggregate(P, A, In, Out).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    unsafe_cast_to_ground(_X::(any >> ground)),
    [promise_pure, will_not_call_mercury, thread_safe, will_not_modify_trail],
"
// No code needed.
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
