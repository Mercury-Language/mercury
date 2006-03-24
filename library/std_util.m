%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: std_util.m.
% Main author: fjh.
% Stability: medium.

% This file is intended for all the useful standard utilities
% that don't belong elsewhere, like <stdlib.h> in C.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module std_util.
:- interface.

:- import_module bool.
:- import_module list.
:- import_module set.
:- import_module type_desc.

%-----------------------------------------------------------------------------%
%
% The universal type `univ'
%

    % An object of type `univ' can hold the type and value of an object of any
    % other type.
:- type univ.

    % type_to_univ(Object, Univ):
    %
    % True iff the type stored in `Univ' is the same as the type of `Object',
    % and the value stored in `Univ' is equal to the value of `Object'.
    %
    % Operational, the forwards mode converts an object to type `univ',
    % while the reverse mode converts the value stored in `Univ'
    % to the type of `Object', but fails if the type stored in `Univ'
    % does not match the type of `Object'.
    %
:- pred type_to_univ(T, univ).
:- mode type_to_univ(di, uo) is det.
:- mode type_to_univ(in, out) is det.
:- mode type_to_univ(out, in) is semidet.

    % univ_to_type(Univ, Object) :- type_to_univ(Object, Univ).
    %
:- pred univ_to_type(univ, T).
:- mode univ_to_type(in, out) is semidet.
:- mode univ_to_type(out, in) is det.
:- mode univ_to_type(uo, di) is det.

    % The function univ/1 provides the same functionality as type_to_univ/2.
    % univ(Object) = Univ :- type_to_univ(Object, Univ).
    %
:- func univ(T) = univ.
:- mode univ(in) = out is det.
:- mode univ(di) = uo is det.
:- mode univ(out) = in is semidet.

    % det_univ_to_type(Univ, Object):
    %
    % The same as the forwards mode of univ_to_type, but aborts
    % if univ_to_type fails.
    %
:- pred det_univ_to_type(univ::in, T::out) is det.

    % univ_type(Univ):
    %
    % Returns the type_desc for the type stored in `Univ'.
    %
:- func univ_type(univ) = type_desc.type_desc.

    % univ_value(Univ):
    %
    % Returns the value of the object stored in Univ.
    %
:- some [T] func univ_value(univ) = T.

%-----------------------------------------------------------------------------%
%
% The "maybe" type
%

:- type maybe(T) ---> no ; yes(T).
:- inst maybe(I) ---> no ; yes(I).

:- type maybe_error ---> ok ; error(string).
:- type maybe_error(T) ---> ok(T) ; error(string).
:- inst maybe_error(I) ---> ok(I) ; error(ground).

    % map_maybe(P, yes(Value0), yes(Value)) :- P(Value, Value).
    % map_maybe(_, no, no).
    %
:- pred map_maybe(pred(T, U), maybe(T), maybe(U)).
:- mode map_maybe(pred(in, out) is det, in, out) is det.
:- mode map_maybe(pred(in, out) is semidet, in, out) is semidet.
:- mode map_maybe(pred(in, out) is multi, in, out) is multi.
:- mode map_maybe(pred(in, out) is nondet, in, out) is nondet.

    % map_maybe(F, yes(Value)) = yes(F(Value)).
    % map_maybe(_, no) = no.
    %
:- func map_maybe(func(T) = U, maybe(T)) = maybe(U).

    % fold_maybe(P, yes(Value), Acc0, Acc) :- P(Value, Acc0, Acc).
    % fold_maybe(_, no, Acc, Acc).
    %
:- pred fold_maybe(pred(T, U, U), maybe(T), U, U).
:- mode fold_maybe(pred(in, in, out) is det, in, in, out) is det.
:- mode fold_maybe(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode fold_maybe(pred(in, di, uo) is det, in, di, uo) is det.

    % fold_maybe(F, yes(Value), Acc0) = F(Acc0).
    % fold_maybe(_, no, Acc) = Acc.
    %
:- func fold_maybe(func(T, U) = U, maybe(T), U) = U.

    % map_fold_maybe(P, yes(Value0), yes(Value), Acc0, Acc) :-
    %       P(Value, Value, Acc0, Acc).
    % map_fold_maybe(_, no, no, Acc, Acc).
    %
:- pred map_fold_maybe(pred(T, U, Acc, Acc), maybe(T), maybe(U), Acc, Acc).
:- mode map_fold_maybe(pred(in, out, in, out) is det, in, out, in, out) is det.
:- mode map_fold_maybe(pred(in, out, di, uo) is det, in, out, di, uo) is det.

    % As above, but with two accumulators.
    %
:- pred map_fold2_maybe(pred(T, U, Acc1, Acc1, Acc2, Acc2),
    maybe(T), maybe(U), Acc1, Acc1, Acc2, Acc2).
:- mode map_fold2_maybe(pred(in, out, in, out, in, out) is det, in, out,
    in, out, in, out) is det.
:- mode map_fold2_maybe(pred(in, out, in, out, di, uo) is det,
    in, out, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%
% The "unit" type - stores no information at all.
%

:- type unit        --->    unit.

:- type unit(T)     --->    unit1.

%-----------------------------------------------------------------------------%
%
% The "pair" type.  Useful for many purposes.
%

:- type pair(T1, T2)
    --->    (T1 - T2).
:- type pair(T) ==  pair(T, T).

:- inst pair(I1, I2)
    --->    (I1 - I2).
:- inst pair(I) ==  pair(I, I).

    % Return the first element of the pair.
    %
:- pred fst(pair(X, Y)::in, X::out) is det.
:- func fst(pair(X, Y)) = X.

    % Return the second element of the pair.
    %
:- pred snd(pair(X, Y)::in, Y::out) is det.
:- func snd(pair(X, Y)) = Y.

:- func pair(T1, T2) = pair(T1, T2).

%-----------------------------------------------------------------------------%

% NOTE: the procedures in this section are all obsolete and will be deleted
%       in a later release.  New code should use the versions defined in
%       solutions.m instead.

    % solutions/2 collects all the solutions to a predicate and returns
    % them as a list in sorted order, with duplicates removed.
    % solutions_set/2 returns them as a set.  unsorted_solutions/2 returns
    % them as an unsorted list with possible duplicates; since there are an
    % infinite number of such lists, this must be called from a context in
    % which only a single solution is required.
    %
:- pragma obsolete(solutions/2).
:- pred solutions(pred(T), list(T)).
:- mode solutions(pred(out) is multi, out(non_empty_list)) is det.
:- mode solutions(pred(out) is nondet, out) is det.

:- pragma obsolete(solutions/1).
:- func solutions(pred(T)) = list(T).
:- mode solutions(pred(out) is multi) = out(non_empty_list) is det.
:- mode solutions(pred(out) is nondet) = out is det.

:- pragma obsolete(solutions_set/2).
:- pred solutions_set(pred(T), set(T)).
:- mode solutions_set(pred(out) is multi, out) is det.
:- mode solutions_set(pred(out) is nondet, out) is det.

:- pragma obsolete(solutions_set/1).
:- func solutions_set(pred(T)) = set(T).
:- mode solutions_set(pred(out) is multi) = out is det.
:- mode solutions_set(pred(out) is nondet) = out is det.

:- pragma obsolete(unsorted_solutions/2).
:- pred unsorted_solutions(pred(T), list(T)).
:- mode unsorted_solutions(pred(out) is multi, out(non_empty_list))
    is cc_multi.
:- mode unsorted_solutions(pred(out) is nondet, out) is cc_multi.

:- pragma obsolete(aggregate/3).
:- func aggregate(pred(T), func(T, U) = U, U) = U.
:- mode aggregate(pred(out) is multi, func(in, in) = out is det, in)
    = out is det.
:- mode aggregate(pred(out) is nondet, func(in, in) = out is det, in)
    = out is det.

%-----------------------------------------------------------------------------%

    % aggregate/4 generates all the solutions to a predicate,
    % sorts them and removes duplicates, then applies an accumulator
    % predicate to each solution in turn:
    %
    % aggregate(Generator, Accumulator, Acc0, Acc) <=>
    %   solutions(Generator, Solutions),
    %   list.foldl(Accumulator, Solutions, Acc0, Acc).
    %
:- pragma obsolete(aggregate/4).
:- pred aggregate(pred(T), pred(T, U, U), U, U).
:- mode aggregate(pred(out) is multi, pred(in, in, out) is det,
    in, out) is det.
:- mode aggregate(pred(out) is multi, pred(in, di, uo) is det,
    di, uo) is det.
:- mode aggregate(pred(out) is nondet, pred(in, di, uo) is det,
    di, uo) is det.
:- mode aggregate(pred(out) is nondet, pred(in, in, out) is det,
    in, out) is det.

    % aggregate2/6 generates all the solutions to a predicate,
    % sorts them and removes duplicates, then applies an accumulator
    % predicate to each solution in turn:
    %
    % aggregate2(Generator, Accumulator, AccA0, AccA, AccB0, AccB) <=>
    %   solutions(Generator, Solutions),
    %   list.foldl2(Accumulator, Solutions, AccA0, AccA, AccB0, AccB).
    %
:- pragma obsolete(aggregate2/6).
:- pred aggregate2(pred(T), pred(T, U, U, V, V), U, U, V, V).
:- mode aggregate2(pred(out) is multi, pred(in, in, out, in, out) is det,
    in, out, in, out) is det.
:- mode aggregate2(pred(out) is multi, pred(in, in, out, di, uo) is det,
    in, out, di, uo) is det.
:- mode aggregate2(pred(out) is nondet, pred(in, in, out, di, uo) is det,
    in, out, di, uo) is det.
:- mode aggregate2(pred(out) is nondet, pred(in, in, out, in, out) is det,
    in, out, in, out) is det.

    % unsorted_aggregate/4 generates all the solutions to a predicate
    % and applies an accumulator predicate to each solution in turn.
    % Declaratively, the specification is as follows:
    %
    % unsorted_aggregate(Generator, Accumulator, Acc0, Acc) <=>
    %   unsorted_solutions(Generator, Solutions),
    %   list.foldl(Accumulator, Solutions, Acc0, Acc).
    %
    % Operationally, however, unsorted_aggregate/4 will call the
    % Accumulator for each solution as it is obtained, rather than
    % first building a list of all the solutions.
    %
:- pragma obsolete(unsorted_aggregate/4).
:- pred unsorted_aggregate(pred(T), pred(T, U, U), U, U).
:- mode unsorted_aggregate(pred(out) is multi, pred(in, in, out) is det,
    in, out) is cc_multi.
:- mode unsorted_aggregate(pred(out) is multi, pred(in, in, out) is cc_multi,
    in, out) is cc_multi.
:- mode unsorted_aggregate(pred(out) is multi, pred(in, di, uo) is det,
    di, uo) is cc_multi.
:- mode unsorted_aggregate(pred(out) is multi, pred(in, di, uo) is cc_multi,
    di, uo) is cc_multi.
:- mode unsorted_aggregate(pred(muo) is multi, pred(mdi, di, uo) is det,
    di, uo) is cc_multi.
:- mode unsorted_aggregate(pred(out) is nondet, pred(in, di, uo) is det,
    di, uo) is cc_multi.
:- mode unsorted_aggregate(pred(out) is nondet, pred(in, di, uo) is cc_multi,
    di, uo) is cc_multi.
:- mode unsorted_aggregate(pred(out) is nondet, pred(in, in, out) is det,
    in, out) is cc_multi.
:- mode unsorted_aggregate(pred(out) is nondet, pred(in, in, out) is cc_multi,
    in, out) is cc_multi.
:- mode unsorted_aggregate(pred(muo) is nondet, pred(mdi, di, uo) is det,
    di, uo) is cc_multi.

    % This is a generalization of unsorted_aggregate which allows the
    % iteration to stop before all solutions have been found.
    % Declaratively, the specification is as follows:
    %
    %   do_while(Generator, Filter, !Acc) :-
    %       unsorted_solutions(Generator, Solutions),
    %       do_while_2(Solutions, Filter, !Acc).
    %
    %   do_while_2([], _, !Acc).
    %   do_while_2([X | Xs], Filter, !Acc) :-
    %       Filter(X, More, !Acc),
    %       ( More = yes ->
    %           do_while_2(Xs, Filter, !Acc)
    %       ;
    %           true
    %       ).
    %
    % Operationally, however, do_while/4 will call the Filter
    % predicate for each solution as it is obtained, rather than
    % first building a list of all the solutions.
    %
:- pragma obsolete(do_while/4).
:- pred do_while(pred(T), pred(T, bool, T2, T2), T2, T2).
:- mode do_while(pred(out) is multi, pred(in, out, in, out) is det, in, out)
    is cc_multi.
:- mode do_while(pred(out) is multi, pred(in, out, di, uo) is det, di, uo)
    is cc_multi.
:- mode do_while(pred(out) is multi, pred(in, out, di, uo) is cc_multi, di, uo)
    is cc_multi.
:- mode do_while(pred(out) is nondet, pred(in, out, in, out) is det, in, out)
    is cc_multi.
:- mode do_while(pred(out) is nondet, pred(in, out, di, uo) is det, di, uo)
    is cc_multi.
:- mode do_while(pred(out) is nondet, pred(in, out, di, uo) is cc_multi, di, uo)
    is cc_multi.

%-----------------------------------------------------------------------------%
%
% General purpose higher-order programming constructs.
%

    % compose(F, G, X) = F(G(X))
    %
    % Function composition.
    % XXX It would be nice to have infix `o' or somesuch for this.
    %
:- func compose(func(T2) = T3, func(T1) = T2, T1) = T3.

    % converse(F, X, Y) = F(Y, X).
    %
:- func converse(func(T1, T2) = T3, T2, T1) = T3.

    % pow(F, N, X) = F^N(X)
    %
    % Function exponentiation.
    %
:- func pow(func(T) = T, int, T) = T.

    % The identity function.
    %
:- func id(T) = T.

%-----------------------------------------------------------------------------%

    % maybe_pred(Pred, X, Y) takes a closure Pred which transforms an
    % input semideterministically. If calling the closure with the input
    % X succeeds, Y is bound to `yes(Z)' where Z is the output of the
    % call, or to `no' if the call fails.
    %
:- pred maybe_pred(pred(T1, T2), T1, maybe(T2)).
:- mode maybe_pred(pred(in, out) is semidet, in, out) is det.

:- func maybe_func(func(T1) = T2, T1) = maybe(T2).
:- mode maybe_func(func(in) = out is semidet, in) = out is det.

%-----------------------------------------------------------------------------%

    % isnt(Pred, X) <=> not Pred(X)
    %
    % This is useful in higher order programming, e.g.
    %   Odds  = list.filter(odd, Xs)
    %   Evens = list.filter(isnt(odd), Xs)
    %
:- pred isnt(pred(T)::(pred(in) is semidet), T::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module construct.
:- import_module deconstruct.
:- import_module int.
:- import_module require.
:- import_module set.
:- import_module string.

:- use_module private_builtin. % for the `heap_pointer' type.
:- use_module solutions.

% XXX This should not be necessary, but the current compiler is broken in that
% it puts foreign_proc clauses into deconstruct.opt without also putting the
% foreign_decl they require into deconstruct.opt as well.

:- pragma foreign_decl("C", "

#include ""mercury_deconstruct.h""
#include ""mercury_deconstruct_macros.h""

").

%-----------------------------------------------------------------------------%

map_maybe(_, no, no).
map_maybe(P, yes(T0), yes(T)) :- P(T0, T).

map_maybe(_, no) = no.
map_maybe(F, yes(T)) = yes(F(T)).

fold_maybe(P, yes(Value), Acc0, Acc) :- P(Value, Acc0, Acc).
fold_maybe(_, no, Acc, Acc).

fold_maybe(F, yes(Value), Acc0) = F(Value, Acc0).
fold_maybe(_, no, Acc) = Acc.

map_fold_maybe(_, no, no, Acc, Acc).
map_fold_maybe(P, yes(T0), yes(T), Acc0, Acc) :-
    P(T0, T, Acc0, Acc).

map_fold2_maybe(_, no, no, A, A, B, B).
map_fold2_maybe(P, yes(T0), yes(T), A0, A, B0, B) :-
    P(T0, T, A0, A, B0, B).

%   Is this really useful?
% % for use in lambda expressions where the type of functor '-' is ambiguous
% :- pred pair(X, Y, pair(X, Y)).
% :- mode pair(in, in, out) is det.
% :- mode pair(out, out, in) is det.
%
% pair(X, Y, X-Y).

fst(X-_Y) = X.
fst(P, X) :-
    X = fst(P).

snd(_X-Y) = Y.
snd(P, X) :-
    X = snd(P).

maybe_pred(Pred, X, Y) :-
    ( call(Pred, X, Z) ->
        Y = yes(Z)
    ;
        Y = no
    ).

%----------------------------------------------------------------------------%

% NOTE: the implementations for these predicates is in solutions.m.

solutions(Pred, List) :-
    solutions.solutions(Pred, List).

solutions_set(Pred, Set) :-
    solutions.solutions_set(Pred, Set).

unsorted_solutions(Pred, List) :-
    solutions.unsorted_solutions(Pred, List).

aggregate(Generator, Accumulator, !Acc) :-
    solutions.aggregate(Generator, Accumulator, !Acc).

aggregate2(Generator, Accumulator, !Acc1, !Acc2) :-
    solutions.aggregate2(Generator, Accumulator, !Acc1, !Acc2).

unsorted_aggregate(Generator, Accumulator, !Acc) :-
    solutions.unsorted_aggregate(Generator, Accumulator, !Acc).

do_while(GeneratorPred, CollectorPred, Accumulator0, Accumulator) :-
    solutions.do_while(GeneratorPred, CollectorPred,
        Accumulator0, Accumulator).

%-----------------------------------------------------------------------------%

    % We call the constructor for univs `univ_cons' to avoid ambiguity
    % with the univ/1 function which returns a univ.
:- type univ
    --->    some [T] univ_cons(T).

univ_to_type(Univ, X) :- type_to_univ(X, Univ).

univ(X) = Univ :- type_to_univ(X, Univ).

det_univ_to_type(Univ, X) :-
    ( type_to_univ(X0, Univ) ->
        X = X0
    ;
        UnivTypeName = type_desc.type_name(univ_type(Univ)),
        ObjectTypeName = type_desc.type_name(type_desc.type_of(X)),
        string.append_list(["det_univ_to_type: conversion failed\\n",
            "\tUniv Type: ", UnivTypeName,
            "\\n\tObject Type: ", ObjectTypeName], ErrorString),
        error(ErrorString)
    ).

univ_value(univ_cons(X)) = X.

:- pragma promise_pure(type_to_univ/2).

type_to_univ(T::di, Univ::uo) :-
    Univ0 = 'new univ_cons'(T),
    unsafe_promise_unique(Univ0, Univ).
type_to_univ(T::in, Univ::out) :-
    Univ0 = 'new univ_cons'(T),
    unsafe_promise_unique(Univ0, Univ).
type_to_univ(T::out, Univ::in) :-
    Univ = univ_cons(T0),
    private_builtin.typed_unify(T0, T).

univ_type(Univ) = type_desc.type_of(univ_value(Univ)).

:- pred construct_univ(T, univ).
:- mode construct_univ(in, out) is det.
:- pragma export(construct_univ(in, out), "ML_construct_univ").

construct_univ(X, Univ) :-
    Univ = univ(X).

:- some [T] pred unravel_univ(univ, T).
:- mode unravel_univ(in, out) is det.
:- pragma export(unravel_univ(in, out), "ML_unravel_univ").

unravel_univ(Univ, X) :-
    univ_value(Univ) = X.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cam.sri.com> 24/04/99
%   Function forms added.

pair(X, Y) =
    X-Y.

maybe_func(PF, X) =
    ( if Y = PF(X) then yes(Y) else no ).

compose(F, G, X) =
    F(G(X)).

converse(F, X, Y) =
    F(Y, X).

pow(F, N, X) =
    ( if N = 0 then X else pow(F, N - 1, F(X)) ).

isnt(P, X) :-
    not P(X).

id(X) = X.

solutions(P) = solutions.solutions(P).

solutions_set(P) = solutions.solutions_set(P).

aggregate(P, F, Acc0) = solutions.aggregate(P, F, Acc0).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
