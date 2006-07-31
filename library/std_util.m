%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: std_util.m.
% Main author: fjh.
% Stability: high.
% 
% This file contains higher-order programming constructs and other 
% useful standard utilities.
% 
% NOTE: fomerly this module contained considerably more functionality but
%       most of that has now been moved to other modules.  In particular:
%
%       * the `univ' type is now defined in the module univ.
%       
%       * the `maybe' and `maybe_error' types are now defined in the module
%         maybe.
%
%       * the `unit' type is now defined in the module unit.
%
%       * the `pair' type is now defined in the module pair.
% 
% This module also used to define a number of RTTI access predicates.  These
% are now to be found in the modules type_desc, construct and deconstruct.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module std_util.
:- interface.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module set.

%-----------------------------------------------------------------------------%
%
% General purpose higher-order programming constructs
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
%
% All-solutions predicates
%

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
%-----------------------------------------------------------------------------%

:- implementation.

:- use_module solutions.

%-----------------------------------------------------------------------------%

maybe_pred(Pred, X, Y) :-
    Y = ( Pred(X, Z) -> yes(Z) ; no ).

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

%-----------------------------------------------------------------------------%
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

solutions(P) = solutions.solutions(P).

solutions_set(P) = solutions.solutions_set(P).

aggregate(P, F, Acc0) = solutions.aggregate(P, F, Acc0).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
