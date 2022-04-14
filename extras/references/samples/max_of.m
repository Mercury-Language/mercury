%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1998, 2010 The University of Melbourne.
% Copyright (C) 2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: max_of.m.
% Authors: pets (Peter Schachte).
% Stability: low.
% Purpose: demonstration of nb_reference type.
%
% This module defines a predicate max_of/2 that is equivalent to
%
%   max_of(Pred, Max) :-
%       unsorted_solutions(Pred, List),
%       List = [First | Rest],
%       list.foldl((pred(X, Y, Z) is det :- max(X, Y, Z)), Rest, First, Max).
%
% but which is potentially more efficient, because it avoids building a
% list of solutions.
%
%-----------------------------------------------------------------------------%

:- module max_of.
:- interface.

:- pred max_of(pred(int), int).
:- mode max_of(pred(out) is nondet, out) is semidet.
:- mode max_of(pred(out) is multi, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module nb_reference.
:- import_module bool.
:- import_module int.

%-----------------------------------------------------------------------------%

% This implementation uses two non-backtrackable references, one to keep
% track of whether or not we've had any solutions, and the other to store the
% max "so far." For each solution we find, if it's the first, we set the max
% so far to it, and we record that we've had some solutions. If not the
% first solution, then we update the max if the new solution is larger than
% the max so far. When we've found all the solutions, we make sure we've
% found at least one solution, and then return the max so far as the result.
%
% There is one difficulty implementing this predicate. When the Pred
% argument is a multi closure, we want max_of to be det. But when Pred is
% nondet, we must check to make sure than we have had any solutions; if not,
% max_of/2 must fail.  Unfortunately, the Mercury compiler can't determine
% that when Pred is multi, the test will always succeed, so the determinacy
% checker complains that max_of/2 in that mode is actually semidet.  We work
% around that with the min_solutions/1 predicate, which is implemented with
% C code.  This allows us to have different code for different modes, which
% allows us to work around the problem. It would be much more convenient if
% Mercury allowed us to have different code for different modes of a
% predicate implemented in Mercury.

:- pragma promise_pure(pred(max_of/2)).

max_of(Pred, Max) :-
    impure new_nb_reference(no, Someref),
    impure new_nb_reference(0, Maxref),
    (
        Pred(Value),
        semipure value(Someref, Some),
        (
            Some = no,
            impure update(Someref, yes),
            impure update(Maxref, Value)
        ;
            Some = yes,
            semipure value(Maxref, Prev),
            ( if Value > Prev then
                impure update(Maxref, Value)
            else
                true
            )
        ),
        fail
    ;
        impure min_solutions(Pred, MinSolutions),
        (
            MinSolutions = 1
        ;
            semipure value(Someref, yes)
        ),
        semipure value(Maxref, Max)
    ).

:- impure pred min_solutions(pred(T), int).
:- mode min_solutions(pred(out) is nondet, out(bound(0))) is det.
:- mode min_solutions(pred(out) is multi, out(bound(1))) is det.

:- pragma foreign_proc("C",
    min_solutions(_Pred::(pred(out) is nondet), Res::out(bound(0))),
    [will_not_call_mercury],
"
    Res = 0;
").

:- pragma foreign_proc("C",
    min_solutions(_Pred::(pred(out) is multi), Res::out(bound(1))),
    [will_not_call_mercury],
"
    Res = 1;
").

%-----------------------------------------------------------------------------%
:- end_module max_of.
%-----------------------------------------------------------------------------%
