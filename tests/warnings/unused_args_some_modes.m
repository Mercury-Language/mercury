%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test how unused_args.m handles arguments whose status (is it used or not,
% and if not, is this expected or not) differs between clauses.
%
%---------------------------------------------------------------------------%

:- module unused_args_some_modes.
:- interface.
:- import_module char.

:- type t
    --->    f1(int)
    ;       f2(char).

:- inst i1 for t/0
    --->    f1(ground).
:- inst i2 for t/0
    --->    f2(ground).

:- pred p(t, int, int, int, int, int).
:- mode p(in(i1), in, in, unused, unused, out) is det.
:- mode p(in(i2), in, in, unused, in, out) is semidet.

:- implementation.

:- pragma promise_equivalent_clauses(pred(p/6)).

% Input arg I's status in the two clauses is unused/used.
% Input arg J's status in the two clauses is unused/unused.
% Input arg K's status in the two clauses is unused/unused, but both expected.
% Input arg L's status in the two clauses is unused/unused, with ONE expected.

p(T::in(i1), _I::in, _J::in, _K::unused, _L::unused, O::out) :-
    % I is unused in this clause.
    % J is unused in this clause.
    % K is unused in this clause, but this is expected.
    % L is unused in this clause, but this is expected.
    T = f1(N),
    O = N.
p(T::in(i2), I::in, _J::in, _K::unused, _L::in, O::out) :-
    % I is used in this clause.
    % J is unused in this clause.
    % K is unused in this clause, but this is expected.
    % L is unused in this clause.
    T = f2(C),
    ( if char.is_alpha(C) then
        O = I
    else
        fail
    ).
