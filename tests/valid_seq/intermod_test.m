%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test overloading resolution for cross-module optimization.
%

:- module intermod_test.
:- interface.

:- import_module int.

:- pred p(int::out) is semidet.

:- type t
    --->    f(int)
    ;       g.

:- implementation.

:- import_module intermod_test2.

:- pragma inline(p/1).
p(X) :-
    Y = f(1),
    Y = f(_),
    Lambda = (pred(Z::int_mode) is det :- Z = 2),
    local(Lambda, X),
    intermod_test2.baz(X).

:- mode int_mode == out.

:- pred local(pred(int), int).
:- mode local(pred(int_mode) is det, out) is det.

local(Pred, Int) :-
    call(Pred, Int).

:- pred local_2(pred(int), int).
:- mode local_2(pred(int_mode) is det, out) is det.

local_2(Pred, plusone(Int)) :-
    call(Pred, Int).
