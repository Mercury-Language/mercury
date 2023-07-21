%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module kind.
:- interface.

:- type t(K1)
    --->    t(K1(int)).

:- typeclass tc(K2) where [
    func f(K2(string)) = string,
    func g(t(K2)) = int
].

:- inst i(K3)
    --->    t(K3(ground)).
:- mode m(K4) == free >> bound(t(K4(ground))).

:- inst t == (pred(K5, in(K6)) is det).
:- mode u == in(pred(K7, in) is det).
:- mode v == in(pred(p, in(q), in) is det).

:- type f
    --->    f1(int, float).

:- inst fi for f/0
    --->    f1(f9, ground).
