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
