%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bad_mutable.

:- interface.

:- type export_something == int.
:- implementation.

:- mutable(not_a_type, no_type, 0, ground, [untrailed]).

:- mutable(not_an_inst, int, 0, special_ground, [untrailed]).

:- type t1
    --->    f1(int, int).
:- inst t1
    --->    f1(ground, unique).

:- mutable(non_ground, t1, f1(41, 43), t1, [untrailed]).
