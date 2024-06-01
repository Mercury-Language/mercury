%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module duplicate_mutable.

:- interface.

:- type foo.

:- implementation.

:- type foo == int.

:- mutable(global, int, 42, ground, [untrailed]).

:- mutable(global, int, 43, ground, [untrailed]).
