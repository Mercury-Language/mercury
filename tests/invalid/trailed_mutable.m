%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module trailed_mutable.

:- interface.

:- type foo.

:- implementation.

:- type foo == int.

:- mutable(global, int, 42, ground, [trailed]).
