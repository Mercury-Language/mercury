%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test parsing of invalid binary litrals.
%
%---------------------------------------------------------------------------%

:- module invalid_binary_literal.
:- interface.

:- func foo1 = int.
:- func foo2 = int.
:- func foo3 = int.
:- func foo4 = int.
:- func foo5 = int.
:- func foo6 = int.
:- func foo7 = int.
:- func foo8 = int.

:- implementation.

foo1 = 0b.

foo2 = -0b.

foo3 = 0b_.

foo4 = -0b_.

foo5 = 0b11_.

foo6 = -0b11_.

foo7 = 0_b11.

foo8 = -0_b11.
