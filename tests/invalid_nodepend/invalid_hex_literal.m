%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test parsing of invalid hex integer literals.
%
%---------------------------------------------------------------------------%

:- module invalid_hex_literal.
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

foo1 = 0x.

foo2 = -0x.

foo3 = 0x_.

foo4 = -0x_.

foo5 = 0xff_.

foo6 = -0xff_.

foo7 = 0_xff.

foo8 = -0_xff.
