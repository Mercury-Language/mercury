%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test parsing of invalid octal integer literals.
%
%---------------------------------------------------------------------------%

:- module invalid_octal_literal.
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

foo1 = 0o.

foo2 = -0o.

foo3 = 0o_.

foo4 = -0o_.

foo5 = 0o77_.

foo6 = -0o77_.

foo7 = 0_o77.

foo8 = -0_o77.
