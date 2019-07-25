%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test parsing of invalid decimal integer literals.
%
%---------------------------------------------------------------------------%

:- module invalid_decimal_literal.
:- interface.

:- func foo1 = int.
:- func foo2 = int.

:- implementation.

foo1 = 561_.

foo2 = -444_.
