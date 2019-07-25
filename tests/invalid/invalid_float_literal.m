%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test parsing of invalid float literals.
%
%---------------------------------------------------------------------------%

:- module invalid_float_literal.
:- interface.

:- func foo1 = float.
:- func foo2 = float.
:- func foo3 = float.
:- func foo4 = float.
:- func foo5 = float.
:- func foo6 = float.
:- func foo7 = float.
:- func foo8 = float.
:- func foo9 = float.
:- func foo10 = float.
:- func foo11 = float.
:- func foo12 = float.

:- implementation.

foo1 = 1_2_3.1_2_3_.

foo2 = 1_2_3e1_2_3_.

foo3 = 123_e12.

foo4 = 123e_12.

foo5 = 123_._123.

foo6 = 123._123.

foo7 = 123_.123.

foo8 = 123.12e_-12.

foo9 = 123.12e_+12.

foo10 = 123_e12.

foo11 = 123.12e-_12.

foo12 = 123.12e+_12.
