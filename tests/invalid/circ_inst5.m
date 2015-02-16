%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module circ_inst5.
:- interface.

:- inst i(I) == I.
:- inst c(I) == i(c(I)).
:- inst c == c(ground).

:- type foo ---> foo.

:- func f(foo) = int.
:- mode f(in(c)) = out is det.

:- implementation.

f(foo) = 1.
