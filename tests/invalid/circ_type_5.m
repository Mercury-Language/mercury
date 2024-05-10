%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This corresponds to circ_inst_5.
%

:- module circ_type_5.
:- interface.

:- type i(I) == I.
:- type c(I) == i(c(I)).
:- type c == c(int).
