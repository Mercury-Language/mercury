% This corresponds to circ_inst5.
:- module circ_type5.
:- interface.

:- type i(I) == I.
:- type c(I) == i(c(I)).
:- type c == c(int).

