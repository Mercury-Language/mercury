%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test for circular equivalence modes.

:- module circ_mode.

:- interface.

:- mode circ == circ.

:- mode circ1 == circ2.

:- mode circ2 == circ1.
