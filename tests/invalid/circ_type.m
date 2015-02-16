%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test for circular equivalence types.

:- module circ_type.

:- interface.

:- type circ == circ.

:- type circ1 == circ2.

:- type circ2 == circ1.
