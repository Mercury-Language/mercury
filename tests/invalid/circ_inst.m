%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test for circular equivalence insts.

:- module circ_inst.

:- interface.

:- inst circ == circ.

:- inst circ1 == circ2.

:- inst circ2 == circ1.
