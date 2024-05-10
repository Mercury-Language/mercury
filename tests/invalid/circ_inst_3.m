%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test for circular equivalence insts.
% This is the same as circ_inst_1.m,
% except that we also have a predicate which makes use of the insts.
%

:- module circ_inst_3.

:- interface.

:- inst circ == circ.

:- inst circ1 == circ2.

:- inst circ2 == circ1.

:- pred p(int, int, int, int).
:- mode p(in(circ), out(circ), in(circ1), out(circ2)) is det.

:- implementation.

p(X, X, Y, Y).
