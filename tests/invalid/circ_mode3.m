% Test for circular equivalence modes.
% This is the same as circ_mode.m,
% except that we also have a predicate which makes use of the modes.

:- module circ_mode3.

:- interface.

:- mode circ == circ.

:- mode circ1 == circ2.

:- mode circ2 == circ1.

:- pred p(int, int, int, int).
:- mode p(circ, circ, circ1, circ2) is det.

:- implementation.
p(X, X, Y, Y).
