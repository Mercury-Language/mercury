% Test for circular equivalence types.
% This is the same as circ_type.m, except that we also have 
% a predicate that uses the types.
:- module circ_type3.

:- interface.

:- type circ == circ.

:- type circ1 == circ2.

:- type circ2 == circ1.

:- pred p(circ, circ1, circ2).
:- mode p(in, in, out) is det.

:- implementation.
p(_, X, X).
