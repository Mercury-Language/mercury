% Abother test for circular equivalence insts.
% This test is the same as circ_inst2.m except that we
% also have a predicate which makes use of the insts.

:- module circ_mode4.

:- interface.
:- import_module list.

:- mode circ1(I) == circ1(I).			% error -- cyclic
:- mode circ2(I) == circ2(list_skel(I)).	% error -- cyclic

:- mode left(I) == right(I).			% error -- cyclic
:- mode right(I) == left(I).			% error -- cyclic

:- pred p(int, int, int, int).
:- mode p(circ1(ground), circ2(ground),
	left(ground), right(ground))
	is det.

:- implementation.
p(A, A, B, B).
