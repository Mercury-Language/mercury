% Abother test for circular equivalence insts.
% This test is the same as circ_inst2.m except that we
% also have a predicate which makes use of the insts.

:- module circ_inst4.

:- interface.
:- import_module list.

:- inst circ1(I) == circ1(I).			% error -- cyclic
:- inst circ2(I) == circ2(circ2(I)).		% error -- cyclic
:- inst circ3(I) == list_skel(circ3(I)).	% OK
:- inst circ4(I) == circ4(list_skel(I)).	% OK

:- inst left(I) == right(I).			% error -- cyclic
:- inst right(I) == left(I).			% error -- cyclic

:- inst circ_left(I) == circ_right(list_skel(I)).	% OK
:- inst circ_right(I) == list_skel(circ_left(I)).	% OK

:- pred p(int, int, int, int, int, int, int, int).
:- mode p(in(circ1(ground)), out(circ2(ground)),
	in(circ3(ground)), out(circ4(ground)),
	in(left(ground)), out(right(ground)),
	in(circ_left(ground)), out(circ_right(ground)))
	is det.

:- implementation.
p(A, A, B, B, C, C, D, D).
