% Abother test for circular equivalence insts.
% This test is similar to circ_inst.m but tests some more
% complicated cases involving parametric insts.

:- module circ_inst2.

:- interface.
:- import_module list.

:- inst circ1(I) == circ1(I).			% error -- cyclic
:- inst circ2(I) == circ2(circ2(I)).		% error -- cyclic
:- inst circ3(I) == list_skel(circ3(I)).	% OK
:- inst circ4(I) == circ4(list_skel(I)).	% error -- cyclic

:- inst left(I) == right(I).			% error -- cyclic
:- inst right(I) == left(I).			% error -- cyclic

:- inst circ_left(I) == circ_right(list_skel(I)).	% OK
:- inst circ_right(I) == list_skel(circ_left(I)).	% OK

