%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Abother test for circular equivalence modes.
% This test is similar to circ_mode_1.m but tests some more
% complicated cases involving parametric modes.
%

:- module circ_mode_2.

:- interface.
:- import_module list.

:- mode circ1(I) == circ1(I).               % error -- cyclic
:- mode circ2(I) == circ2(list_skel(I)).    % error -- cyclic

:- mode left(I) == right(I).                % error -- cyclic
:- mode right(I) == left(I).                % error -- cyclic
