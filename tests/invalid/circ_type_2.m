%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Abother test for circular equivalence types.
% This test is similar to circ_type_1.m but tests some more
% complicated cases involving parametric types.
%

:- module circ_type_2.

:- interface.
:- import_module list.
:- import_module set.

:- type circ1(T) == circ1(T).
:- type circ2(T) == circ2(circ2(T)).
:- type circ3(T) == list(circ3(T)).
:- type circ4(T) == circ4(set(T)).

:- type left(T) == right(T).
:- type right(T) == left(T).

:- type circ_left(T) == circ_right(list(T)).
:- type circ_right(T) == set(circ_left(T)).
