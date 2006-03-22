% 
% Regression test.
%
% Name: agc_unbound_typevars.m
%
% Description of bug:
% 	This module uses code that contains unbound type variables. The
% 	compiler was not recording what variables the type variables
% 	were mapped to in this case.
%
% Symptom(s) of bug:
% 	Map lookups fail when looking up unbound type variables.
%
% Date bug existed: 11-May-1997
%
% Author: trd

:- module agc_unbound_typevars.
:- interface.

:- pred foo(int::out) is det.

:- implementation.

:- import_module construct.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module type_desc.

foo(X) :-
	TypeInfo = type_of([]), 
	map__init(Map),
	TypeInfo2 = type_of(Map), 
	N = num_functors(TypeInfo),
	M = num_functors(TypeInfo2),
	X = N + M.

