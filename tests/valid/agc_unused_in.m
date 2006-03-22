%
% Regression test.
%
% Name: agc_unused_in.m
%
% Description of bug:
% 	This module uses code that has a variable of type T that is
% 	unused, but has mode in (the argument of `type_of' has mode
% 	unused). This caused problems since the variable was considered 
% 	always live because there was no use of the variable that killed it. 
% 	In accurate gc grades, it caused the compiler to try to save the
% 	typeinfo associated with it (TypeInfo_for_T).
%
% Symptom(s) of bug:
% 	Map lookups fail when trying to save TypeInfo_for_T, or cannot
% 	find stackslots for the variable.
%
% Date bug existed: 11-May-1997
%
% Author: trd

:- module agc_unused_in.
:- interface.

:- pred test_1(T::in, string::out) is det.

%----------------------------------------------------------------------------%
:- implementation.

:- import_module type_desc.

test_1(T, N) :-
	Info = type_of(T),
	N = type_name(Info).

