% Regression test.
%
% Name: agc_unbound_typevars2.m
%
% Description of bug:
% 	This module uses code that contains unbound type variables. The
% 	compiler was not correctly handling the unbound type variables
% 	when doing inlining. All type variables need to be mapped to a
% 	corresponding type_info variable for accurate GC compilation.
%
% Symptom(s) of bug:
% 	Map lookups fail when looking up unbound type variables.
%
% Date bug existed: 29-May-1997
%
% Author: trd

:- module agc_unbound_typevars2.
:- interface.
:- import_module io.

:- pred write_it(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module construct.
:- import_module list.
:- import_module type_desc.

:- pred test_all(T::in, io__state::di, io__state::uo) is det.

:- type poly(A, B)
	--->	poly_one(A)
	;	poly_two(B)
	;	poly_three(B, A, poly(B, A))
	;	poly_four(A, B).

%----------------------------------------------------------------------------%

write_it -->
	test_all(3).

%----------------------------------------------------------------------------%

test_all(_T) -->
	{ TypeInfo = type_of(poly_one([2399.3])) },
	{ N = num_functors(TypeInfo) },
	io__write_int(N).
