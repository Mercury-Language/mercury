%-----------------------------------------------------------------------------%
%
% The function identity creates an internal alias between the 2 fields of pp,
% thus we cannot reuse the individual fields of pp when calling scale as
% both fields point to the same memory cell.
%
%-----------------------------------------------------------------------------%
:- module internal_alias.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module float.

:- type point
	--->	p(float, float).

:- type point_pair
	--->	pp(point, point).

main -->
	io__write(scale(2.0, identity)),
	io__nl.

:- func identity = point_pair.

identity = pp(P, P) :- P = p(1.0, 1.0).

:- func scale(float, point_pair) = point_pair.

scale(Factor, pp(A0, B0)) = pp(A, B) :-
	A0 = p(X, Y),
	A =  p(Factor * X, Factor * Y),

	B0 = p(X0, Y0),
	B =  p((1.0/Factor) * X0, (1.0/Factor) * Y0).
