:- module fundeps_poly_instance.
:- interface.

:- typeclass foo(A, B) <= (A -> B) where [].

:- typeclass bar(A, B, C) <= foo(A, B) where [].

:- typeclass baz(A, B, C) <= ((A -> B, C), bar(A, B, C)) where [].

:- implementation.

:- type t(T) ---> t(T).

	% B is in the range of the functional dependency we must satisfy,
	% but it is determined from A by the functional dependencies on the
	% constraint.
	%
:- instance foo(t(A), t(B)) <= foo(A, B) where [].

:- instance bar(t(A), t(B), t(C)) <= foo(A, B) where [].

	% B and C are in the range of the functional dependency we must
	% satisfy.  B is determined from the foo/2 constraint, and C is
	% determined by an ancestor of the bar/3 constraint.
:- instance baz(t(A), t(B), t(C)) <= (foo(A, B), bar(B, C, A)) where [].

