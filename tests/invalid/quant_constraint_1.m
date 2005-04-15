:- module quant_constraint_1.
:- interface.

:- typeclass c1(T1, T2) where [].
:- typeclass c2(T1) where [].
:- typeclass c3(T1) where [].

	% Two errors:
	% 	T2 is in universal constraint c2, but is existentially
	% 	quantified.
	%
	% 	T1 is in existential constraint c1, but is universally
	% 	quantified.
	%
:- all [T1] some [T2] ((pred p(T1, T2) => c1(T1, T2)) <= (c2(T2), c3(T1))).
:- mode p(in, out) is det.

:- implementation.

p(A, A).

