:- module quant_constraint_2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- typeclass c1(T1) where [].

	% Error: T is existentially quantified, but appears in a universal
	% constraint.
	%
:- some [T] (func q = T <= c1(T)).

:- implementation.

:- instance c1(int) where [].

q = 1.

main(!IO) :-
	X = q,
	write(X, !IO),
	nl(!IO).

