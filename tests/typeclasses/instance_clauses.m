:- module instance_clauses.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module solutions.

:- typeclass thisclass(T) where [
	pred c(T, T),
	mode c(in, in) is semidet,
	mode c(out, in) is semidet,
	mode c(in, out) is nondet,

	pred d(T, T),
	mode d(in, in) is semidet,
	mode d(out, in) is semidet,
	mode d(in, out) is nondet
].

:- instance thisclass(int) where [
	(c(X, Y) :-
		( X = 1, Y = 1
		; X = 1, Y = 2
		; X = 1, Y = 3
		; X = 2, Y = 4
		)),
	d(42, 101),
	d(42, 102),
	d(43, 103)
].

:- pred mypred(T, T) <= thisclass(T).
:- mode mypred(in, out) is nondet.
mypred(A, B) :- c(A, B).

:- pred mypred2(T, T) <= thisclass(T).
:- mode mypred2(in, out) is nondet.
mypred2(A, B) :- d(A, B).

main -->
	{ solutions(mypred(1), X) },
	io__write(X), nl,
	{ solutions(mypred2(42), Y) },
	io__write(Y), nl.
