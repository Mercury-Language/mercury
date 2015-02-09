:- module multi_moded.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module solutions.

:- typeclass thisclass(T) where [
	pred c(T, T),
	mode c(in, in) is semidet,
	mode c(out, in) is semidet,
	mode c(in, out) is nondet
].

:- instance thisclass(int) where [
	pred(c/2) is p
].

:- pred p(int, int).
:- mode p(in, in) is semidet.
:- mode p(out, in) is semidet.
:- mode p(in, out) is nondet.
p(1, 1).
p(1, 2).
p(1, 3).
p(2, 4).

:- pred mypred(T, T) <= thisclass(T).
:- mode mypred(in, out) is nondet.
mypred(A, B) :- c(A, B).

main -->
	{ solutions(mypred(1), X) },
	io__write(X),
	nl.
