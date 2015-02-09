:- module multi_parameter_bug.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, char.

main -->
	p(42, 'a').

:- pred p(T1, T2, io__state, io__state) <= c2(T1, T2).
:- mode p(in, in, di, uo) is det.
p(X, _) -->
	{ m1(X, Y) },
	io__write_int(Y),
	io__nl.

:- typeclass c1(T) where [
	pred m1(T, int),
	mode m1(in, out) is det
].

:- typeclass c2(T1, T2) <= c1(T1) where [
].

:- instance c1(int) where [
	m1(X,X)
].

:- instance c2(int, char) where [
].
