:- module ite_2.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is cc_multi.
:- implementation.
:- import_module library_forwarding.

main -->
	{ ite(a, 1, M) },
	{ ite(b, 1, N) },
	io__write_string("ite(a, 1, "),
	io__write_int(M),
	io__write_string(").\nite(b, 1, "),
	io__write_int(N),
	io__write_string(").\n\n").

:- pred ite(pred(int, int), int, int).
:- mode ite(in(pred(in, out) is semidet), in, out) is det.
:- mode ite(in(pred(in, out) is nondet), in, out) is multi.

ite(P, X, Y) :-
	(
		P(X, Z),
		Z > 1
	->
		Y = Z
	;
		c(X, Y)
	).

:- pred a(int, int).
:- mode a(in, out) is semidet.

a(1, 1).

:- pred b(int, int).
:- mode b(in, out) is nondet.

b(1, 0).
b(1, 1).

:- pred c(int, int).
:- mode c(in, out) is det.

c(X, X).

