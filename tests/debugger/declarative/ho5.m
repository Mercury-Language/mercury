:- module ho5.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is cc_multi.
:- implementation.
:- import_module exception.

main -->
	{ try(p(1), R1) },
	io__write(R1),
	io__nl,
	{ try(p(2), R2) },
	io__write(R2),
	io__nl.

:- pred p(int::in, int::out) is det.

p --> q, r.

:- pred q(int::in, int::out) is det.

q(_, 0).

:- pred r(int::in, int::out) is det.

r(M, N) :-
	(
		M = 0
	->
		throw(zero)
	;
		N = 0
	).

:- func zero(int) = string.

zero(_) = "zero".

