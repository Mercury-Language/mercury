:- module catch_retry.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is cc_multi.
:- implementation.
:- import_module exception, int.

main -->
	{ p(1, R1) },
	io__write(R1),
	io__nl,
	{ p(2, R2) },
	io__write(R2),
	io__nl.

:- pred p(int::in, exception_result(int)::out) is cc_multi.

p(N, R) :-
	try(q(N), R).

:- pred q(int::in, int::out) is det.

q(N, M) :-
	(
		N > 1
	->
		M = N
	;
		throw("q: bad input")
	).

