:- module args.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is cc_multi.
:- implementation.
:- import_module library_forwarding.

main -->
	(
		{ p(1, X, 3, Y, 5) },
		{ semidet_fail }
	->
		io__write_int(X),
		io__nl,
		io__write_int(Y),
		io__nl
	;
		io__write_string("no.\n")
	).

:- pred p(int, int, int, int, int).
:- mode p(in, out, in, out, in) is nondet.

p(A, A + (B * C), B, (A + B) * C, C) :-
	semidet_succeed.
p(A, A - B, B, C - B, C) :-
	semidet_succeed.

