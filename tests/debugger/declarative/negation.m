:- module negation.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module library_forwarding.

main(S0, S) :-
	p(1, X),
	io__write_int(X, S0, S1),
	io__nl(S1, S).

:- pred p(int, int).
:- mode p(in, out) is det.

p(A, B) :-
	(
		not (
			r(A, C),
			not q(C)
		)
	->
		B = 42
	;
		B = A
	).

:- pred q(int).
:- mode q(in) is semidet.

q(N) :-
	N > 10.

:- pred r(int, int).
:- mode r(in, out) is multi.

r(K, K + 10).
r(K, K + 20).
