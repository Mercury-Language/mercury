:- module binary_search.

:- interface.

:- import_module io, bool, int.

:- pred main(io::di, io::uo) is det.

:- pred silly_even(int::in, bool::out) is det.

:- implementation.

:- import_module binary_search_1, list.

main(!IO) :-
	a(A),
	b(B),
	add1s([1,2,3,4,5,6,7,8,9,10], L),
	write(A, !IO),
	write(B, !IO),
	write(L, !IO),
	nl(!IO).

:- pred a(bool::out) is det.

:- pred b(bool::out) is det.

a(X) :- binary_search.silly_even(1001, X).

b(X) :- binary_search.silly_even(1003, X).

silly_even(N, R) :- 
	(
		N > 0
	->
		(
			(N =< 600 , N >= 405 ; N mod 3 = 0)
		->
			binary_search_1.sillier_even(N, R)
		;
			(
				N = 619
			->
				binary_search.silly_even(N-1, R)
			;
				binary_search.silly_even(N-2, R)
			)
		)
	;
		(
			N = 0
		->
			R = yes
		;
			R = no
		)
	).

:- pred add1s(list(int)::in, list(int)::out) is det.

add1s([], []).
add1s([H0 | T0], [H0+1 | T]) :- add1s(T0, T).
