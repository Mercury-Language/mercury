:- module binary_search_1.

:- interface.

:- import_module bool, int.

:- pred sillier_even(int::in, bool::out) is det.

:- implementation.

:- import_module binary_search.

sillier_even(N, R) :- 
	(
		not (N =< 600, N >= 405 ; N mod 3 = 0)
	->
		binary_search.silly_even(N, R)
	;
		binary_search_1.sillier_even(N-2, R)
	).
