% regression test: versions 0.7.3 and earlier failed this test.

:- module minint_bug.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, list, string.

main -->
	{ string__int_to_string(-2147483648, S) }, print(S), nl,
	print(-2147483648), nl,
	format("%d\n", [i(-2147483648)]).
/*
We could test the following too, but the correct output is machine-dependent,
which makes it difficult to validate the results of the test.
	{ int__min_int(MinInt) },
	{ string__int_to_string(MinInt, S2) }, print(S2), nl,
	print(MinInt), nl,
	format("%d\n", [i(MinInt)]), nl.
*/
