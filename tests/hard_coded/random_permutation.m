:- module random_permutation.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module int, list, random.

main -->
	{ List = gen_sorted_list(1, 100) },
	{ random__init(1, RS) },
	do_tests(List, 10, RS).

:- pred do_tests(list(int), int, random__supply, io__state, io__state).
:- mode do_tests(in, in, mdi, di, uo) is det.

do_tests(List, Count, RS0) -->
	(
		{ Count > 1 }
	->
		{ random__permutation(List, Perm, RS0, RS1) },
		{ list__sort_and_remove_dups(Perm, SortedList) },
		(
			{ SortedList = List }
		->
			io__write_string("Test passed.\n")
		;
			io__write_string("Test failed!\n")
		),
		do_tests(List, Count - 1, RS1)
	;
		[]
	).

:- func gen_sorted_list(int, int) = list(int).

gen_sorted_list(M, N)
	= ( M > N ->
		[]
	;
		[M | gen_sorted_list(M + 1, N)]
	).

