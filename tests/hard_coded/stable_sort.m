:- module stable_sort.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module list, random, int, bool.

main -->
	{ generate_random_list(42, List) },
	% io__write(List),
	% io__nl,
	{ sort_is_stable(List, Stable) },
	(
		{ Stable = yes },
		io__write_string("list__sort/3 appears stable\n")
	;
		{ Stable = no },
		io__write_string("list__sort/3 is not stable\n")
	).

:- pred generate_random_list(int, list({int, int})).
:- mode generate_random_list(in, out) is det.

generate_random_list(Seed, List) :-
	random__init(Seed, RS),
	%
	% We generate random integers from 0 to 9.  The list length
	% must be large enough to ensure that there are plenty of
	% duplications, otherwise the test is trivial.
	%
	Count = 100,
	generate_random_list_2(Count, [], List, RS, _).

:- pred generate_random_list_2(int, list({int, int}), list({int, int}),
	random__supply, random__supply).
:- mode generate_random_list_2(in, in, out, mdi, muo) is det.

generate_random_list_2(Count, List0, List) -->
	(
		{ Count > 0 }
	->
		rnd_mod_10(R1),
		rnd_mod_10(R2),
		generate_random_list_2(Count - 1, [{R1, R2} | List0], List)
	;
		{ List = List0 }
	).

:- pred rnd_mod_10(int, random__supply, random__supply).
:- mode rnd_mod_10(out, mdi, muo) is det.

rnd_mod_10(N) -->
	random__random(R),
	{ N = R mod 10 }.

:- pred sort_is_stable(list({int, int}), bool).
:- mode sort_is_stable(in, out) is det.

sort_is_stable(Unsorted, Stable) :-
	%
	% If the sort is stable then sorting on the second component
	% followed by sorting on the first component should give a
	% fully sorted result.
	%
	list__sort(compare_second, Unsorted, SortedOnSecond),
	list__sort(compare_first, SortedOnSecond, SortedOnSecondThenFirst),

	%
	% Check the result is fully sorted.
	%
	list__sort(Unsorted, Sorted),
	(
		SortedOnSecondThenFirst = Sorted
	->
		Stable = yes
	;
		Stable = no
	).

:- pred compare_first({int, int}, {int, int}, comparison_result).
:- mode compare_first(in, in, out) is det.

compare_first({A, _}, {B, _}, Res) :-
	compare(Res, A, B).

:- pred compare_second({int, int}, {int, int}, comparison_result).
:- mode compare_second(in, in, out) is det.

compare_second({_, A}, {_, B}, Res) :-
	compare(Res, A, B).

