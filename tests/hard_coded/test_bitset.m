:- module test_bitset.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module bool, enum, int, list, bitset_tester, random, require.

:- func list1 = list(int).

list1 = [29, 28, 31, 22, -15, 32, 19, 58, -59, 36, 7, 39, 42,
		-34, 25, 40, 59, 2, -19, 44, 47, 38].

:- func list2 = list(int).

list2 = [21, 52, 23, -18, -23, 56, 11, -46, 61, -4, 63, 54, 17, -64,
		-13, -38, 37, 4, 39, -2, 57, -56, -37, -30, -51, 12, -49,
		-58, -31, -48, -61, 42, 53, -44, 55, 14, 9, -40, 43, 50].

main -->
	% Run one lot of tests with known input lists,
	% to generate some visible output.
	{ Write = yes },
	run_test(Write, list1, list2),

	% Run some more tests with random input, checking
	% the output against that of set_ordlist.
	{ Iterations = 10 },
	{ random__init(1, Supply) },
	run_tests(Iterations, Supply).

:- pred run_tests(int::in, random__supply::mdi,
		io__state::di, io__state::uo) is det.

run_tests(Iterations, Supply0) -->
	( { Iterations = 0 } ->
		[]
	;
		{ Num1 = 20 },
		{ get_random_numbers(Num1, [], List1, Supply0, Supply1) },
		{ Num2 = 40 },
		{ get_random_numbers(Num2, [], List2, Supply1, Supply) },
		
		{ Write = no },
		run_test(Write, List1, List2),


		run_tests(Iterations - 1, Supply)
	).

:- pred get_random_numbers(int::in, list(int)::in, list(int)::out,
		random__supply::mdi, random__supply::muo) is det.

get_random_numbers(Num, List0, List, Supply0, Supply) :-
	( Num = 0 ->
		List = List0,
		Supply = Supply0
	;
		% Test negative as well as positive numbers.
		random__random(-64, 128, RN, Supply0, Supply1),
		get_random_numbers(Num - 1, [RN | List0], List,
			Supply1, Supply)
	).

:- pred run_test(bool::in, list(int)::in, list(int)::in,
		io__state::di, io__state::uo) is det.

run_test(Write, List1, List2) -->
	( { Write = yes } ->
		io__write_string("List1: "),
		io__write_list(list__sort(List1), ", ", io__write_int),
		io__nl, io__nl,
		io__write_string("List2: "),
		io__write_list(list__sort(List2), ", ", io__write_int),
		io__nl, io__nl
	;
		[]	
	),
	{ Set1 = bitset_tester__list_to_set(List1) },
	{ Set2 = bitset_tester__list_to_set(List2) },

	io__write_string("testing count\n"),
	{ Count1 = count(Set1) },
	{ Count2 = count(Set2) },
	( { Write = yes } ->
		io__write_string("count: "),
		io__write_int(Count1),
		io__write_string(" "),
		io__write_int(Count2),
		io__nl
	;
		[]
	),

	io__write_string("testing foldl\n"),
	{ Sum = (func(Elem, Acc) = Elem + Acc) },
	{ Result1 = foldl(Sum, Set1, 0) },
	{ Result2 = foldl(Sum, Set2, 0) },
	( { Write = yes } ->
		io__write_string("Sum of List1 = "),
		io__write_int(Result1),
		io__nl,
		io__write_string("Sum of List2 = "),
		io__write_int(Result2),
		io__nl
	;
		[]
	),

	io__write_string("testing union\n"),
	{ Union = union(Set1, Set2) },
	write_bitset_result(Write, Union),

	io__write_string("testing intersection\n"),
	{ Intersection = intersect(Set1, Set2) },
	write_bitset_result(Write, Intersection),

	io__write_string("testing difference\n"),
	{ Difference = difference(Set1, Set2) },
	write_bitset_result(Write, Difference),

	io__write_string("testing remove_least_element\n"),
	( { remove_least(Set1, Least, RemovedLeast) } ->
		( { Write = yes } ->
			io__write_int(Least),
			io__nl
		;
			[]
		),
		write_bitset_result(Write, RemovedLeast)
	;
		{ error("remove_least failed") }
	),

	io__write_string("testing delete_list\n"),
	{ Delete = delete_list(Set1, List2) },
	write_bitset_result(Write, Delete),

	{ require(unify(delete_list(Set1, List1),
			init `with_type` bitset_tester(int)),
		"delete_list_failed") }.

:- pred write_bitset_result(bool::in, bitset_tester(int)::in,
		io__state::di, io__state::uo) is det.
:- pragma no_inline(write_bitset_result/4).
	
write_bitset_result(Write, Set) -->
	( { Write = yes } ->
		{ List `with_type` list(int) = to_sorted_list(Set) },
		io__write(List),
		io__nl
	;
		[]
	).

