% A test case to exercise the code for expanding hash tables,
% and for tabling typeinfos and tuples. We test the tabling of
% typeinfos for types of arity zero, one and two, and depths
% zero, one and two.
% The test is the same as expand_poly -- it needs to
% be done separately because if too many tests are run
% at once the program runs out of memory.
:- module expand_tuple.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module bool, int, list, std_util, random, require.

:- type record(T1, T2)	--->	record(T1, T1, T2).

main -->
	{ random__init(0, RS0) },
	{ random__permutation(range(0, 1023), Perm, RS0, RS1) },
	{ choose_signs_and_enter(Perm, {}, Solns1, RS1, RS2) },
	( { test_tables(Solns1, yes) } ->
		io__write_string("First test successful.\n")
	;
		io__write_string("First test unsuccessful.\n")
	),
	{ choose_signs_and_enter(Perm, {'a', [1, 2]}, Solns2, RS2, RS3) },
	( { test_tables(Solns2, yes) } ->
		io__write_string("Second test successful.\n")
	;
		io__write_string("Second test unsuccessful.\n")
	),
	{ choose_signs_and_enter(Perm, {{'a', 'b'}, 3, 4}, Solns3, RS3, _) },
	( { test_tables(Solns3, yes) } ->
		io__write_string("Third test successful.\n")
	;
		io__write_string("Third test unsuccessful.\n")
	).
	% io__report_tabling_stats.

:- func range(int, int) = list(int).
range(Min, Max) =
	(if Min > Max then
		[]
	else
		[Min | range(Min + 1, Max)]
	).

:- pred choose_signs_and_enter(list(int)::in, T::in, list(record(int, T))::out,
	random__supply::mdi, random__supply::muo) is det.

choose_signs_and_enter([], _, [], RS, RS).
choose_signs_and_enter([N | Ns], A, [record(F, S, A) | ISs], RS0, RS) :-
	random__random(Random, RS0, RS1),
	( Random mod 2 = 0 ->
		F = N
	;
		F = 0 - N
	),
	sum(F, A, S),
	choose_signs_and_enter(Ns, A, ISs, RS1, RS).

:- pred test_tables(list(record(int, T))::in, bool::out) is det.

test_tables([], yes).
test_tables([record(I, S0, A) | Is], Correct) :-
	sum(I, A, S1),
	( S0 = S1 ->
		test_tables(Is, Correct)
	;
		Correct = no
	).

:- pred sum(int::in, T::in, int::out) is det.
:- pragma memo(sum/3).

sum(N, A, F) :-
	( N < 0 ->
		sum(0 - N, A, NF),
		F = 0 - NF
	; N = 1 ->
		F = 1
	;
		sum(N - 1, A, F1),
		F = N + F1
	).
