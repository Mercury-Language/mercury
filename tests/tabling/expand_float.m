% A test case to exercise the code for expanding hash tables.

:- module expand_float.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool, int, float, list, assoc_list, std_util, random, require.

main(!IO) :-
	random__init(0, RS0),
	random__permutation(range(0, 1023), Perm, RS0, RS1),
	choose_signs_and_enter(Perm, Solns, RS1, _RS),
	( test_tables(Solns, yes) ->
		io__write_string("Test successful.\n", !IO)
	;
		io__write_string("Test unsuccessful.\n", !IO)
	).
	% io__report_tabling_stats(!IO).

:- func range(int, int) = list(int).

range(Min, Max) =
	(if Min > Max then
		[]
	else
		[Min | range(Min + 1, Max)]
	).

:- pred choose_signs_and_enter(list(int)::in, assoc_list(float)::out,
	random__supply::mdi, random__supply::muo) is det.

choose_signs_and_enter([], [], RS, RS).
choose_signs_and_enter([N | Ns], [F - S | ISs], RS0, RS) :-
	random__random(Random, RS0, RS1),
	( Random mod 2 = 0 ->
		F = float(N)
	;
		F = float(0 - N)
	),
	sum(F, S),
	choose_signs_and_enter(Ns, ISs, RS1, RS).

:- pred test_tables(assoc_list(float)::in, bool::out) is det.

test_tables([], yes).
test_tables([I - S0 | Is], Correct) :-
	sum(I, S1),
	( S0 = S1 ->
		test_tables(Is, Correct)
	;
		Correct = no
	).

:- pred sum(float::in, float::out) is det.
:- pragma memo(sum/2).

sum(N, F) :-
	( N < 0.0 ->
		sum(0.0 - N, NF),
		F = 0.0 - NF
	; N = 1.0 ->
		F = 1.0
	;
		sum(N - 1.0, F1),
		F = N + F1
	).
