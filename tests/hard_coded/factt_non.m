:- module factt_non.
:- interface.

:- import_module io.
:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

:- import_module int, float, string.
:- import_module pair, maybe, list, bool, solutions.

:- pred example(int, float, string).
:- mode example(in, in, in) is semidet.
:- mode example(in, in, out) is nondet.
:- mode example(in, out, out) is nondet.
:- mode example(out, out, out) is multi.

:- pragma fact_table(example/3,"factt_non_examples").

main -->
	{ test_in_in_in(Result1) },
	{ test_in_in_out(Result2) },
	{ test_in_out_out(Result3) },
	{ test_out_out_out(Result4) },
	print(Result1), nl,
	print(Result2), nl,
	print(Result3), nl,
	print(Result4), nl.

:- pred test_in_in_in(pair(bool)::out) is det.

test_in_in_in(Res1 - Res2) :-
	Res1 = ( example(2, 2.0, "2.0") -> yes ; no ),
	Res2 = ( example(42, 2.0, "foobar") -> yes ; no ).

:- pred test_in_in_out(pair(maybe(string))::out) is cc_multi.

test_in_in_out(Res1 - Res2) :-
	Res1 = ( example(42, 3.0, S1) -> yes(S1) ; no ),
	Res2 = ( example(42, 2.0, S2) -> yes(S2) ; no ).

:- pred test_in_out_out(pair(maybe(string))::out) is cc_multi.

test_in_out_out(Res1 - Res2) :-
	(
		example(42, F1, S1),
		F1 > 10.0
	->
		Res1 = yes(S1)
	;
		Res1 = no
	),
	(
		example(2, F2, S2),
		F2 > 10.0
	->
		Res2 = yes(S2)
	;
		Res2 = no
	).

:- pred test_out_out_out(list(string)::out) is det.

test_out_out_out(Res) :-
	solutions((pred(S::out) is nondet :-
			example(N, F, S),
			( N > 10 ; F < 1.5 )
		), Res).
