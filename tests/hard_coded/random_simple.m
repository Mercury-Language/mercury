:- module random_simple.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module random, int.

main -->
	{ Seed = 3 },
	{ random__init(Seed, RS0) },
	test(1, 20, RS0, RS1),
	test(-1, 20, RS1, _).

:- pred test(int::in, int::in, random__supply::mdi, random__supply::muo,
	io__state::di, io__state::uo) is det.

test(Range, Count, RS0, RS) -->
	(
		{ Count > 0 }
	->
		{ random__random(0, Range, N, RS0, RS1) },
		(
			{ N = 0 }
		->
			test(Range, Count - 1, RS1, RS)
		;
			io__write_string("Test failed.\n"),
			{ RS = RS1 }
		)
	;
		io__write_string("Test passed.\n"),
		{ RS = RS0 }
	).

