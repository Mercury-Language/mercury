:- module cut_test.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int.

main -->
	( { best(100) } ->
		io__write_string("best case nondet test 1 succeeded:   BUG.\n")
	;
		io__write_string("best case nondet test 1 failed:      OK.\n")
	),
	( { best(300) } ->
		io__write_string("best case nondet test 2 succeeded:   OK.\n")
	;
		io__write_string("best case nondet test 2 failed:      BUG.\n")
	),
	( { middle(100) } ->
		io__write_string("middle case nondet test 1 succeeded: BUG.\n")
	;
		io__write_string("middle case nondet test 1 failed:    OK.\n")
	),
	( { middle(180) } ->
		io__write_string("middle case nondet test 2 succeeded: OK.\n")
	;
		io__write_string("middle case nondet test 2 failed:    BUG.\n")
	),
	( { middle(190) } ->
		io__write_string("middle case nondet test 3 succeeded: OK.\n")
	;
		io__write_string("middle case nondet test 3 failed:    BUG.\n")
	),
	( { middle(200) } ->
		io__write_string("middle case nondet test 4 succeeded: OK.\n")
	;
		io__write_string("middle case nondet test 4 failed:    BUG.\n")
	),
	( { worst(100) } ->
		io__write_string("worst case nondet test 1 succeeded:  BUG.\n")
	;
		io__write_string("worst case nondet test 1 failed:     OK.\n")
	),
	( { worst(180) } ->
		io__write_string("worst case nondet test 2 succeeded:  OK.\n")
	;
		io__write_string("worst case nondet test 2 failed:     BUG.\n")
	),
	( { worst(190) } ->
		io__write_string("worst case nondet test 3 succeeded:  OK.\n")
	;
		io__write_string("worst case nondet test 3 failed:     BUG.\n")
	),
	( { worst(200) } ->
		io__write_string("worst case nondet test 4 succeeded:  OK.\n")
	;
		io__write_string("worst case nondet test 4 failed:     BUG.\n")
	).

:- pred best(int::in) is semidet.

best(A) :-
	test(A, _).

:- pred middle(int::in) is semidet.

middle(A0) :-
	(
		A1 = A0 + 10
	;
		A1 = A0 + 20
	;
		A1 = A0 + 30
	),
	test(A1, _).

:- pred worst(int::in) is semidet.

worst(A0) :-
	addsome(A0, A1),
	test(A1, _).

:- pred addsome(int::in, int::out) is multi.

addsome(A0, A1) :-
	(
		A1 = A0 + 10
	;
		A1 = A0 + 20
	;
		A1 = A0 + 30
	).

:- pred test(int::in, int::out) is nondet.

test(A, B) :-
	A > 200,
	(
		B = A
	;
		B = A * 2
	;
		B = A * 3
	).
