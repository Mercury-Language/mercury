% Some very simple tests of type_to_univ and univ_to_type.

:- module univ.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module std_util, list, bool.

main --> 
	( { test1 } ->
		io__write_string("test 1 ok\n")
	;
		io__write_string("test 1 failed\n")
	),
	( { test2 } ->
		io__write_string("test 2 failed\n")
	;
		io__write_string("test 2 ok\n")
	),
	( { test3 } ->
		io__write_string("test 3 failed\n")
	;
		io__write_string("test 3 ok\n")
	),
	( { test4(Compare) } ->
		( { Compare = no } ->
			io__write_string("test 4 ok\n")
		;
			io__write_string("test 4 comparison failed\n")
		)
	;
		io__write_string("test 4 type_to_univ failed\n")
	).

:- pred test1 is semidet.

test1 :-
	X = 1,
	type_to_univ(X, UnivX),
	Y = 1,
	type_to_univ(Y, UnivY),
	UnivX = UnivY.

:- pred test2 is semidet.

test2 :-
	X = 1,
	type_to_univ(X, UnivX),
	Y = 2,
	type_to_univ(Y, UnivY),
	UnivX = UnivY.

:- pred test3 is semidet.

test3 :-
	X = 1,
	type_to_univ(X, UnivX),
	type_to_univ(Y, UnivX),
	Y = "string".

:- pred test4(bool::out) is semidet.

test4(Compare) :-
	X = 1,
	type_to_univ(X, UnivX),
	type_to_univ(Y, UnivX),
	% The comparison should establish the type of Y as integer (same as X)
	( Y = 2 ->
		Compare = yes
	;
		Compare = no
	).
