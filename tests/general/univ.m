:- module univ.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module std_util, list.

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

