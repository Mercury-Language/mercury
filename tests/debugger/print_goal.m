:- module print_goal.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module int.

:- type big
	--->	big(big, int, big)
	;	small.

main -->
	{ big_data(Data) },
	io__print(Data),
	io__write_string(".\n"),
	print_goal(100, 101, _, Y, 102, Z),
	io__print(Y),
	io__write_string(".\n"),
	io__print(Z),
	io__write_string(".\n").

:- pred big_data(big::out) is det.

big_data(Data) :-
	Data = big(
		big(
			big(
				small,
				1,
				small
			),
			2,
			small
		),
		3,
		big(
			big(
				small,
				4,
				big(
					small,
					5,
					small
				)
			),
			6,
			small
		)
	).

:- pred print_goal(int::in, int::in, int::out, int::out, int::in, int::out,
		io__state::di, io__state::uo) is det.

print_goal(W, X, X + 1, X + 2, Y, Y + 1) --> [].

