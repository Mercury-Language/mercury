:- module browser_test.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.

:- type big
	--->	big(big, int, big)
	;	small.

main -->
	{ big_data(Data) },
	io__print(Data),
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

