:- module browse_pretty.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module list.

:- type big
	--->	big(big, list(int), big)
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
				[1],
				small
			),
			[1, 2],
			small
		),
		[1, 2, 3],
		big(
			big(
				small,
				[1, 2, 3, 4],
				big(
					small,
					[1, 2, 3, 4, 5],
					small
				)
			),
			[1, 2, 3, 4, 5, 6],
			small
		)
	).

