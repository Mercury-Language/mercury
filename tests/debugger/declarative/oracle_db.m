:- module oracle_db.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.

main -->
	(
		{ a(99, 99, 99) }
	->
		io__write_string("yes.\n")
	;
		io__write_string("no.\n")
	).

:- pred a(int, int, int).
:- mode a(in, in, in) is semidet.

a(X, Y, Z) :-
	b(X),
	b(Y),
	b(Z).

:- pred b(int).
:- mode b(in) is semidet.

b(99).

