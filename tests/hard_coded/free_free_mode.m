:- module free_free_mode.
:- interface.
:- import_module io.

:- inst f == free.

:- pred foo(int::(f>>f)) is det.
:- pred bar(int::(f>>f)) is det.

:- pred main(io__state::di, io__state::uo) is det.


:- implementation.

foo(_).

bar(X) :- foo(X).

main -->
	{ bar(X) },
	{ X = 42 },
	{ bar(X) },
	io__write_int(X),
	io__write_string("\n").
