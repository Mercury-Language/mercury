:- module app.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module list.

:- type foo(T) ---> append(T, T, T).

main -->
	{ A = [1, 2, 3, 4, 5] },
	{ B = [6, 7, 8] },
	{ app(A, B, C) },
	io__write(app__append(A, B, C)),
	io__write_string(".\n"),
	{ D = [1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5] },
	{ E = [6, 7, 8] },
	{ app(D, E, F) },
	io__write(app__append(D, E, F)),
	io__write_string(".\n").


:- pred app(list(T), list(T), list(T)).
:- mode app(in, in, out) is det.

app([], Bs, Bs).
app([A|As], Bs, [A|Cs]) :-
	app(As, Bs, Cs).
