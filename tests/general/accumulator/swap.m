	%
	% Tests that the compiler recognises append is assocative if we
	% swap the order of the two input arguments.
	%
:- module swap.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list.

main -->
	io__write_string("rev: "),
	{ rev([5,6,7], ListA) },
	io__write(ListA),
	io__nl.

:- pred rev(list(T), list(T)).
:- mode rev(in, out) is det.

rev([], []).
rev([H|T], R) :-
	rev(T, R0),
	append(R0, [H], R).
