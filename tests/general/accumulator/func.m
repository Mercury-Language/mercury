	% Test that accumulators are introduced into functions.
:- module (func).

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list, int.

main -->
	io__write_string("sumlist: "),
	{ Sum = sumlist([5,6,7]) },
	io__write(Sum),
	io__nl.

:- func sumlist(list(int)) = int.

sumlist([]) = 0.
sumlist([H|T]) = H + sumlist(T).
