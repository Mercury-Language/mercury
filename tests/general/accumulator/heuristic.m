	%
	% Tests that even though it is possible to introduce an
	% accumulator for p it would be counter productive because it
	% makes the algorithm O(N^2).
	%
:- module heuristic.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list, int.

main -->
	io__write_string("p: "),
	{ p([[1,10,100],[],[1,2,3],[6,5,4]], Length) },
	io__write(Length),
	io__nl.

:- pred p(list(list(T))::in, list(T)::out) is det.

p([], []).
p([X|Xs], L) :-
	p(Xs, L0),
	append(X, L0, L).
