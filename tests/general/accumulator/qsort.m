	%
	% Make sure that this doesn't get recognised as an
	% opportunity to introduce accumulator recursion,
	% because qsort is already tail recursive.
	%
:- module qsort.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list, int.

main -->
	{ qsort([1,6,0,8,7,4], [], S) },
	io__write_string("qsort: "),
	io__write(S),
	io__nl.

:- pred qsort(list(T), list(T), list(T)).
:- mode qsort(in, in, out) is det.

qsort([], R, R).
qsort([X|L], R0, R) :-
        partition(L, X, L1, L2),
        qsort(L2, R0, R1),
        qsort(L1, [X|R1], R).

:- pred partition(list(T), T, list(T), list(T)).
:- mode partition(in, in, out, out) is det.

partition([], _, [], []).
partition([Head|Tail], Partition, Low, High) :-
        ( compare(<, Head, Partition) ->
                partition(Tail, Partition, Low1, High),
                Low = [Head|Low1]
        ;
                partition(Tail, Partition, Low, High1),
		High = [Head|High1]
	).
