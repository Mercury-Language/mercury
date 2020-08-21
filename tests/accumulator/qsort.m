%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Make sure that this doesn't get recognised as an opportunity to introduce
% accumulator recursion, because qsort is already tail recursive.
%

:- module qsort.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module int.

main(!IO) :-
    qsort([1, 6, 0, 8, 7, 4], [], S),
    io.write_string("qsort: ", !IO),
    io.write_line(S, !IO).

:- pred qsort(list(T)::in, list(T)::in, list(T)::out) is det.

qsort([], R, R).
qsort([X | L], R0, R) :-
    partition(L, X, L1, L2),
    qsort(L2, R0, R1),
    qsort(L1, [X | R1], R).

:- pred partition(list(T)::in, T::in, list(T)::out, list(T)::out) is det.

partition([], _, [], []).
partition([Head | Tail], Partition, Low, High) :-
    ( if compare(<, Head, Partition) then
        partition(Tail, Partition, Low1, High),
        Low = [Head | Low1]
    else
        partition(Tail, Partition, Low, High1),
        High = [Head | High1]
    ).
