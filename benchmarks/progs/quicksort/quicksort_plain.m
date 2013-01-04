
:- module quicksort_plain.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module benchmarking.
:- import_module list.
:- import_module int.
:- import_module string.

:- import_module qs_utils.

main(!IO) :-
    read_input(Input, !IO),
    promise_equivalent_solutions [Time, !:IO] (
        benchmark_det(quicksort, Input, Sorted, 1, Time),
        sink_output(Sorted, !IO)
    ),
    format("Quicksort of size %d in %dmsec\n", [i(length(Input)), i(Time)], !IO).

:- pred quicksort(list(string)::in, list(string)::out) is det.

quicksort([], []).
quicksort([P | Xs], Sorted) :-
    partition(P, Xs, Bigs, Littles),
    quicksort(Bigs, SortedBigs),
    quicksort(Littles, SortedLittles),
    my_append(SortedLittles, [P | SortedBigs], Sorted).

