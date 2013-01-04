
:- module qs_top_par_THRESHOLD.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module benchmarking.
:- import_module int.
:- import_module list.
:- import_module string.

:- import_module qs_utils.

main(!IO) :-
    read_input(Input, !IO),
    quicksort_top(0, Input, Sorted),
    sink_output(Sorted, !IO).

:- pred quicksort_top(int::in, list(string)::in, list(string)::out) is det.

quicksort_top(_, [], []).
quicksort_top(N0, [P | Xs], Sorted) :-
    ( N0 > THRESHOLD ->
        quicksort([P | Xs], Sorted)
    ;
        partition(P, Xs, Bigs, Littles),
        N = N0 + 1,
        (
            quicksort_top(N, Bigs, SortedBigs)
        &
            quicksort_top(N, Littles, SortedLittles)
        ),
        my_append(SortedLittles, [P | SortedBigs], Sorted)
    ).

:- pred quicksort(list(string)::in, list(string)::out) is det.

quicksort([], []).
quicksort([P | Xs], Sorted) :-
    partition(P, Xs, Bigs, Littles),
    quicksort(Bigs, SortedBigs),
    quicksort(Littles, SortedLittles),
    my_append(SortedLittles, [P | SortedBigs], Sorted).

