
:- module qs_always_par.

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
    quicksort(Input, Sorted),
    sink_output(Sorted, !IO).

:- pred quicksort(list(string), list(string)) is det.
:- mode quicksort(in, out) is det.
:- mode quicksort(list_skel_di, list_skel_uo) is det.

quicksort([], []).
quicksort([P | Xs], Sorted) :-
    partition(P, Xs, Bigs, Littles),
    (
        quicksort(Bigs, SortedBigs)
    &
        quicksort(Littles, SortedLittles)
    ),
    my_append(SortedLittles, [P | SortedBigs], Sorted).

