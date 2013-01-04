
:- module qs_always_test_allqueues_4.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module benchmarking.
:- import_module list.
:- import_module int.
:- import_module string.

:- import_module qs_utils.

:- import_module par_builtin. 

main(!IO) :-
    read_input(Input, !IO),
    quicksort(Input, Sorted),
    %par_cond_close_stats_file(!IO),
    sink_output(Sorted, !IO).

:- pred quicksort(list(string)::in, list(string)::out) is det.

quicksort([], []).
quicksort([P | Xs], Sorted) :-
    partition(P, Xs, Bigs, Littles),
    promise_pure
    (
        impure par_cond_contexts_and_all_sparks_vs_num_cpus(4)
    ->
        (
            quicksort(Bigs, SortedBigs)
        &
            quicksort(Littles, SortedLittles)
        )
    ;
        quicksort(Bigs, SortedBigs),
        quicksort(Littles, SortedLittles)
    ),
    my_append(SortedLittles, [P | SortedBigs], Sorted).

