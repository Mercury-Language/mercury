%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test whether list.sort is stable.
%
%---------------------------------------------------------------------------%

:- module stable_sort.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module random.
:- import_module random.sfc16.

main(!IO) :-
    generate_random_list(List),
    % io.write(List),
    % io.nl,
    % io.write(sort(List) `with_type` list({int, int})),
    % io.nl,
    is_sort_stable(List, Stable),
    (
        Stable = yes,
        io.write_string("list.sort/3 appears stable\n", !IO)
    ;
        Stable = no,
        io.write_string("list.sort/3 is not stable\n", !IO)
    ).

:- pred generate_random_list(list({int, int})::out) is det.

generate_random_list(List) :-
    RandomState0 = random.sfc16.init,
    % We generate random integers from 0 to 9. The list length must be
    % large enough to ensure that there are plenty of duplications,
    % otherwise the test is trivial.
    Count = 100,
    generate_random_list_2(Count, [], List, RandomState0, _).

:- pred generate_random_list_2(int::in,
    list({int, int})::in, list({int, int})::out,
    RandomState::in, RandomState::out) is det <= random(RandomState).

generate_random_list_2(Count, List0, List, !RandomState) :-
    ( if Count > 0 then
        uniform_int_in_range(0, 9, R1, !RandomState),
        uniform_int_in_range(0, 9, R2, !RandomState),
        generate_random_list_2(Count - 1, [{R1, R2} | List0], List,
            !RandomState)
    else
        List = List0
    ).

:- pred is_sort_stable(list({int, int})::in, bool::out) is det.

is_sort_stable(Unsorted, Stable) :-
    % If the sort is stable, then sorting on the second component
    % followed by sorting on the first component *should* give a
    % fully sorted result.
    list.sort(compare_second, Unsorted, SortedOnSecond),
    list.sort(compare_first, SortedOnSecond, SortedOnSecondThenFirst),

    % Check whether the result is fully sorted.
    list.sort(Unsorted, Sorted),
    ( if SortedOnSecondThenFirst = Sorted then
        Stable = yes
    else
        Stable = no
    ).

:- pred compare_first({int, int}::in, {int, int}::in, comparison_result::out)
    is det.

compare_first({A, _}, {B, _}, Res) :-
    compare(Res, A, B).

:- pred compare_second({int, int}::in, {int, int}::in, comparison_result::out)
    is det.

compare_second({_, A}, {_, B}, Res) :-
    compare(Res, A, B).
