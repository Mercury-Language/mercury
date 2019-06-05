%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module map_select_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.

%---------------------------------------------------------------------------%

main(!IO) :-
    init_map(100, 1, [], Pairs),
    map.from_assoc_list(Pairs, FullMap),
    init_keys(100, 5, [], UnsortedKeys),
    list.sort(UnsortedKeys, SortedKeys),

    map.select_sorted_list(FullMap, SortedKeys, SelectedMapA),
    map.select_unselect_sorted_list(FullMap, SortedKeys,
        SelectedMapB, UnselectedMapB),
    map.to_sorted_assoc_list(SelectedMapA, SelectedAssocListA),
    map.to_sorted_assoc_list(SelectedMapB, SelectedAssocListB),
    map.to_sorted_assoc_list(UnselectedMapB, UnselectedAssocListB),
    expect(unify(SelectedAssocListA, SelectedAssocListB), $pred,
        "SelectedAssocListA != SelectedAssocListB"),
    io.write_line(SelectedAssocListB, !IO),
    io.write_line(UnselectedAssocListB, !IO).

:- pred init_map(int::in, int::in,
    assoc_list(int, int)::in, assoc_list(int, int)::out) is det.

init_map(Max, Cur, !AssocList) :-
    ( if Cur > Max then
        true
    else
        ( if Cur mod 2 = 0 then
            !:AssocList = [Cur - Cur * 10 | !.AssocList]
        else if Cur mod 3 = 0 then
            !:AssocList = [Cur - Cur * 100 | !.AssocList]
        else
            true
        ),
        init_map(Max, Cur + 1, !AssocList)
    ).

:- pred init_keys(int::in, int::in, list(int)::in, list(int)::out) is det.

init_keys(Max, Cur, !List) :-
    ( if Cur > Max then
        true
    else
        !:List = [Cur | !.List],
        init_keys(Max, Cur + 5, !List)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
