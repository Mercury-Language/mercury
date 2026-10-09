%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module map_ops.

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
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    AL1 = ["0zero" - 0, "1one" - 1, "2two" - 2, "3three" - 3,
        "4four" - 4, "5five" - 5, "6six" - 6, "7seven" - 7],
    AL2 = ["0zero" - 0, "1oneX" - 1, "2two" - 2, "3threeX" - 3,
        "4four" - 4, "5five" - 50, "6six" - 60, "7seven" - 7],
    AL3 = ["0zero" - 0, "1oneX" - 100, "2two" - 200, "3threeX" - 300,
        "4fourX" - 400, "5five" - 500, "6six" - 600, "7seven" - 7],
    AL4 = ["0zero" - 0, "1oneX" - 100, "2two" - 2000, "3threeX" - 3000,
        "4fourX" - 400, "5fiveX" - 5000, "6six" - 6000, "7seven" - 7],
    map.from_assoc_list(AL1, Map1),
    map.from_assoc_list(AL2, Map2),
    map.from_assoc_list(AL3, Map3),
    map.from_assoc_list(AL4, Map4),

    test_ops(0, [], !IO),
    test_ops(1, [Map1], !IO),
    test_ops(2, [Map1, Map2], !IO),
    test_ops(3, [Map1, Map2, Map3], !IO),
    test_ops(4, [Map1, Map2, Map3, Map4], !IO).

:- pred test_ops(int::in, list(map(string, int))::in, io::di, io::uo) is det.

test_ops(TestNum, Maps, !IO) :-
    list.length(Maps, NumMaps),
    io.write_string("-----------------------------------\n", !IO),
    io.format("\nTEST %d: %d input maps\n", [i(TestNum), i(NumMaps)], !IO),
    list.map(map.to_sorted_assoc_list, Maps, ALs),
    list.foldl(io.write_line, ALs, !IO),

    (
        Maps = []
    ;
        Maps = [HeadMap | TailMaps],

        map.common_subset_list(Maps) = Common,
        map.to_sorted_assoc_list(Common, CommonAL),
        io.write_string("\ncommon_subset_list\n", !IO),
        io.write_line(CommonAL, !IO),

        map.intersect_list(int_add, HeadMap, TailMaps, Intersect),
        map.to_sorted_assoc_list(Intersect, IntersectAL),
        io.write_string("\nintersect_list\n", !IO),
        io.write_line(IntersectAL, !IO),

        map.union_list(int_add, HeadMap, TailMaps, Union),
        map.to_sorted_assoc_list(Union, UnionAL),
        io.write_string("\nunion_list\n", !IO),
        io.write_line(UnionAL, !IO)
    ),

    io.nl(!IO).

:- pred int_add(int::in, int::in, int::out) is det.

int_add(A, B, A + B).
