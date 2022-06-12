%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a test of the operations of the ra_list module.
%

:- module ra_list_test.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module ra_list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    ra_list.init(Nil),
    ra_list.cons(1.0, Nil,  RA_1),
    ra_list.cons(2.0, RA_1, RA_2),
    ra_list.cons(3.0, RA_2, RA_3),
    ra_list.cons(4.0, RA_3, RA_4),

    ra_list.append(RA_4, RA_4, RA_8),
    ra_list.append(RA_8, RA_8, RA_16),

    RALists = [RA_1, RA_2, RA_3, RA_4, RA_8, RA_16],

    Searches = [0, 1, 2, 3, 4, 7, 8, 9, 14, 15, 16],
    Drops = [0, 1, 2, 5, 9],

    list.foldl(test_ops(Searches, Drops), RALists, !IO),

    ra_list.init(Strs_0),
    ra_list.cons("one",   Strs_0, Strs_1),
    ra_list.cons("two",   Strs_1, Strs_2),
    ra_list.cons("three", Strs_2, Strs_3),
    ra_list.cons("four",  Strs_3, Strs_4),
    ra_list.cons("five",  Strs_4, Strs_5),
    ra_list.cons("six",   Strs_5, Strs_6),
    ra_list.cons("seven", Strs_6, Strs_7),
    ra_list.cons("eight", Strs_7, Strs_8),
    ra_list.cons("nine",  Strs_8, Strs_9),
    ra_list.cons("ten",   Strs_9, Strs_10),

    Brackets = ra_list.map((func(S) = "<" ++ S ++ ">"), Strs_10),

    io.write_string("foldl (should be 10 to 1):\n", !IO),
    ra_list.foldl(io.write_line, Brackets, !IO),
    io.nl(!IO),

    io.write_string("foldr (should be 1 to 10):\n", !IO),
    RevBracketsStr = ra_list.foldr((func(S, A0) = A0 ++ S), Brackets, ""),
    io.write_line(RevBracketsStr, !IO),

    Items =
        ["one", "two", "three", "four", "five",
        "six", "seven", "eight", "nine", "ten",
        "eleven", "twelve", "thirteen", "fourteen", "fifteen",
        "sixteen", "seventeen"],
    test_direct_construction(Items, 1, 17, !IO).

:- pred test_ops(list(int)::in, list(int)::in, ra_list(float)::in,
    io::di, io::uo) is det.

test_ops(Searches, Drops, RAList, !IO) :-
    ra_list_to_list(RAList, List),
    io.write_line(List, !IO),
    ( if ra_list.head_tail(RAList, RAHead, RATail) then
        ra_list_to_list(RATail, Tail),
        io.format("head: %.1f\n", [f(RAHead)], !IO),
        io.format("tail: ", [], !IO),
        io.write_line(Tail, !IO)
    else
        io.format("ra_list.head_tail failed\n", [], !IO)
    ),
    ra_list.length(RAList, Len),
    io.format("length: %d\n", [i(Len)], !IO),
    list.foldl(test_index(RAList), Searches, !IO),
    list.foldl(test_drop(RAList), Drops, !IO),
    io.nl(!IO).

:- pred test_index(ra_list(float)::in, int::in, io::di, io::uo) is det.

test_index(RAList, Index, !IO) :-
    ( if ra_list.index0(RAList, Index, Val) then
        io.format("at index0 %d: %.1f\n", [i(Index), f(Val)], !IO)
    else
        io.format("at index0 %d: _\n", [i(Index)], !IO)
    ).

:- pred test_drop(ra_list(float)::in, int::in, io::di, io::uo) is det.

test_drop(RAList0, Drop, !IO) :-
    io.format("drop %d: ", [i(Drop)], !IO),
    ( if ra_list.drop(Drop, RAList0, RAList) then
        ra_list_to_list(RAList, List),
        io.write_line(List, !IO)
    else
        io.format("failed\n", [], !IO)
    ).

:- pred test_direct_construction(list(T)::in, int::in, int::in,
    io::di, io::uo) is det.

test_direct_construction(Items, CurN, MaxN, !IO) :-
    ( if CurN =< MaxN then
        list.det_take(CurN, Items, FirstNItems),
        construct_ra_list_indirectly(FirstNItems, IndirectRAList),
        list_to_ra_list(FirstNItems, DirectRAList),
        io.format("\nconstruction %d\n", [i(CurN)], !IO),
        io.write_line(DirectRAList, !IO),
        io.write_line(IndirectRAList, !IO),
        test_direct_construction(Items, CurN + 1, MaxN, !IO)
    else
        true
    ).

:- pred construct_ra_list_indirectly(list(T)::in, ra_list(T)::out) is det.

construct_ra_list_indirectly([], RAList) :-
    ra_list.init(RAList).
construct_ra_list_indirectly([H | T], RAList) :-
    construct_ra_list_indirectly(T, RAListTail),
    ra_list.cons(H, RAListTail, RAList).

%---------------------------------------------------------------------------%
