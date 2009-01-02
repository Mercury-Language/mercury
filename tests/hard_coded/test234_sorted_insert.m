%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% This test case tests the tree234 library module's conversion of sorted lists
% directly to trees.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module test234_sorted_insert.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module tree234.

main(!IO) :-
    test_upto(1000, 0, 0, Failed, !IO),
    ( Failed = 0 ->
        io.write_string("all tests passed\n", !IO)
    ;
        io.format("%d tests failed\n", [i(Failed)], !IO)
    ).

:- pred test_upto(int::in, int::in, int::in, int::out, io::di, io::uo) is det.

test_upto(Max, Cur, !Failed, !IO) :-
    ( Cur > Max ->
        true
    ;
        test(Cur, !Failed, !IO),
        test_upto(Max, Cur + 1, !Failed, !IO)
    ).

:- pred test(int::in, int::in, int::out, io::di, io::uo) is det.

test(N, !Failed, !IO) :-
    trace [io(!IO), compile_time(flag("progress"))] (
        io.format("test %d:\n", [i(N)], !IO)
    ),

    iota(N, List),
    tree234.from_sorted_assoc_list(List, Tree),

    trace [io(!IO), compile_time(flag("print_tree"))] (
        io.format("test %d tree:\n", [i(N)], !IO),
        io.write(Tree, !IO),
        io.nl(!IO)
    ),

    well_formed(Tree, MaybeDepth),
    tree234_to_assoc_list(Tree, TreeList),
    list.sort(TreeList, SortedTreeList),

    rev_iota(N, RevList),
    tree234.from_rev_sorted_assoc_list(RevList, RevTree),

    trace [io(!IO), compile_time(flag("print_tree"))] (
        io.format("test %d tree:\n", [i(N)], !IO),
        io.write(RevTree, !IO),
        io.nl(!IO)
    ),

    well_formed(RevTree, RevMaybeDepth),
    tree234_to_assoc_list(RevTree, RevTreeList),
    list.sort(RevTreeList, RevSortedTreeList),

    (
        MaybeDepth = yes(_),
        RevMaybeDepth = yes(_),
        TreeList = SortedTreeList,
        RevTreeList = RevSortedTreeList
    ->
        true
    ;
        !:Failed = !.Failed + 1
    ),

    trace [io(!IO), compile_time(flag("progress"))] (
        io.nl(!IO)
    ).

:- pred iota(int::in, assoc_list(int)::out) is det.

iota(N, List) :-
    iota_2(N, 1, [], RevList),
    list.reverse(RevList, List).

:- pred rev_iota(int::in, assoc_list(int)::out) is det.

rev_iota(N, RevList) :-
    iota_2(N, 1, [], RevList).

:- pred iota_2(int::in, int::in,
    assoc_list(int, int)::in, assoc_list(int, int)::out) is det.

iota_2(Max, N, !RevList) :-
    ( N > Max ->
        true
    ;
        !:RevList = [N - N | !.RevList],
        iota_2(Max, N + 1, !RevList)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
