%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module test_cord_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.
:- import_module solutions.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("Test list and rev_list\n", !IO),
    solutions(gen_cord3, Cords),
    list.foldl(test_list_and_rev_list, Cords, !IO),

    io.write_string("\nTest cons_list\n", !IO),
    list.foldl(test_cons_list([111, 222]), Cords, !IO),
    io.write_string("done.\n", !IO),

    io.write_string("\nTest folds\n", !IO),
    list.foldl(test_folds, Cords, !IO),
    io.write_string("done.\n", !IO),

    io.write_string("\nTest cord_list_to_cord and cord_list_to_list\n", !IO),
    solutions(gen_cord_list, CordLists),
    list.foldl(test_cord_list_funcs, CordLists, !IO),
    io.write_string("done.\n", !IO).

:- pred test_list_and_rev_list(cord(int)::in, io::di, io::uo) is det.

test_list_and_rev_list(Cord, !IO) :-
    io.write_line(Cord, !IO),

    List = list(Cord),
    io.write_line(List, !IO),

    RevList = rev_list(Cord),
    io.write_line(RevList, !IO),

    expect(unify(length(List), length(Cord)),
        $module, $pred, "length(List) != length(Cord)"),

    expect(unify(List, reverse(RevList)),
        $module, $pred, "List != reverse(RevList)").

:- pred test_cons_list(list(int)::in, cord(int)::in, io::di, io::uo) is det.

test_cons_list(List, Cord0, !IO) :-
    CordA = cord.from_list(List) ++ Cord0,
    cord.cons_list(List, Cord0, CordB),
    ListA = cord.list(CordA),
    ListB = cord.list(CordB),
    ( if ListA = ListB then
        io.write_line(ListA, !IO)
    else
        io.format("disagree: %s vs %s\n",
            [s(string(ListA)), s(string(ListB))], !IO)
    ).

:- pred test_folds((cord(int))::in, io::di, io::uo) is det.

test_folds(Cord, IO, IO) :-
    List = cord.list(Cord),
    RevList = cord.rev_list(Cord),

    cord.foldl(list.cons, Cord, []) = FoldlResultA,
    cord.foldl_pred(list.cons, Cord, [], FoldlResultB),
    expect(unify(FoldlResultA, RevList), $module, $pred, "foldl wrong"),
    expect(unify(FoldlResultB, RevList), $module, $pred, "foldl_pred wrong"),

    cord.foldr(list.cons, Cord, []) = FoldrResultA,
    cord.foldr_pred(list.cons, Cord, [], FoldrResultB),
    expect(unify(FoldrResultA, List), $module, $pred, "foldr wrong"),
    expect(unify(FoldrResultB, List), $module, $pred, "foldr_pred wrong").

:- pred test_cord_list_funcs(list(cord(int))::in, io::di, io::uo) is det.

test_cord_list_funcs(Cords, IO, IO) :-
    List1 = cord_list_to_list(Cords),
    List2 = list(cord_list_to_cord(Cords)),
    List3 = condense(map(list, Cords)),
    expect(unify(List1, List2), $module, $pred, "List1 != List2"),
    expect(unify(List1, List3), $module, $pred, "List1 != List3").

%---------------------------------------------------------------------------%

:- pred gen_cord1(cord(int)::out) is multi.

gen_cord1(empty).
gen_cord1(singleton(1)).
gen_cord1(from_list([2, 3, 4])).

:- pred gen_cord2(cord(int)::out) is multi.

gen_cord2(C) :-
    (
        gen_cord1(C)
    ;
        gen_cord1(A),
        gen_cord1(B),
        C = A ++ B
    ).

:- pred gen_cord3(cord(int)::out) is multi.

gen_cord3(C) :-
    (
        gen_cord2(C)
    ;
        gen_cord2(A),
        gen_cord2(B),
        C = A ++ B
    ).

:- pred gen_cord_list(list(cord(int))::out) is multi.

gen_cord_list(L) :-
    (
        L = []
    ;
        gen_cord2(A),
        (
            L = [A]
        ;
            gen_cord2(B),
            (
                L = [A, B]
            ;
                gen_cord2(C),
                L = [A, B, C]
            )
        )
    ).
