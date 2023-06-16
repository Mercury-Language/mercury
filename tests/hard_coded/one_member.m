%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module one_member.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module solutions.

:- type set_ctree234(T)
    --->    ct(int, set_tree234(T))
    where equality is one_member.equal,
    comparison is one_member.compare_total_order.

:- type set_tree234(T)
    --->    empty
    ;       two(T, set_tree234(T), set_tree234(T))
    ;       three(T, T, set_tree234(T), set_tree234(T), set_tree234(T))
    ;       four(T, T, T, set_tree234(T), set_tree234(T),
                set_tree234(T), set_tree234(T)).

main(!IO) :-
    Set = ct(4, two("two", two("one", empty, empty),
        three("three", "four", empty, empty, empty))),
    solutions(one_member(Set), Solns),
    io.write(Solns, !IO),
    io.nl(!IO).

:- pred one_member(set_ctree234(T)::in, T::out) is nondet.

one_member(Set, Item) :-
    promise_equivalent_solution_sets [Item] (
        arbitrary [Tree] Set = ct(_, Tree),
        do_one_member(Tree, Item)
    ).

:- pred do_one_member(set_tree234(T)::in, T::out) is nondet.

do_one_member(empty, _) :- fail.
do_one_member(two(E0, T0, T1), E) :-
    (
        E = E0
    ;
        do_one_member(T0, E)
    ;
        do_one_member(T1, E)
    ).
do_one_member(three(E0, E1, T0, T1, T2), E) :-
    (
        E = E0
    ;
        E = E1
    ;
        do_one_member(T0, E)
    ;
        do_one_member(T1, E)
    ;
        do_one_member(T2, E)
    ).
do_one_member(four(E0, E1, E2, T0, T1, T2, T3), E) :-
    (
        E = E0
    ;
        E = E1
    ;
        E = E2
    ;
        do_one_member(T0, E)
    ;
        do_one_member(T1, E)
    ;
        do_one_member(T2, E)
    ;
        do_one_member(T3, E)
    ).

:- pred equal(set_ctree234(T)::in, set_ctree234(T)::in) is semidet.

equal(SetA, SetB) :-
    promise_equivalent_solution_sets [] (
        arbitrary [SizeA, TreeA] SetA = ct(SizeA, TreeA),
        arbitrary [SizeB, TreeB] SetB = ct(SizeB, TreeB),
        SizeA = SizeB,
        do_to_sorted_list(TreeA, [], ListA),
        do_to_sorted_list(TreeB, [], ListB),
        ListA = ListB
    ).

:- pred compare_total_order(comparison_result::uo,
    set_ctree234(T)::in, set_ctree234(T)::in) is det.

compare_total_order(Result, SetA, SetB) :-
    promise_equivalent_solution_sets [Result] (
        arbitrary [SizeA, TreeA] SetA = ct(SizeA, TreeA),
        arbitrary [SizeB, TreeB] SetB = ct(SizeB, TreeB),
        ( if SizeA = SizeB then
            do_to_sorted_list(TreeA, [], ListA),
            do_to_sorted_list(TreeB, [], ListB),
            compare(Result, ListA, ListB)
        else
            compare(Result, SizeA, SizeB)
        )
    ).

:- pred do_to_sorted_list(set_tree234(T)::in, list(T)::in, list(T)::out)
    is det.

do_to_sorted_list(empty, L, L).
do_to_sorted_list(two(E0, T0, T1), L0, L) :-
    do_to_sorted_list(T1, L0, L1),
    do_to_sorted_list(T0, [E0 | L1], L).
do_to_sorted_list(three(E0, E1, T0, T1, T2), L0, L) :-
    do_to_sorted_list(T2, L0, L1),
    do_to_sorted_list(T1, [E1 | L1], L2),
    do_to_sorted_list(T0, [E0 | L2], L).
do_to_sorted_list(four(E0, E1, E2, T0, T1, T2, T3), L0, L) :-
    do_to_sorted_list(T3, L0, L1),
    do_to_sorted_list(T2, [E2 | L1], L2),
    do_to_sorted_list(T1, [E1 | L2], L3),
    do_to_sorted_list(T0, [E0 | L3], L).
