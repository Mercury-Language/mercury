%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% test_pretty_printer.m
% Ralph Becket <rafe@csse.unimelb.edu.au>
% Tue Jun  5 16:19:10 EST 2007
%---------------------------------------------------------------------------%

:- module test_pretty_printer.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module test_pretty_printer_helper.

:- import_module float.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pretty_printer.
:- import_module string.
:- import_module univ.

%---------------------------------------------------------------------------%

main(!IO) :-
    setup_pretty_printer(!IO),
    test_cases(TestCases),
    list.foldl(run_test_case, TestCases, !IO).

%---------------------------------------------------------------------------%

:- pred test_cases(list(test_case)::out) is det.

test_cases(Docs) :-
    List = 1..100,
    ListUniv = list.map(func(X) = univ(X), List),
    MapStr = list.foldl(
        (func(X, M) = M ^ elem(X) := "0x" ++ int_to_base_string(X, 16)),
        List, map.init : map(int, string)),
    MapFloat = list.foldl(
        (func(X, M) = M ^ elem(X) := float(X)),
        List, map.init : map(int, float)),
    OpTree = mk_op_tree(200),
    Church = list.foldl(func(_, X) = succ(X), 1..10, zero),
    Tuple = {1, 2.0, "three", '4', {5}, "«ąąąąą»"},
    Square = list.duplicate(10, 1..10) : list(list(int)),
    IndentTest = docs([
        str("indentation test:"),
        indent("_1_", [nl, str(" one "),
            indent("_2_", [nl, str(" two "),
                indent("_3_", [nl, str(" three "),
                    indent("_4_", [nl, str(" four "),
                        indent("_5_", [nl, str(" five ")])])])])])
    ]),
    NonCanonTest = docs([
        format(non_canonical_bool(0)), nl,
        format(non_canonical_bool(1)), nl,
        format(non_canonical_bool(42)), nl,
        format(non_canonical_bool(43)), nl
    ]),

    Docs = [
        test_case("list",            format(List)),
        test_case("list_univ",       format_list(ListUniv, str(", "))),
        test_case("map_float",       format(MapFloat)),
        test_case("map_hext_str",    format(MapStr)),
        test_case("op_tree",         format(OpTree)),
        test_case("church",          format(Church)),
        test_case("lazy_countdown",  lazy_countdown(100)),
        test_case("tuple",           format(Tuple)),
        test_case("square",          format(Square)),
        test_case("noncanon",        NonCanonTest),
        test_case("indent",          IndentTest)
    ].

%---------------------------------------------------------------------------%

:- type church
    --->    zero
    ;       succ(church).

%---------------------------------------------------------------------------%

:- type op_tree
    --->    x
    ;       - op_tree
    ;       op_tree + op_tree
    ;       op_tree - op_tree
    ;       op_tree * op_tree
    ;       op_tree / op_tree.

:- func mk_op_tree(int) = op_tree.

mk_op_tree(N) =
    (    if N =< 3      then x
    else if N mod 5 = 0 then - mk_op_tree(N - 1)
    else if N mod 5 = 1 then mk_op_tree(0 + N/2) + mk_op_tree(0 + N/3)
    else if N mod 5 = 2 then mk_op_tree(1 + N/2) - mk_op_tree(1 + N/3)
    else if N mod 5 = 3 then mk_op_tree(0 + N/2) * mk_op_tree(2 + N/3)
    else                     mk_op_tree(1 + N/2) / mk_op_tree(0 + N/3)
    ).

%---------------------------------------------------------------------------%

:- type non_canonical_bool
    --->    non_canonical_bool(int)
    where equality is non_canonical_bool_eq.

:- pred non_canonical_bool_eq(non_canonical_bool::in, non_canonical_bool::in)
    is semidet.

non_canonical_bool_eq(A, B) :-
    promise_equivalent_solutions [AX, BX] (
        A = non_canonical_bool(AX),
        B = non_canonical_bool(BX)
    ),
    AX /\ 1 = BX /\ 1.

%---------------------------------------------------------------------------%

:- func lazy_countdown(int) = doc.

lazy_countdown(N) =
    ( if N =< 1 then
        str("1")
    else
        docs([
            group([nl, format(N), str(".")]),
            format_susp((func) = lazy_countdown(N - 1))
        ])
    ).

%---------------------------------------------------------------------------%
