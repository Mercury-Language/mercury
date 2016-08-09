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

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module deconstruct.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module ops.
:- import_module pair.
:- import_module pretty_printer.
:- import_module solutions.
:- import_module string.
:- import_module term_io.
:- import_module type_desc.
:- import_module univ.

:- type test_case
    --->    test_case(
                line_width              :: int,
                max_lines               :: int,
                fs_limit                :: func_symbol_limit,
                doc                     :: doc
            ).

:- type op_tree
    --->    x
    ;       - op_tree
    ;       op_tree + op_tree
    ;       op_tree - op_tree
    ;       op_tree * op_tree
    ;       op_tree / op_tree.

:- type church
    --->    zero
    ;       succ(church).

:- type non_canonical_bool
    --->    non_canonical_bool(int)
    where equality is non_canonical_bool_eq.

main(!IO) :-
    set_default_formatter("list",    "list",       1, fmt_list,   !IO),
    set_default_formatter("tree234", "tree234",    2, fmt_map,    !IO),
    set_default_formatter("builtin", "character",  0, fmt_char,   !IO),
    set_default_formatter("builtin", "float",      0, fmt_float,  !IO),
    set_default_formatter("builtin", "int",        0, fmt_int,    !IO),
    set_default_formatter("builtin", "string",     0, fmt_string, !IO),
    unsorted_solutions(test_case, TestCases),
    list.foldl(run_test_case, TestCases, !IO).

:- pred run_test_case(test_case::in, io::di, io::uo) is det.

run_test_case(TestCase, !IO) :-
    TestCase = test_case(LineWidth, MaxLines, Limit, Doc),
    set_default_params(pp_params(LineWidth, MaxLines, Limit), !IO),
    io.format("\nlimit = %s, max lines = %d, line width = %d",
        [s(string.string(Limit)), i(MaxLines), i(LineWidth)], !IO),
    Ruler = "\n|" ++ string.duplicate_char(('-'), LineWidth - 2) ++ "|\n",
    io.write_string(Ruler, !IO),
    pretty_printer.write_doc(Doc, !IO),
    io.write_string(Ruler, !IO).

:- func fmt_float : pretty_printer.formatter.

fmt_float(Univ, _) =
    ( if Univ = univ(Float) then
        str(string.float_to_string(Float))
    else
        str("?fmt_float?")
    ).

:- func fmt_int : pretty_printer.formatter.

fmt_int(Univ, _) =
    ( if Univ = univ(Int) then
        str(string.int_to_string(Int))
    else
        str("?fmt_int?")
    ).

:- func fmt_string : pretty_printer.formatter.

fmt_string(Univ, _) =
    ( if Univ = univ(String) then
        docs([str("\""), str(String), str("\"")])
    else
        str("?fmt_string?")
    ).

:- func fmt_char : pretty_printer.formatter.

fmt_char(Univ, _) =
    ( if Univ = univ(Char) then
        str(term_io.quoted_char(Char))
    else
        str("?fmt_char?")
    ).

:- func fmt_list(univ, list(type_desc)) = doc.

fmt_list(Univ, ArgDescs) = Doc :-
    ( if
        ArgDescs = [ArgDesc],
        has_type(Arg, ArgDesc),
        same_list_type(List, Arg),
        Value = univ_value(Univ),
        dynamic_cast(Value, List),
        UnivList = list.map(make_univ, List)
    then
        Doc = indent([str("["), format_list(UnivList, str(", ")), str("]")])
    else
        Doc = str("?fmt_list?")
    ).

:- pred same_list_type(list(T)::unused, T::unused) is det.

same_list_type(_, _).

:- type key_value(K, V)
    --->    (K -> V).

:- func fmt_map(univ, list(type_desc)) = doc.

fmt_map(Univ, ArgDescs) = Doc :-
    ( if
        ArgDescs = [KArgDesc, VArgDesc],
        has_type(K, KArgDesc),
        has_type(V, VArgDesc),
        same_map_type(Map, K, V),
        Value = univ_value(Univ),
        dynamic_cast(Value, Map)
    then
        UnivList =
            map.foldr(func(KK, VV, KVs) = [univ(KK -> VV) | KVs], Map, []),
        Doc = indent([
            str("map(["), format_list(UnivList, str(", ")), str("])")
        ])
    else
        Doc = str("?fmt_map?")
    ).

:- func fmt_susp_seq(int) = doc.

fmt_susp_seq(N) =
    ( if N =< 1 then
        str("1")
    else
        docs([
            group([nl, format(N), str(".")]),
            format_susp((func) = fmt_susp_seq(N - 1))
        ])
    ).

:- func make_univ(T) = univ.

make_univ(X) = univ(X).

:- pred same_map_type(map(K, V)::unused, K::unused, V::unused) is det.

same_map_type(_, _, _).

:- pred test_case(test_case::out) is multi.

test_case(test_case(LineWidth, MaxLines, Limit, Doc)) :-
    List = 1..100,
    ListUniv = list.map(func(X) = univ(X), List),
    MapStr = list.foldl(
        func(X, M) = M ^ elem(X) := "0x" ++ int_to_base_string(X, 16),
        List, map.init : map(int, string)),
    MapFloat = list.foldl(
        func(X, M) = M ^ elem(X) := float(X),
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
    ( Limit = linear(100)
    ; Limit = linear(10)
    ; Limit = triangular(100)
    ; Limit = triangular(10)
    ; Limit = linear(1)
    ; Limit = triangular(1)
    ),
    ( MaxLines = 10
    ; MaxLines = 3
    ),
    ( LineWidth = 78
    ; LineWidth = 38
    ),
    ( Doc = format(List)
    ; Doc = format_list(ListUniv, str(", "))
    ; Doc = format(MapFloat)
    ; Doc = format(MapStr)
    ; Doc = format(OpTree)
    ; Doc = format(Church)
    ; Doc = fmt_susp_seq(100)
    ; Doc = format(Tuple)
    ; Doc = format(Square)
    ; Doc = NonCanonTest
    ; Doc = IndentTest
    ).

:- func mk_op_tree(int) = op_tree.

mk_op_tree(N) =
    (      if N =< 3      then x
      else if N mod 5 = 0 then - mk_op_tree(N - 1)
      else if N mod 5 = 1 then mk_op_tree(0 + N/2) + mk_op_tree(0 + N/3)
      else if N mod 5 = 2 then mk_op_tree(1 + N/2) - mk_op_tree(1 + N/3)
      else if N mod 5 = 3 then mk_op_tree(0 + N/2) * mk_op_tree(2 + N/3)
      else                     mk_op_tree(1 + N/2) / mk_op_tree(0 + N/3)
    ).

:- pred non_canonical_bool_eq(non_canonical_bool::in, non_canonical_bool::in)
        is semidet.

non_canonical_bool_eq(A, B) :-
    promise_equivalent_solutions [AX, BX] (
        A = non_canonical_bool(AX),
        B = non_canonical_bool(BX)
    ),
    AX /\ 1 = BX /\ 1.

%---------------------------------------------------------------------------%
