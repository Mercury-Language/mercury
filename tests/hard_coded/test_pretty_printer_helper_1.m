%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Utilities for the test cases that test the pretty_printer's operation.
%
%---------------------------------------------------------------------------%

:- module test_pretty_printer_helper_1.

:- interface.

:- import_module io.
:- import_module list.
:- import_module pretty_printer.
:- import_module univ.

%---------------------------------------------------------------------------%

:- pred setup_pretty_printer(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- type maybe_print_test_value
    --->    do_not_print_value
    ;       do_print_value.

:- type test_case
    --->    test_case(string, test_data).

:- type test_data
    --->    td_univ(univ)
    ;       td_univ_list(list(univ), doc)
    ;       td_doc(doc).

:- pred run_test_case(list(int)::in, list(int)::in,
    list(func_symbol_limit)::in, maybe_print_test_value::in, test_case::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module map.
:- import_module string.
:- import_module term_io.
:- import_module type_desc.

%---------------------------------------------------------------------------%

setup_pretty_printer(!IO) :-
    set_default_formatter("builtin", "int",        0, fmt_int,    !IO),
    set_default_formatter("builtin", "float",      0, fmt_float,  !IO),
    set_default_formatter("builtin", "character",  0, fmt_char,   !IO),
    set_default_formatter("builtin", "string",     0, fmt_string, !IO),
    set_default_formatter("list",    "list",       1, fmt_list,   !IO),
    set_default_formatter("tree234", "tree234",    2, fmt_map,    !IO).

%---------------------%

:- func fmt_int(univ, list(type_desc)) = doc.

fmt_int(Univ, _) =
    ( if Univ = univ(Int) then
        str(string.int_to_string(Int))
    else
        str("?fmt_int?")
    ).

%---------------------%

:- func fmt_float(univ, list(type_desc)) = doc.

fmt_float(Univ, _) =
    ( if Univ = univ(Float) then
        str(string.float_to_string(Float))
    else
        str("?fmt_float?")
    ).

%---------------------%

:- func fmt_char(univ, list(type_desc)) = doc.

fmt_char(Univ, _) =
    ( if Univ = univ(Char) then
        str(term_io.quoted_char(Char))
    else
        str("?fmt_char?")
    ).

%---------------------%

:- func fmt_string(univ, list(type_desc)) = doc.

fmt_string(Univ, _) =
    ( if Univ = univ(String) then
        docs([str("\""), str(String), str("\"")])
    else
        str("?fmt_string?")
    ).

%---------------------%

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

:- func make_univ(T) = univ.

make_univ(X) = univ(X).

%---------------------%

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

:- type key_value(K, V)
    --->    (K -> V).

:- pred same_map_type(map(K, V)::unused, K::unused, V::unused) is det.

same_map_type(_, _, _).

%---------------------------------------------------------------------------%

run_test_case(LineWidths, MaxLines, Limits, MaybePrintDoc, TestCase, !IO) :-
    TestCase = test_case(Name, TestData),
    io.nl(!IO),
    io.write_string("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n", !IO),
    io.format("Test case %s\n", [s(Name)], !IO),
    io.write_string("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n", !IO),
    (
        TestData = td_univ(Univ),
        Value = univ_value(Univ),
        Doc = format(Value),
        (
            MaybePrintDoc = do_not_print_value
        ;
            MaybePrintDoc = do_print_value,
            io.print_line(Value, !IO)
        )
    ;
        TestData = td_univ_list(UnivList, SepDoc),
        Doc = format_list(UnivList, SepDoc)
    ;
        TestData = td_doc(Doc)
    ),
    run_test_case_loop_1(Doc, LineWidths, MaxLines, Limits, !IO).

:- pred run_test_case_loop_1(doc::in, list(int)::in, list(int)::in,   
    list(func_symbol_limit)::in, io::di, io::uo) is det.

run_test_case_loop_1(_Doc, [], _MaxLines, _Limits, !IO).
run_test_case_loop_1(Doc, [LineWidth | LineWidths], MaxLines, Limits, !IO) :-
    run_test_case_loop_2(Doc, LineWidth, MaxLines, Limits, !IO),
    run_test_case_loop_1(Doc, LineWidths, MaxLines, Limits, !IO).

:- pred run_test_case_loop_2(doc::in, int::in, list(int)::in,   
    list(func_symbol_limit)::in, io::di, io::uo) is det.

run_test_case_loop_2(_Doc, _LineWidth, [], _Limits, !IO).
run_test_case_loop_2(Doc, LineWidth, [MaxLine | MaxLines], Limits, !IO) :-
    run_test_case_loop_3(Doc, LineWidth, MaxLine, Limits, !IO),
    run_test_case_loop_2(Doc, LineWidth, MaxLines, Limits, !IO).

:- pred run_test_case_loop_3(doc::in, int::in, int::in,   
    list(func_symbol_limit)::in, io::di, io::uo) is det.

run_test_case_loop_3(_Doc, _LineWidth, _MaxLines, [], !IO).
run_test_case_loop_3(Doc, LineWidth, MaxLines, [Limit | Limits], !IO) :-
    run_test_case_with_params(Doc, LineWidth, MaxLines, Limit, !IO),
    run_test_case_loop_3(Doc, LineWidth, MaxLines, Limits, !IO).

:- pred run_test_case_with_params(doc::in, int::in, int::in,
    func_symbol_limit::in, io::di, io::uo) is det.

run_test_case_with_params(Doc, LineWidth, MaxLines, Limit, !IO) :-
    Params = pp_params(LineWidth, MaxLines, Limit),
    io.format("\nline width = %d, max lines = %d, limit = %s",
        [i(LineWidth), i(MaxLines), s(string.string(Limit))], !IO),
    Ruler = "\n|" ++ string.duplicate_char(('-'), LineWidth - 2) ++ "|\n",
    io.write_string(Ruler, !IO),
    set_default_params(Params, !IO),
    pretty_printer.write_doc(Doc, !IO),
    io.write_string(Ruler, !IO).

%---------------------------------------------------------------------------%
