%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Test the generation of error messages for invalid format_call pragmas.
%---------------------------------------------------------------------------%

:- module bad_format_call.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

main(!IO) :-
    arg_num_test_1("%s", [s("a")], !IO),

    ArgNumTest2 = arg_num_test_2("%s", [s("a")]),
    io.format("%s\n", [s(ArgNumTest2)], !IO),

    type_test_1("%s", [s("a")], !IO),

    TypeTest2 = type_test_2("%s", [s("a")]),
    io.format("%s\n", [s(TypeTest2)], !IO),

    mode_test_1("%s", _, [s("a")], !IO),

    ModeTest2 = mode_test_2("%s", [s("a")], _),
    io.format("%s\n", [s(ModeTest2)], !IO).

%---------------------------------------------------------------------------%

:- pred arg_num_test_1(string::in, list(poly_type)::in, io::di, io::uo) is det.
:- pragma format_call(pred(arg_num_test_1/4), format_string_values(-2, 5)).

arg_num_test_1(FormatStr, Values, !IO) :-
    io.format(FormatStr, Values, !IO).

:- func arg_num_test_2(string, list(poly_type)) = string.
:- pragma format_call(func(arg_num_test_2/2), format_string_values(3, -4)).

arg_num_test_2(FormatStr, Values) = Str :-
    string.format(FormatStr, Values, Str).

%---------------------------------------------------------------------------%

:- pred type_test_1(string::in, list(poly_type)::in, io::di, io::uo) is det.
:- pragma format_call(pred(type_test_1/4), format_string_values(2, 3)).

type_test_1(FormatStr, Values, !IO) :-
    io.format(FormatStr, Values, !IO).

:- func type_test_2(string, list(poly_type)) = string.
:- pragma format_call(func(type_test_2/2), format_string_values(2, 1)).

type_test_2(FormatStr, Values) = Str :-
    string.format(FormatStr, Values, Str).

%---------------------------------------------------------------------------%

:- pred mode_test_1(string::in, string::out, list(poly_type)::in,
    io::di, io::uo) is det.
:- pragma format_call(pred(mode_test_1/5), format_string_values(2, 3)).

mode_test_1(FormatStr0, FormatStr, Values, !IO) :-
    FormatStr = FormatStr0,
    io.format(FormatStr0, Values, !IO).

:- func mode_test_2(string::in, list(poly_type)::in, list(poly_type)::out)
    = (string::out) is det.
:- pragma format_call(func(mode_test_2/3), format_string_values(1, 3)).

mode_test_2(FormatStr, Values0, Values) = Str :-
    Values = Values0,
    string.format(FormatStr, Values, Str).

%---------------------------------------------------------------------------%
