%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%-----------------------------------------------------------------------------%
%
% This test case is designed to test the correctness of the program
% transformation performed by compiler/format_call.m and the associated
% code in compiler/simplify.m.

:- module opt_format.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-	
	io.write_string(test_string_format_1(42, 'x', "HAL"), !IO),
	io.write_string(test_string_format_2(142, 'y', "IBM"), !IO),
	io.write_string(test_string_format_2(242, 'z', "JCN"), !IO),
	io.write_string(test_string_format_2(342, 'v', "KDO"), !IO),
    test_io_format_1(42, 'a', "WHAL", !IO),
    test_io_format_2(142, 'b', "WIBM", !IO),
    test_io_format_2(242, 'c', "WJCN", !IO),
    test_io_format_2(342, 'd', "WKDO", !IO).

:- func test_string_format_1(int, char, string) = string.

test_string_format_1(Int, Char, Str) =
    string.format("abc_%d_def_%%%c_ghi_%s_jkl\\\n", [i(Int), c(Char), s(Str)]).

:- func test_string_format_2(int, char, string) = string.

test_string_format_2(Int, Char, Str) = Result :-
    PolyStr = s(Str),
    (
        Int > 300
    ->
        Tail = [c(Char), PolyStr],
        IntX = Int + 1,
        Result = string.format("abc_%d_def_%%%c_ghi_%s_jkl\\\n",
            [i(IntX) | Tail])
    ;
        Int > 200,
        IntY = Int - 1,
        FmtStr = "cba_%s_fed_%%%c_ghi_%d_jkl\\\n",
        Values = [PolyStr, c(Char), i(IntY)]
    ->
        Result = string.format(FmtStr, Values)
    ;
        IntX = Int + 1,
        Tail = [PolyStr],
        Result = string.format("cba_%c_def_%%%d_ghi_%s_jkl\\\n",
            [c(Char), i(IntX) | Tail])
    ).

:- pred test_io_format_1(int::in, char::in, string::in, io::di, io::uo) is det.

test_io_format_1(Int, Char, Str, !IO) :-
    io.format("abc_%d_def_%%%c_ghi_%s_jkl\\\n", [i(Int), c(Char), s(Str)], !IO).

:- pred test_io_format_2(int::in, char::in, string::in, io::di, io::uo) is det.

test_io_format_2(Int, Char, Str, !IO) :-
    PolyStr = s(Str),
    io.output_stream(OutStream, !IO),
    (
        Int > 300
    ->
        Tail = [c(Char), PolyStr],
        IntX = Int + 1,
        io.format("abc_%d_def_%%%c_ghi_%s_jkl\\\n", [i(IntX) | Tail], !IO)
    ;
        Int > 200,
        IntY = Int - 1,
        FmtStr = "cba_%s_fed_%%%c_ghi_%d_jkl\\\n",
        Values = [PolyStr, c(Char), i(IntY)]
    ->
        io.format(FmtStr, Values, !IO)
    ;
        IntX = Int + 1,
        Tail = [PolyStr],
        io.format(OutStream, "cba_%c_def_%%%d_ghi_%s_jkl\\\n",
            [c(Char), i(IntX) | Tail], !IO)
    ).
