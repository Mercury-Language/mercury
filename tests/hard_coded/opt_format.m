%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
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
:- import_module stream.
:- import_module stream.string_writer.
:- import_module string.

main(!IO) :-
    io.write_string(test_string_format_1(42,  'x', "HAL", 1111i64, 2222u64),
        !IO),
    io.write_string(test_string_format_2(142, 'y', "IBM"), !IO),
    io.write_string(test_string_format_2(242, 'z', "JCN"), !IO),
    io.write_string(test_string_format_2(342, 'v', "KDO"), !IO),
    io.nl(!IO),
    test_io_format_1(42,  'a', "WHAL", 3333i64, 4444u64, !IO),
    test_io_format_2(142, 'b', "WIBM", !IO),
    test_io_format_2(242, 'c', "WJCN", !IO),
    test_io_format_2(342, 'd', "WKDO", !IO),
    io.nl(!IO),
    io.output_stream(OutStream, !IO),
    test_stream_writer_format_1(OutStream, 42,  'e', "XHAL", 5555i64, 6666u64,
        !IO),
    test_stream_writer_format_2(OutStream, 142, 'f', "XIBM", !IO),
    test_stream_writer_format_2(OutStream, 242, 'g', "XJCN", !IO),
    test_stream_writer_format_2(OutStream, 342, 'h', "XKDO", !IO).

%---------------------------------------------------------------------------%

:- func test_string_format_1(int, char, string, int64, uint64) = string.

test_string_format_1(Int, Char, Str, Int64, UInt64) =
    string.format("abc_%d_def_%%%c_ghi_%s_jkl_%d_mno_%u_pqr\\\n",
        [i(Int), c(Char), s(Str), i64(Int64), u64(UInt64)]).

:- func test_string_format_2(int, char, string) = string.

test_string_format_2(Int, Char, Str) = Result :-
    PolyStr = s(Str),
    (
        Int > 300
    ->
        Tail = [c(Char), PolyStr],
        IntX = Int + 1,
        Result = string.format("abc_%04d_def_%%%c_ghi_%s_jkl\\\n",
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
        Result = string.format("cba_%c_def_%%%d_ghi_%-7s_jkl\\\n",
            [c(Char), i(IntX) | Tail])
    ).

%---------------------------------------------------------------------------%

:- pred test_io_format_1(int::in, char::in, string::in, int64::in, uint64::in,
    io::di, io::uo) is det.

test_io_format_1(Int, Char, Str, Int64, UInt64, !IO) :-
    io.format("abc_%d_def_%%%c_ghi_%s_jkl_%d_mno_%u_pqr\\\n",
        [i(Int), c(Char), s(Str), i64(Int64), u64(UInt64)], !IO).

:- pred test_io_format_2(int::in, char::in, string::in, io::di, io::uo) is det.

test_io_format_2(Int, Char, Str, !IO) :-
    PolyStr = s(Str),
    io.output_stream(OutStream, !IO),
    (
        Int > 300
    ->
        Tail = [c(Char), PolyStr],
        IntX = Int + 1,
        io.format("abc_%05.3d_def_%%%c_ghi_%5s_jkl\\\n", [i(IntX) | Tail], !IO)
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

%---------------------------------------------------------------------------%

:- pred test_stream_writer_format_1(Stream::in, int::in, char::in, string::in,
    int64::in, uint64::in, State::di, State::uo) is det
    <= stream.writer(Stream, string, State).

test_stream_writer_format_1(Stream, Int, Char, Str, Int64, UInt64, !State) :-
    stream.string_writer.format(Stream, "abc_%d_def_%%%c_ghi_%s_jkl_%d_mno_%u_pqr\\\n",
        [i(Int), c(Char), s(Str), i64(Int64), u64(UInt64)], !State).

:- pred test_stream_writer_format_2(Stream::in, int::in, char::in, string::in,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

test_stream_writer_format_2(Stream, Int, Char, Str, !State) :-
    PolyStr = s(Str),
    (
        Int > 300
    ->
        Tail = [c(Char), PolyStr],
        IntX = Int + 1,
        stream.string_writer.format(Stream,
            "abc_%05.3d_def_%%%c_ghi_%5s_jkl\\\n", [i(IntX) | Tail], !State)
    ;
        Int > 200,
        IntY = Int - 1,
        FmtStr = "cba_%s_fed_%%%c_ghi_%d_jkl\\\n",
        Values = [PolyStr, c(Char), i(IntY)]
    ->
        stream.string_writer.format(Stream, FmtStr, Values, !State)
    ;
        IntX = Int + 1,
        Tail = [PolyStr],
        stream.string_writer.format(Stream, "cba_%c_def_%%%d_ghi_%s_jkl\\\n",
            [c(Char), i(IntX) | Tail], !State)
    ).
