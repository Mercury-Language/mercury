%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test reading of binary uint64s.
%
%---------------------------------------------------------------------------%

:- module read_binary_uint64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.
:- import_module uint64.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(run_test(big_endian), test_cases, !IO),
    list.foldl(run_test(little_endian), test_cases, !IO),
    list.foldl(run_test(native), test_cases, !IO).

:- pred run_test(byte_order::in, test_case::in, io::di, io::uo) is det.

run_test(ByteOrder, TestBytes, !IO) :-
    io.remove_file(test_file, _, !IO),
    io.write_string("================\n", !IO),
    io.open_binary_output(test_file, OpenOutResult, !IO),
    (
        OpenOutResult = ok(OutFile),
        io.write_string("Input: ", !IO),
        io.write(TestBytes, !IO),
        ( if
            TestBytes = [Byte1, Byte2, Byte3, Byte4, Byte5, Byte6, Byte7, Byte8]
        then
            io.write_string(" (LE: ", !IO),
            io.write_uint64(from_bytes_le(Byte1, Byte2, Byte3, Byte4, Byte5, Byte6, Byte7, Byte8), !IO),
            io.write_string(") (BE: ", !IO),
            io.write_uint64(from_bytes_be(Byte1, Byte2, Byte3, Byte4, Byte5, Byte6, Byte7, Byte8), !IO),
            io.write_string(")\n", !IO)
        else
            io.nl(!IO)
        ),
        list.foldl(write_binary_uint8(OutFile), TestBytes, !IO),
        io.close_binary_output(OutFile, !IO),
        io.open_binary_input(test_file, OpenInResult, !IO),
        (
            OpenInResult = ok(InFile),
            (
                ByteOrder = big_endian,
                read_binary_uint64_be(InFile, ReadResult, !IO)
            ;
                ByteOrder = little_endian,
                read_binary_uint64_le(InFile, ReadResult, !IO)
            ;
                ByteOrder = native,
                read_binary_uint64(InFile, ReadResult, !IO)
            ),
            io.close_binary_input(InFile, !IO),
            (
                ReadResult = ok(ResultUInt16),
                io.write_string("Result: ", !IO),
                io.write(ResultUInt16, !IO),
                io.write_string(" (", !IO),
                describe_byte_order(ByteOrder, !IO),
                io.write_string(")\n", !IO)
            ;
                ReadResult = eof,
                io.write_string("Result: EOF (", !IO),
                describe_byte_order(ByteOrder, !IO),
                io.write_string(")\n", !IO)
            ;
                ReadResult = incomplete(Bytes),
                io.format("Result: Incomplete (%s) (", [s(string(Bytes))],
                    !IO),
                describe_byte_order(ByteOrder, !IO),
                io.write_string(")\n", !IO)
            ;
                ReadResult = error(IO_Error),
                io.format("Result: Error (%s)\n", [s(io.error_message(IO_Error))],
                    !IO)
            ),
            io.remove_file(test_file, _, !IO)
        ;
            OpenInResult = error(IO_Error),
            io.format("I/O ERROR: %s\n", [s(io.error_message(IO_Error))],
                !IO)
        )
    ;
        OpenOutResult = error(IO_Error),
        io.format("I/O ERROR: %s\n", [s(io.error_message(IO_Error))], !IO)
    ).

:- pred describe_byte_order(byte_order::in, io::di, io::uo) is det.

describe_byte_order(ByteOrder, !IO) :-
    (
        ByteOrder = big_endian,
        io.write_string("read big-endian", !IO)
    ;
        ByteOrder = little_endian,
        io.write_string("read little-endian", !IO)
    ;
        ByteOrder = native,
        io.write_string("read native byte order", !IO)
    ).

:- func test_file = string.

test_file = "read_binary_uint64.bin".

%---------------------------------------------------------------------------%

:- type byte_order
    --->    big_endian
    ;       little_endian
    ;       native.

:- type test_case == list(uint8).

:- func test_cases = list(test_case).

test_cases = [
    [],
    [1u8],
    [1u8, 2u8],
    [1u8, 2u8, 3u8],
    [1u8, 2u8, 3u8, 4u8],
    [1u8, 2u8, 3u8, 4u8, 5u8],
    [1u8, 2u8, 3u8, 4u8, 5u8, 6u8],
    [1u8, 2u8, 3u8, 4u8, 5u8, 7u8],
    [1u8, 2u8, 3u8, 4u8, 5u8, 7u8, 8u8],
    [0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 1u8],
    [0xffu8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8],
    [1u8, 1u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8]
].

%---------------------------------------------------------------------------%
:- end_module read_binary_uint64.
%---------------------------------------------------------------------------%
