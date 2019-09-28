%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% This a regression test for the issue in commit 3e3dbab
% (Github pull req #76): when reading a bitmap from a binary file stream the
% size of the result bitmap was being set based on the size of the file
% without regard for our current position in the stream. This was leading
% to an abort since less bytes than % expected ended up being read from
% the stream.
%---------------------------------------------------------------------------%

:- module read_bitmap_size.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bitmap.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.open_binary_output(test_file, OpenResult, !IO),
    (
        OpenResult = ok(TestFile),
        list.foldl(write_binary_uint8(TestFile),
            [1u8, 2u8, 4u8, 8u8, 16u8, 32u8, 64u8, 128u8, 255u8], !IO),
        io.close_binary_output(TestFile, !IO),
        do_test(0, !IO),
        do_test(1, !IO),
        do_test(2, !IO),
        do_test(4, !IO),
        do_test(8, !IO),
        do_test(9, !IO)
    ;
        OpenResult = error(IOError),
        io.error_message(IOError, Msg),
        io.print_line(Msg, !IO)
    ),
    io.remove_file(test_file, _, !IO).

:- pred do_test(int::in, io::di, io::uo) is det.

do_test(N, !IO) :-
    io.format("SKIP %d: ", [i(N)], !IO),
    io.open_binary_input(test_file, OpenResult, !IO),
    (
        OpenResult = ok(File),
        read_and_discard(File, N, !IO),
        io.read_binary_file_as_bitmap(File, ReadRes, !IO),
        (
            ReadRes = ok(Bitmap),
            ByteString = bitmap.to_byte_string(Bitmap),
            io.print_line(ByteString, !IO),
            io.close_binary_input(File, !IO)
        ;
            ReadRes = error(IOError),
            io.error_message(IOError, Msg),
            io.print_line(Msg, !IO)
        )
    ;
        OpenResult = error(IOError),
        io.error_message(IOError, Msg),
        io.print_line(Msg, !IO)
    ).

    % We could use seek here, but that isn't supported properly
    % by all backends.
:- pred read_and_discard(io.binary_input_stream::in, int::in,
    io::di, io::uo) is det.

read_and_discard(File, N, !IO) :-
    ( if N > 0 then
        read_binary_uint8(File, ReadResult, !IO),
        (
            ReadResult = ok(_),
            read_and_discard(File, N - 1, !IO)
        ;
            ReadResult = eof
        ;
            ReadResult = error(IOError),
            throw(software_error(io.error_message(IOError)))
        )
    else
        true
    ).

:- func test_file = string.

test_file = "read_bitmap_size.bin".

%---------------------------------------------------------------------------%
:- end_module read_bitmap_size.
%---------------------------------------------------------------------------%
