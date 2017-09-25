%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test writing of int16, uint16, in32 and uint32 to binary file streams.

:- module write_binary_multibyte_int.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

main(!IO) :-
    io.make_temp_file(MakeTempResult, !IO),
    (
        MakeTempResult = ok(TempFileName),
        main_2(TempFileName, !IO)
    ;
        MakeTempResult = error(Msg),
        io.print_line(Msg, !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred main_2(string::in, io::di, io::uo) is det.

main_2(FileName, !IO) :-
    io.open_binary_output(FileName, OpenOutputResult, !IO),
    (
        OpenOutputResult = ok(OutputFile),
        io.write_binary_int16_le(OutputFile, 0x0b0a_i16, !IO),
        io.write_binary_int16_be(OutputFile, 0x0b0a_i16, !IO),
        io.write_binary_uint16_le(OutputFile, 0xbbaa_u16, !IO),
        io.write_binary_uint16_be(OutputFile, 0xbbaa_u16, !IO),
        io.write_binary_int32_le(OutputFile, 0x0d0c0b0a_i32, !IO),
        io.write_binary_int32_be(OutputFile, 0x0d0c0b0a_i32, !IO),
        io.write_binary_uint32_le(OutputFile, 0xddccbbaa_u32, !IO),
        io.write_binary_uint32_be(OutputFile, 0xddccbbaa_u32, !IO),
        io.close_binary_output(OutputFile, !IO),

        io.open_binary_input(FileName, OpenInputResult, !IO),
        (
            OpenInputResult = ok(InputFile),
            read_bytes(InputFile, [], ReadResult, !IO),
            (
                ReadResult = ok(Bytes),
                io.close_binary_input(InputFile, !IO),
                io.write_list(Bytes, " ", print_byte, !IO),
                io.nl(!IO)
            ;
                ReadResult = error(IO_Error),
                io.error_message(IO_Error, Msg),
                io.print_line(Msg, !IO)
            )
        ;
            OpenInputResult = error(IO_Error),
            io.error_message(IO_Error, Msg),
            io.print_line(Msg, !IO)
        )
    ;
        OpenOutputResult = error(IO_Error),
        io.error_message(IO_Error, Msg),
        io.print_line(Msg, !IO)
    ),
    io.remove_file(FileName, _, !IO).

:- pred print_byte(int::in, io::di, io::uo) is det.

print_byte(Byte, !IO) :-
    io.format("0x%0.2x", [i(Byte)], !IO).

:- pred read_bytes(io.binary_input_stream::in,
    list(int)::in, io.res(list(int))::out, io::di, io::uo) is det.

read_bytes(InputFile, !.Bytes, Result, !IO) :-
    io.read_byte(InputFile, ReadResult, !IO),
    (
        ReadResult = ok(Byte),
        !:Bytes = [Byte | !.Bytes],
        read_bytes(InputFile, !.Bytes, Result, !IO)
    ;
        ReadResult = eof,
        list.reverse(!Bytes),
        Result = ok(!.Bytes)
    ;
        ReadResult = error(IO_Error),
        Result = error(IO_Error)
    ).

%---------------------------------------------------------------------------%
:- end_module write_binary_multibyte_int.
%---------------------------------------------------------------------------%
