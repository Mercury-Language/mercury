%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test writing and reading uint8s with binary streams.
%
%---------------------------------------------------------------------------%

:- module write_binary_uint8.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module uint8.

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
        list.foldl(write_binary_uint8(OutputFile), data, !IO),
        io.close_binary_output(OutputFile, !IO),

        io.open_binary_input(FileName, OpenInputResult, !IO),
        (
            OpenInputResult = ok(InputFile),
            read_uint8s(InputFile, [], ReadResult, !IO),
            (
                ReadResult = ok(RevUInt8s),
                io.close_binary_input(InputFile, !IO),
                list.reverse(RevUInt8s, UInt8s),
                ( if UInt8s = data then
                    io.print_line("PASSED", !IO)
                else
                    io.print_line("FAILED", !IO)
                )
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

:- pred read_uint8s(io.binary_input_stream::in,
    list(uint8)::in, io.res(list(uint8))::out, io::di, io::uo) is det.

read_uint8s(InputFile, !.Us, Result, !IO) :-
    io.read_binary_uint8(InputFile, ReadResult, !IO),
    (
        ReadResult = ok(U),
        !:Us = [U | !.Us],
        read_uint8s(InputFile, !.Us, Result, !IO)
    ;
        ReadResult = eof,
        Result = ok(!.Us)
    ;
        ReadResult = error(IO_Error),
        Result = error(IO_Error)
    ).

:- func data = list(uint8).

data = Uint8s :-
    int.fold_down(add_uint8, 0, 255, [], Uint8s).

:- pred add_uint8(int::in,
    list(uint8)::in, list(uint8)::out) is det.

add_uint8(I, !Us) :-
    U = uint8.cast_from_int(I),
    !:Us = [U | !.Us].
