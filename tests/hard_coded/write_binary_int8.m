%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test writing and reading int8s with binary streams.
%
%---------------------------------------------------------------------------%

:- module write_binary_int8.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module int8.
:- import_module io.file.
:- import_module list.

main(!IO) :-
    io.file.make_temp_file(MakeTempResult, !IO),
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
        list.foldl(write_binary_int8(OutputFile), data, !IO),
        io.close_binary_output(OutputFile, !IO),

        io.open_binary_input(FileName, OpenInputResult, !IO),
        (
            OpenInputResult = ok(InputFile),
            read_int8s(InputFile, [], ReadResult, !IO),
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
    io.file.remove_file(FileName, _, !IO).

:- pred read_int8s(io.binary_input_stream::in,
    list(int8)::in, io.res(list(int8))::out, io::di, io::uo) is det.

read_int8s(InputFile, !.Us, Result, !IO) :-
    io.read_binary_int8(InputFile, ReadResult, !IO),
    (
        ReadResult = ok(U),
        !:Us = [U | !.Us],
        read_int8s(InputFile, !.Us, Result, !IO)
    ;
        ReadResult = eof,
        Result = ok(!.Us)
    ;
        ReadResult = error(IO_Error),
        Result = error(IO_Error)
    ).

:- func data = list(int8).

data = Uint8s :-
    int.fold_down(add_int8, -128, 127, [], Uint8s).

:- pred add_int8(int::in,
    list(int8)::in, list(int8)::out) is det.

add_int8(I, !Us) :-
    U = int8.cast_from_int(I),
    !:Us = [U | !.Us].
