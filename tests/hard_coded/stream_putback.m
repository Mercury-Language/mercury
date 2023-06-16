%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module stream_putback.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.open_input("stream_putback.data", OpenRes, !IO),
    (
        OpenRes = ok(Stream),
        print_read(Stream, !IO),            % a
        % Can only expect one pushback.
        print_putback(Stream, 'X', !IO),
        print_read(Stream, !IO),            % X
        print_read(Stream, !IO),            % b
        print_read(Stream, !IO),            % c
        print_read(Stream, !IO),            % \n
        print_read(Stream, !IO),            % eof
        io.close_input(Stream, !IO)
    ;
        OpenRes = error(Error),
        error(io.error_message(Error))
    ).

:- pred print_read(io.input_stream::in, io::di, io::uo) is det.

print_read(Stream, !IO) :-
    io.read_char(Stream, ReadRes, !IO),
    (
        ReadRes = ok(Char),
        io.write_string("Read: ", !IO),
        io.write_line(Char, !IO)
    ;
        ReadRes = eof,
        io.write_string("Read: eof\n", !IO)
    ;
        ReadRes = error(Error),
        error(io.error_message(Error))
    ).

:- pred print_putback(io.input_stream::in, char::in, io::di, io::uo) is det.

print_putback(Stream, Char, !IO) :-
    io.putback_char(Stream, Char, !IO),
    io.write_string("Put back: ", !IO),
    io.write_line(Char, !IO).
