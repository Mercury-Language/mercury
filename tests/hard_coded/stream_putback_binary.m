%-----------------------------------------------------------------------------%

:- module stream_putback_binary.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%


main(!IO) :-
    io.open_binary_input("stream_putback.data", OpenRes, !IO),
    (
        OpenRes = ok(Stream),
        print_position(Stream, !IO),        % pos 0
        print_read_byte(Stream, !IO),       % pos 1
        print_relseek(Stream, 1, !IO),      % pos 2
        print_read_byte(Stream, !IO),       % pos 3
        print_putback(Stream, 'X', !IO),    % pos 2
        print_read_byte(Stream, !IO),       % X, pos 3
        print_read_byte(Stream, !IO),       % \n, pos 4
        print_read_byte(Stream, !IO),       % eof, pos 4
        print_putback(Stream, 'Y', !IO),    % pos 3
        print_relseek(Stream, -2, !IO),     % pos 1; pushback dropped
        print_read_byte(Stream, !IO),       % b, pos 2
        io.close_binary_input(Stream, !IO)
    ;
        OpenRes = error(Error),
        error(io.error_message(Error))
    ).

:- pred print_read_byte(io.binary_input_stream::in, io::di, io::uo) is det.

print_read_byte(Stream, !IO) :-
    io.read_byte(Stream, ReadRes, !IO),
    (
        ReadRes = ok(Byte),
        io.write_string("Read: ", !IO),
        io.write(det_from_int(Byte) : char, !IO),
        io.nl(!IO)
    ;
        ReadRes = eof,
        io.write_string("Read: eof\n", !IO)
    ;
        ReadRes = error(Error),
        error(io.error_message(Error))
    ),
    print_position(Stream, !IO).

:- pred print_putback(io.binary_input_stream::in, char::in, io::di, io::uo)
    is det.

print_putback(Stream, Char, !IO) :-
    io.putback_byte(Stream, char.to_int(Char), !IO),
    io.write_string("Put back: ", !IO),
    io.write(Char, !IO),
    io.nl(!IO),
    print_position(Stream, !IO).

:- pred print_relseek(io.binary_input_stream::in, int::in, io::di, io::uo)
    is det.

print_relseek(Stream, Offset, !IO) :-
    io.seek_binary_input(Stream, cur, Offset, !IO),
    io.format("Seek: %+d\n", [i(Offset)], !IO),
    print_position(Stream, !IO).

:- pred print_position(io.binary_input_stream::in, io::di, io::uo) is det.

print_position(Stream, !IO) :-
    io.binary_input_stream_offset(Stream, Offset, !IO),
    io.format("Position: %d\n\n", [i(Offset)], !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
