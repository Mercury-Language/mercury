%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2016, 2018 The Mercury Team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module:               net.streams.
% Main Author:          Paul Bone
% Stability:            low
%
% Provide a streams interface for sockets.
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module net.streams.
:- interface.

:- import_module int.
:- import_module io.
:- import_module stream.
:- import_module string.

:- import_module net.sockets.

%-----------------------------------------------------------------------------%

:- type socket_stream.

:- func stream(socket) = socket_stream.

:- func socket(socket_stream) = socket.

:- type byte
    --->    byte(int).

:- type error.

:- instance error(streams.error).

:- instance stream(socket_stream, io).

%-----------------------------------------------------------------------------%

:- instance input(socket_stream, io).

    % XXX: This does not buffer reads, it is slow.
    %
:- instance reader(socket_stream, streams.byte, io, streams.error).

:- instance reader(socket_stream, line, io, streams.error).

%-----------------------------------------------------------------------------%

:- instance output(socket_stream, io).

    % XXX: This does not buffer writes, it is slow.
    %
:- instance writer(socket_stream, streams.byte, io).

:- instance writer(socket_stream, string, io).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- implementation.

:- import_module char.
:- import_module exception.
:- import_module bitmap.
:- import_module list.
:- import_module maybe.
:- import_module require.

:- type socket_stream
    --->    socket_stream(socket).

:- type error
    --->    error(string).

stream(Socket) = socket_stream(Socket).

socket(socket_stream(Socket)) = Socket.

%-----------------------------------------------------------------------------%

:- instance error(streams.error) where [
        error_message(error(Str)) = Str
    ].

:- instance stream(socket_stream, io) where [
        pred(name/4) is stream_name
].

:- pred stream_name(socket_stream::in, name::out, io::di, io::uo) is det.

stream_name(socket_stream(_Socket), Name, !IO) :-
    Name = "a socket".

%-----------------------------------------------------------------------------%

:- instance input(socket_stream, io) where [].

:- instance reader(socket_stream, streams.byte, io, streams.error) where [
        pred(get/4) is get_byte
    ].

:- pred get_byte(socket_stream::in, result(streams.byte, streams.error)::out,
    io::di, io::uo) is det.

get_byte(socket_stream(Socket), Result, !IO) :-
    read(Socket, 1, ReadResult, !IO),
    (
        ReadResult = ok(Bitmap),
        ( if num_bytes(Bitmap) = 1 then
            Byte = Bitmap ^ byte(0),
            Result = ok(byte(Byte))
        else if num_bytes(Bitmap) = 0 then
            Result = eof
        else
            unexpected($file, $pred,
                "Read returned unexpected number of bytes")
        )
    ;
        ReadResult = eof,
        Result = eof
    ;
        ReadResult = error(String),
        Result = error(error(String))
    ).

:- instance reader(socket_stream, line, io, streams.error) where [
        pred(get/4) is get_line
    ].

:- pred get_line(socket_stream::in, result(line, streams.error)::out,
    io::di, io::uo) is det.

get_line(Stream, Result, !IO) :-
    get_chars_until_nl(Stream, [], Result0, !IO),
    (
        Result0 = ok(RevChars),
        Result = ok(line(from_rev_char_list(RevChars)))
    ;
        Result0 = eof,
        Result = eof
    ;
        Result0 = error(Error),
        Result = error(Error)
    ).

:- pred get_chars_until_nl(Stream::in, list(char)::in,
        result(list(char), Error)::out, State::di, State::uo) is det
    <= reader(Stream, streams.byte, State, Error).

get_chars_until_nl(Stream, Chars0, Result, !IO) :-
    get(Stream, ResByte, !IO),
    (
        ResByte = ok(byte(Byte)),
        ( char.from_int(Byte, Char) ->
            ( if
                ( Char = '\n'
                ; Char = '\r'
                )
            then
                Result = ok(Chars0)
            else
                Chars1 = [Char | Chars0],
                get_chars_until_nl(Stream, Chars1, Result, !IO)
            )
        ;
            unexpected($file, $pred, "Encoding error")
        )
    ;
        ResByte = eof,
        Result = eof
    ;
        ResByte = error(Error),
        Result = error(Error)
    ).


%-----------------------------------------------------------------------------%

:- instance output(socket_stream, io) where [
        pred(flush/3) is flush_noop
    ].

:- pred flush_noop(socket_stream::in, io::di, io::uo) is det.

flush_noop(_, !IO).

    % XXX: This does not buffer writes, it is slow.
    %
:- instance writer(socket_stream, streams.byte, io) where [
        pred(put/4) is put_byte
    ].

:- pred put_byte(socket_stream::in, streams.byte::in, io::di, io::uo)
    is det.

put_byte(socket_stream(Socket), byte(Byte), !IO) :-
    Bitmap = init(bits_per_byte) ^ byte(0) := Byte,
    write(Socket, Bitmap, Result, !IO),
    (
        Result = ok
    ;
        Result = error(Error),
        throw(streams.error(Error))
    ).

:- instance writer(socket_stream, string, io) where [
        pred(put/4) is put_string
    ].

:- pred put_string(Stream::in, string::in, State::di, State::uo) is det
    <= writer(Stream, streams.byte, State).

put_string(Stream, String, !State) :-
    foldl(put_char(Stream), String, !State).

:- pred put_char(Stream::in, char::in, State::di, State::uo) is det
    <= writer(Stream, streams.byte, State).

put_char(Stream, Char, !State) :-
    put(Stream, byte(to_int(Char)), !State).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
