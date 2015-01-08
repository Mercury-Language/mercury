%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury Team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB
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

:- import_module net.sockets.

%-----------------------------------------------------------------------------%

:- type socket_stream.

:- func stream(socket) = socket_stream.

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

%-----------------------------------------------------------------------------%

:- instance output(socket_stream, io).

    % XXX: This does not buffer writes, it is slow.
    %
:- instance writer(socket_stream, streams.byte, io).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- implementation.

:- import_module exception.
:- import_module bitmap.
:- import_module maybe.
:- import_module require.

:- type socket_stream
    --->    socket_stream(socket).

:- type error
    --->    error(string).

stream(Socket) = socket_stream(Socket).

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
        ( num_bytes(Bitmap) = 1 ->
            Byte = Bitmap ^ byte(0),
            Result = ok(byte(Byte))
        ; num_bytes(Bitmap) = 0 ->
            Result = eof
        ;
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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
