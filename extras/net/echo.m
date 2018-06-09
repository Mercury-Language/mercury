%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2016, 2018 The Mercury Team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module: echo
% Main Author:  Paul Bone <paul@bone.id.au>
%
% A simple echo server.
%
% Because the sockets library can't yet connect to the io module we cannot
% yet read or write to and from sockets.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module echo.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module stream.
:- import_module string.

:- import_module net.
:- import_module net.sockets.
:- import_module net.streams.
:- import_module net.types.

%-----------------------------------------------------------------------------%

main(!IO) :-
    socket(fam_inet, sock_stream, ResSocket, !IO),
    (
        ResSocket = ok(Socket),
        bind(Socket, ipv4_sockaddr(in_addr_any, 6969), ResBind, !IO),
        (
            ResBind = ok,
            listen(Socket, 5, ResListen, !IO),
            (
                ResListen = ok,
                run(Socket, !IO)
            ;
                ResListen = error(Error),
                unexpected($file, $pred, "listen failed: " ++ Error)
            )
        ;
            ResBind = error(Error),
            unexpected($file, $pred, "bind failed: " ++ Error)
        ),
        close(Socket, ResClose, !IO),
        (
            ResClose = ok
        ;
            ResClose = error(Error),
            unexpected($file, $pred, "close failed: " ++ Error)
        )
    ;
        ResSocket = error(Error),
        unexpected($file, $pred, "create socket failed: " ++ Error)
    ).

:- pred run(socket::in, io::di, io::uo) is det.

run(Socket, !IO) :-
    accept(Socket, Result, !IO),
    (
        Result = ok(accept_result(NewSocket, Address)),
        ( ipv4_sockaddr(InAddr, Port, Address) ->
            AddrStr = format("%s:%d", [s(to_string(InAddr)), i(Port)])
        ;
            AddrStr = format("Unknown peer (family %s)",
                [s(string(family(Address)))])
        ),
        io.format("Connection from %s\n", [s(AddrStr)], !IO),
        run_connection(stream(NewSocket), AddrStr, !IO),
        close(NewSocket, CloseRes, !IO),
        (
            CloseRes = ok
        ;
            CloseRes = error(Error),
            unexpected($file, $pred, "create socket failed: " ++ Error)
        )
    ;
        Result = error(Error),
        unexpected($file, $pred, "create socket failed: " ++ Error)
    ),
    run(Socket, !IO).

:- pred run_connection(socket_stream::in, string::in, io::di, io::uo) is det.

run_connection(Stream, AddrStr, !IO) :-
    get(Stream, MaybeByte, !IO),
    (
        MaybeByte = ok(Byte `with_type` streams.byte),
        put(Stream, Byte, !IO),
        run_connection(Stream, AddrStr, !IO)
    ;
        MaybeByte = eof,
        write_string(io.stderr_stream, "EOF", !IO)
    ;
        MaybeByte = error(Error),
        io.format(io.stderr_stream, "%s; %s\n",
            [s(AddrStr), s(error_message(Error))], !IO)
    ).
