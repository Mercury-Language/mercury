%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2007, 2011 The University of Melbourne.
% Copyright (C) 2014-2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module: sockets
% Main Author:          pro@missioncriticalit.com
%                       (based on code written by pma@missioncriticalit.com)
% Largely rewritten by: Paul Bone
% Stability:            low
%
% Provide a low-level interface to sockets.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module net.sockets.
:- interface.

:- import_module bitmap.
:- import_module io.
:- import_module maybe.

:- import_module net.types.

%-----------------------------------------------------------------------------%

:- type socket.

%-----------------------------------------------------------------------------%

    % socket(Domain, Type, Protocol, Result, !IO):
    %
    % Create a new socket.
    %
:- pred socket(family::in, socktype::in, protocol_num::in,
    maybe_error(socket)::out, io::di, io::uo) is det.

    % socket(Domain, Type, Result, !IO):
    %
    % Create a new socket, use this variant to have the sockets library
    % detect the correct protocol (usually the only protocol).
    %
:- pred socket(family::in, socktype::in,
    maybe_error(socket)::out, io::di, io::uo) is det.

    % connect(Socket, Addr, Addrlen, Result, !IO):
    %
:- pred connect(socket::in, sockaddr::in, maybe_error::out,
    io::di, io::uo) is det.

    % bind(Socket, Addr, Result, !IO):
    %
:- pred bind(socket::in, sockaddr::in, maybe_error::out,
    io::di, io::uo) is det.

    % listen(Socket, Backlog, Result, !IO):
    %
:- pred listen(socket::in, int::in, maybe_error::out, io::di, io::uo)
    is det.

:- type accept_result
    --->    accept_result(
                ar_socket       :: socket,
                ar_address      :: sockaddr
            ).

    % accept(Socket, Addr, Result, !IO):
    %
    % Accept will block until a connection to our socket is made.
    %
:- pred accept(socket::in, maybe_error(accept_result)::out,
    io::di, io::uo) is det.

    % close(Socket, Result, !IO):
    %
    % This closes the socket with lingering enabled.  The call will not
    % return until all the queued data has been sent or the timeout expires
    % (2 seconds).
    %
:- pred close(socket::in, maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type read_result(T)
    --->    ok(T)
    ;       eof
    ;       error(string).

    % The returned buffer may be smaller than the amount of requested data
    % if either 1) the end of file/stream was reached or 2) a smaller amount
    % of data is available.  If the OS has no data then this call will
    % block.
    %
:- pred read(socket::in, int::in, sockets.read_result(bitmap)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred write(socket::in, bitmap::in, maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module require.

:- import_module net.errno.

:- pragma foreign_decl("C",
"
#ifdef MR_WIN32
    #include ""mercury_windows.h""
    #include <winsock2.h>
    #include <ws2tcpip.h>
#else
    #include <errno.h>
    #include <netdb.h>
    #include <netinet/in.h>
    #include <sys/types.h>
    #include <sys/socket.h>
#endif
").

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", socket, "MR_Integer", [can_pass_as_mercury_type]).

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", local,
"
#ifdef MR_WIN32
  #define  error()      WSAGetLastError()
  #define  SHUT_RDWR    SD_BOTH
#else /* !MR_WIN32 */
  #define  error()      errno

  #define  INVALID_SOCKET   -1
#endif /* !MR_WIN32 */
").

%-----------------------------------------------------------------------------%

:- initialise init/2.

:- pred init(io::di, io::uo) is det.

:- pragma foreign_proc(c,
    init(_IO0::di, _IO::uo),
    [will_not_call_mercury, thread_safe, promise_pure, tabled_for_io],
"
#ifdef MR_WIN32
    WORD    wVersionRequested;
    WSADATA wsaData;
    int err;

    wVersionRequested = MAKEWORD( 2, 2 );
    err = WSAStartup(wVersionRequested, &wsaData);

    if ( err != 0 ) {
        MR_fatal_error(""Unable to find a usable winsock.dll\\n"");
    }

    if ( LOBYTE( wsaData.wVersion ) != 2 ||
            HIBYTE( wsaData.wVersion ) != 2 ) {
        WSACleanup();
        MR_fatal_error(""Unable to find a usable winsock.dll\\n"");
    }
#endif /* MR_WIN32 */
").

%-----------------------------------------------------------------------------%

socket(Domain, Type, MaybeSocket, !IO) :-
    socket(Domain, Type, 0, MaybeSocket, !IO).

socket(Domain, Type, Protocol, MaybeSocket, !IO) :-
    socket(Domain, Type, Protocol, Socket, Success, Errno, !IO),
    (
        Success = yes,
        MaybeSocket = ok(Socket)
    ;
        Success = no,
        MaybeSocket = error(strerror(Errno))
    ).

:- pred socket(family::in, socktype::in, protocol_num::in,
    socket::out, bool::out, int::out, io::di, io::uo) is det.

:- pragma foreign_proc(c,
    socket(Domain::in, Type::in, Protocol::in, Socket::out, Success::out,
        Errno::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    Socket = socket(Domain, Type, Protocol);
    if (Socket == INVALID_SOCKET) {
        Errno = error();
        Success = MR_NO;
    } else {
        Success = MR_YES;
    }
").

%-----------------------------------------------------------------------------%

connect(Socket, Addr, Result, !IO) :-
    connect(Socket, Addr, Success, Errno, !IO),
    (
        Success = yes,
        Result = ok
    ;
        Success = no,
        Result = error(strerror(Errno))
    ).

:- pred connect(socket::in, sockaddr::in, bool::out, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    connect(Socket::in, Addr::in, Success::out, Errno::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    if (connect(Socket, &(Addr->raw), sock_addr_size(Addr)) == INVALID_SOCKET) {
        Errno = error();
        Success = MR_NO;
    } else {
        Success = MR_YES;
    }
").

%-----------------------------------------------------------------------------%

bind(Socket, Addr, Result, !IO) :-
    bind(Socket, Addr, Success, Errno, !IO),
    (
        Success = yes,
        Result = ok
    ;
        Success = no,
        Result = error(strerror(Errno))
    ).

:- pred bind(socket::in, sockaddr::in, bool::out, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bind(Socket::in, Addr::in, Success::out, Errno::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    if (bind(Socket, &(Addr->raw), sock_addr_size(Addr)) == INVALID_SOCKET) {
        Errno = error();
        Success = MR_NO;
    } else {
        Success = MR_YES;
    }
").

%-----------------------------------------------------------------------------%

listen(Socket, Backlog, Result, !IO) :-
    listen(Socket, Backlog, Success, Errno, !IO),
    (
        Success = yes,
        Result = ok
    ;
        Success = no,
        Result = error(strerror(Errno))
    ).

:- pred listen(socket::in, int::in, bool::out, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    listen(Socket::in, BackLog::in, Success::out, Errno::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    if (listen(Socket, BackLog) == INVALID_SOCKET) {
        Errno = error();
        Success = MR_NO;
    } else {
        Success = MR_YES;
    }
").

%-----------------------------------------------------------------------------%

accept(Socket, Result, !IO) :-
    accept(Socket, NewSocket, Addr, Success, AddressOk, Errno, !IO),
    (
        Success = yes,
        (
            AddressOk = yes,
            Result = ok(accept_result(NewSocket, Addr))
        ;
            AddressOk = no,
            close(NewSocket, _, !IO),
            Result = error("Could not decode peer address")
        )
    ;
        Success = no,
        Result = error(strerror(Errno))
    ).

:- pred accept(socket::in, socket::out, sockaddr::out, bool::out, bool::out,
    int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    accept(Socket::in, NewSocket::out, Addr::out, Success::out,
        AddressOk::out, Errno::out, _IO0::di, _IO::uo),
    [thread_safe, will_not_call_mercury, promise_pure, tabled_for_io],
"
    socklen_t addrlen;

    Addr = MR_GC_NEW(union my_sockaddr);
    addrlen = sizeof(union my_sockaddr);
    NewSocket = accept(Socket, &(Addr->raw), &addrlen);
    if (NewSocket == INVALID_SOCKET) {
        Errno = error();
        Success = MR_NO;
        AddressOk = MR_NO;
    } else if (addrlen > sizeof(union my_sockaddr)){
        Success = MR_YES;
        AddressOk = MR_NO;
    } else {
        Success = MR_YES;
        AddressOk = MR_YES;
    }
").

%-----------------------------------------------------------------------------%

close(Socket, Result, !IO) :-
    close(Socket, Success, Errno, !IO),
    (
        Success = yes,
        Result = ok
    ;
        Success = no,
        Result = error(strerror(Errno))
    ).

:- pred close(socket::in, bool::out, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    close(Socket::in, Success::out, Errno::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    struct linger sockets_linger = { MR_TRUE, 2 };
    setsockopt(Socket, SOL_SOCKET, SO_LINGER,
        (char*)&sockets_linger, sizeof(sockets_linger));
    if (-1 == shutdown(Socket, SHUT_RDWR)) {
        Errno = error();
        Success = MR_NO;
    } else {
        Success = MR_YES;
    }
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

read(Socket, Len0, Result, !IO) :-
    read(Socket, Len0, Bitmap0, BytesRead, Errno, !IO),
    ( if BytesRead > 0 then
        Bitmap = shrink_without_copying(Bitmap0, BytesRead*8),
        Result = ok(Bitmap)
    else if BytesRead = 0 then
        Result = eof
    else
        Result = error(strerror(Errno))
    ).

:- pred read(socket::in, int::in, bitmap::bitmap_uo, int::out, errno::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    read(Socket::in, Len::in, Bitmap::bitmap_uo, BytesRead::out, Errno::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
    "
        MR_allocate_bitmap_msg(Bitmap, Len*8, MR_ALLOC_ID);
        BytesRead = recv(Socket, Bitmap->elements, Len, 0);
        if (BytesRead == -1) {
            Errno = error();
        }
    ").

%-----------------------------------------------------------------------------%

write(Socket, Bitmap, Result, !IO) :-
    write(Socket, Bitmap, 0, Result, !IO).

:- pred write(socket::in, bitmap::in, int::in, maybe_error::out,
    io::di, io::uo) is det.

write(Socket, Bitmap, Offset, Result, !IO) :-
    ( if LenPrime = num_bytes(Bitmap) - Offset then
        Len = LenPrime
    else
        unexpected($file, $pred,
            "Bitmap must have an integral number of bytes")
    ),
    write_c(Socket, Bitmap, Offset, Len, BytesWritten, Errno, !IO),
    ( if BytesWritten = Len then
        Result = ok
    else if BytesWritten = -1 then
        Result = error(strerror(Errno))
    else if BytesWritten < Len then
        % Not all the bytes were written.  Try again.
        write(Socket, Bitmap, Offset + BytesWritten, Result, !IO)
    else
        unexpected($file, $pred, "BytesWritten > Len")
    ).

:- pred write_c(socket::in, bitmap::in, int::in, int::in, int::out,
    errno::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    write_c(Socket::in, Bitmap::in, Offset::in, Len::in,
        BytesWritten::out, Errno::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
    "
        BytesWritten = send(Socket, &Bitmap->elements[Offset], Len, 0);
        if (BytesWritten == -1) {
            Errno = error();
        }
    ").

%-----------------------------------------------------------------------------%
:- end_module sockets.
%-----------------------------------------------------------------------------%
