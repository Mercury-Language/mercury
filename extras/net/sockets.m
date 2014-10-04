%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2007, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB
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

:- import_module io.
:- import_module maybe.

:- import_module net.netdb.
:- import_module net.types.

%-----------------------------------------------------------------------------%

    % The socket family.  This type is incomplete, support for socket
    % families such as IPX or appletalk will probably never be added.
    % However Unix domain sockets may be added in the future.
    %
:- type family
    --->    fam_inet
    ;       fam_inet6.

    % The socket type.  Informally (for fam_inet and fam_inet6) these
    % correspond to TCP and UDP respectively.  More precicely these specify
    % the socket's behavour, the protocol is optionally specified
    % seperately.
    %
:- type socktype
    --->    sock_stream
    ;       sock_dgram.

:- type socket.

%-----------------------------------------------------------------------------%

    % socket(Domain, Type, Protocol, Result, !IO),
    %
    % Create a new socket.
    %
:- pred socket(family::in, socktype::in, protocol_num::in,
    maybe_error(socket)::out, io::di, io::uo) is det.

    % socket(Domain, Type, Result, !IO),
    %
    % Create a new socket, use this variant to have the sockets library
    % detect the correct protocal (usually the only protocol).
    %
:- pred socket(family::in, socktype::in,
    maybe_error(socket)::out, io::di, io::uo) is det.

    % connect(Socket, Addr, Addrlen, Result, !IO),
    %
:- pred connect(socket::in, sockaddr::in, maybe_error::out,
    io::di, io::uo) is det.

    % bind(Socket, Addr, Result, !IO),
    %
:- pred bind(socket::in, sockaddr::in, maybe_error::out,
    io::di, io::uo) is det.

    % listen(Socket, Backlog, Result, !IO),
    %
:- pred listen(socket::in, int::in, maybe_error::out, io::di, io::uo)
    is det.

:- type accept_result
    --->    accept_result(
                ar_socket       :: socket,
                ar_address      :: sockaddr
            ).

    % accept(Socket, Addr, Result, !IO),
    %
    % Accept will block until a connection to our socket is made.
    %
:- pred accept(socket::in, maybe_error(accept_result)::out,
    io::di, io::uo) is det.

    % close(Socket, Result, !IO),
    %
    % This closes the socket with lingering enabled.  The call will not
    % return until all the queued data has been sent or he timeout expires
    % (2 seconds).
    %
:- pred close(socket::in, maybe_error::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.

:- import_module net.errno.

:- pragma foreign_decl("C",
"
#ifdef MR_WIN32
  #include <winsock.h>
#else
  #include <errno.h>
  #include <netdb.h>
  #include <netinet/in.h>
  #include <sys/types.h>
  #include <sys/socket.h>
#endif
").

%-----------------------------------------------------------------------------%

    % This list of address families is from socket(2) on linux.
    %
:- pragma foreign_enum("C", family/0,
    [fam_inet       - "AF_INET",
     fam_inet6      - "AF_INET6"]).
%     fam_unix       - "AF_UNIX",
%     fam_ipx        - "AF_IPX",
%     fam_netlink    - "AF_NETLINK",
%     fam_x25        - "AF_X25",
%     fam_ax25       - "AF_AX25",
%     fam_atmpvc     - "AF_ATMPVC",
%     fam_appletalk  - "AF_APPLETALK",
%     fam_packet     - "AF_PACKET",

:- pragma foreign_enum("C", socktype/0,
    [sock_stream    - "SOCK_STREAM",
     sock_dgram     - "SOCK_DGRAM"]).
% See socket(2) for the meaning of these values.
%     sock_seqpacket - "SOCK_SEQPACKET",
%     sock_raw       - "SOCK_RAW",
%     sock_rdm       - "SOCK_RDM",
    % Note: sock_packet is obosolete.
    % Note: We deleberately do not support the non-portable SOCK_NONBLOCK
    % and SOCK_CLOEXEC values, this functionality should be accessed via
    % setsocketopt.

:- pragma foreign_type("C", socket, "MR_Integer", [can_pass_as_mercury_type]).

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", local,
"
#ifdef MR_WIN32
  #define  error()      WSAGetLastError()

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
        &sockets_linger, sizeof(sockets_linger));
    if (-1 == close(Socket)) {
        Errno = error();
        Success = MR_NO;
    } else {
        Success = MR_YES;
    }
").

%-----------------------------------------------------------------------------%
:- end_module sockets.
%-----------------------------------------------------------------------------%
