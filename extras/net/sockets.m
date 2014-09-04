%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2007, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB
%-----------------------------------------------------------------------------%
%
% Module: sockets
% Main Author:  pro@missioncriticalit.com
%               (based on code written by pma@missioncriticalit.com)
% Stability:    low
%
% Provide a low-level interface to sockets.
% The more declarative interface is provided by the module tcp.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module sockets.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- type res(T)
    --->    ok(T)
    ;       error(string).

:- type res
    --->    ok
    ;       error(string).

%-----------------------------------------------------------------------------%

    % gethostbyname(Hostname, RealName, !IO),
    %
    % Note, this does not return the address of the host, it returns the
    % "official name of the host" gethostbyname(3), this may be a mistake.
    %
:- pred gethostbyname(string::in, string::out, io::di, io::uo) is det.

    % getservbyname(ServiceName, Protocol, PortNum, !IO),
    %
    % Lookup the port number for a service name (eg "http") and a protocol
    % (eg "tcp").  If the service was not found then -1 is returned.
    %
:- pred getservbyname(string::in, string::in, int::out, io::di, io::uo) is det.

    % socket(Domain, Type, Protocol, Result, !IO),
    %
    % Create a new socket.
    %
:- pred socket(int::in, int::in, int::in, sockets.res(int)::out,
    io::di, io::uo) is det.

    % port_address(Host, Port, Result, !IO),
    %
    % Lookup a hostname and build an address structure with the resulting
    % address and the given port.
    %
:- pred port_address(string::in, int::in, sockets.res(c_pointer)::out,
    io::di, io::uo) is det.

    % service_address(Host, Service, Result, !IO),
    %
:- pred service_address(string::in, string::in,
    sockets.res(c_pointer)::out, io::di, io::uo) is det.

    % connect(Fd, Addr, Addrlen, Result, !IO),
    %
    % XXX: Where does the caller get the Addrlen parameter from?
    %
:- pred connect(int::in, c_pointer::in, int::in, sockets.res::out,
    io::di, io::uo) is det.

    % bind(Fd, Addr, Addrlen, Result, !IO),
    %
:- pred bind(int::in, c_pointer::in, int::in, sockets.res::out,
    io::di, io::uo) is det.

    % listen(Fd, Backlog, Result, !IO),
    %
:- pred listen(int::in, int::in, sockets.res::out, io::di, io::uo)
    is det.

    % accept(Fd, Addr, Result, !IO),
    %
    % Accept will block until a connection to our socket is made.
    %
:- pred accept(int::in, c_pointer::in, sockets.res(int)::out,
    io::di, io::uo) is det.

    % close(Fd, Result, !IO),
    %
    % This closes the socket with lingering enabled.  The call will not
    % return until all the queued data has been sent or he timeout expires
    % (2 seconds).
    %
:- pred close(int::in, sockets.res::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.

:- pragma foreign_decl("C", "
#ifdef MR_WIN32
  #include <winsock.h>

  #define  error()      WSAGetLastError()

#else /* !MR_WIN32 */

  #include <errno.h>
  #include <netdb.h>

  #include <netinet/in.h>

  #include <sys/types.h>
  #include <sys/socket.h>

  #define  error()      errno

  #define  INVALID_SOCKET   -1
#endif /* !MR_WIN32 */

  #include \"mercury_string.h\"

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

    % XXX not thread safe.
:- pragma foreign_proc(c,
    gethostbyname(Name::in, Host::out, _IO0::di, _IO::uo),
     [will_not_call_mercury, promise_pure, tabled_for_io],
"
    struct hostent  *host;
    host = gethostbyname(Name);
    Host = (MR_String) host->h_name;
").

    % XXX not thread safe.
:- pragma foreign_proc(c,
    getservbyname(Name::in, Protocol::in, Port::out, _IO0::di, _IO::uo),
     [will_not_call_mercury, promise_pure, tabled_for_io],
"
    struct servent *service;
    service = getservbyname(Name, Protocol);
    if (service != NULL) {
        Port = (MR_Integer) ntohs(service->s_port);
    } else {
        Port = -1;
    }
").

%-----------------------------------------------------------------------------%

socket(Domain, Type, Protocol, MaybeSocket, !IO) :-
    socket(Domain, Type, Protocol, Socket, Success, Errno, !IO),
    (
        Success = yes,
        MaybeSocket = ok(Socket)
    ;
        Success = no,
        MaybeSocket = error(strerror(Errno))
    ).

:- pred socket(int::in, int::in, int::in, int::out, bool::out,
    int::out, io::di, io::uo) is det.

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

port_address(Host, Port, MaybeSA, !IO) :-
    port_address(Host, Port, SA, Success, Errno, !IO),
    (
        Success = yes,
        MaybeSA = ok(SA)
    ;
        Success = no,
        MaybeSA = error(strerror(Errno))
    ).

:- pred port_address(string::in, int::in, c_pointer::out,  bool::out,
    int::out, io::di, io::uo) is det.

    % XXX Not thread safe as this uses gethostbyname
    %
:- pragma foreign_proc("C",
    port_address(Host::in, Port::in, SA::out, Success::out, Errno::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    struct hostent *host;
    struct sockaddr_in *addr;

    host = gethostbyname(Host);
    if (host == NULL) {
        Errno = error();
        Success = MR_NO;
    } else {
        addr = MR_GC_NEW(struct sockaddr_in);

        MR_memcpy(&(addr->sin_addr), host->h_addr_list[0], host->h_length);
        addr->sin_family = host->h_addrtype;
        addr->sin_port = htons(Port);

        SA = (MR_Word) addr;
        Success = MR_YES;
    }
").

%-----------------------------------------------------------------------------%

service_address(Service, Host, MaybeSA, !IO) :-
    service_address(Service, Host, SA, Success, Errno, !IO),
    (
        Success = yes,
        MaybeSA = ok(SA)
    ;
        Success = no,
        MaybeSA = error(strerror(Errno))
    ).

:- pred service_address(string::in, string::in, c_pointer::out,
    bool::out, int::out, io::di, io::uo) is det.

    % XXX Not thread safe as this uses gethostbyname and getservbyname.
    %
:- pragma foreign_proc("C",
    service_address(Service::in, Host::in, SA::out, Success::out,
        Errno::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    struct hostent *host;
    struct servent *service;
    struct sockaddr_in *addr;

    host = gethostbyname(Host);
    if (host == NULL) {
        Errno = error();
        Success = MR_NO;
    } else {
        service = getservbyname(Service,""tcp"");

        if (service == NULL) {
            Errno = error();
            Success = MR_NO;
        } else {
            addr = MR_GC_NEW(struct sockaddr_in);
            MR_memcpy(&(addr->sin_addr), host->h_addr_list[0], host->h_length);
            addr->sin_family = host->h_addrtype;
            addr->sin_port = service->s_port;
            SA = (MR_Word) addr;
            Success = MR_YES;
        }
    }
").

%-----------------------------------------------------------------------------%

connect(Fd, Addr, AddrLen, Result, !IO) :-
    connect(Fd, Addr, AddrLen, Success, Errno, !IO),
    (
        Success = yes,
        Result = ok
    ;
        Success = no,
        Result = error(strerror(Errno))
    ).

:- pred connect(int::in, c_pointer::in, int::in, bool::out,
    int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    connect(Fd::in, Addr::in, AddrLen::in, Success::out, Errno::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    struct sockaddr *addr = (struct sockaddr *) Addr;
    if (connect(Fd, addr, AddrLen) == INVALID_SOCKET) {
        Errno = error();
        Success = MR_NO;
    } else {
        Success = MR_YES;
    }
").

%-----------------------------------------------------------------------------%

bind(Fd, Addr, AddrLen, Result, !IO) :-
    bind(Fd, Addr, AddrLen, Success, Errno, !IO),
    (
        Success = yes,
        Result = ok
    ;
        Success = no,
        Result = error(strerror(Errno))
    ).

:- pred bind(int::in, c_pointer::in, int::in, bool::out, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    bind(Fd::in, Addr::in, AddrLen::in, Success::out, Errno::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    struct sockaddr *addr = (struct sockaddr *) Addr;
    if (bind(Fd, addr, AddrLen) == INVALID_SOCKET) {
        Errno = error();
        Success = MR_NO;
    } else {
        Success = MR_YES;
    }
").

%-----------------------------------------------------------------------------%

listen(Fd, Backlog, Result, !IO) :-
    listen(Fd, Backlog, Success, Errno, !IO),
    (
        Success = yes,
        Result = ok
    ;
        Success = no,
        Result = error(strerror(Errno))
    ).

:- pred listen(int::in, int::in, bool::out, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    listen(Fd::in, BackLog::in, Success::out, Errno::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    if (listen(Fd, BackLog) == INVALID_SOCKET) {
        Errno = error();
        Success = MR_NO;
    } else {
        Success = MR_YES;
    }
").

%-----------------------------------------------------------------------------%

accept(Fd, Addr, MaybeNewSocket, !IO) :-
    accept(Fd, Addr, NewSocket, Success, Errno, !IO),
    (
        Success = yes,
        MaybeNewSocket = ok(NewSocket)
    ;
        Success = no,
        MaybeNewSocket = error(strerror(Errno))
    ).

:- pred accept(int::in, c_pointer::in, int::out, bool::out, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    accept(Fd::in, Addr::in, NewSocket::out, Success::out, Errno::out,
        _IO0::di, _IO::uo),
    [thread_safe, will_not_call_mercury, promise_pure, tabled_for_io],
"
    struct sockaddr *addr = (struct sockaddr *) Addr;
    NewSocket = accept(Fd, addr, NULL);
    if (NewSocket == INVALID_SOCKET) {
        Errno = error();
        Success = MR_NO;
    } else {
        Success = MR_YES;
    }
").

%-----------------------------------------------------------------------------%

close(Fd, Result, !IO) :-
    close(Fd, Success, Errno, !IO),
    (
        Success = yes,
        Result = ok
    ;
        Success = no,
        Result = error(strerror(Errno))
    ).

:- pred close(int::in, bool::out, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    close(Fd::in, Success::out, Errno::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    struct linger sockets_linger = { MR_TRUE, 2 };
    setsockopt(Fd, SOL_SOCKET, SO_LINGER,
        &sockets_linger, sizeof(sockets_linger));
    if (-1 == shutdown(Fd, SHUT_RDWR)) {
        Errno = error();
        Success = MR_NO;
    } else {
        Success = MR_YES;
    }
").

%-----------------------------------------------------------------------------%

    % Errno handling.
    %
:- func strerror(int) = string.

strerror(Errno) = String :-
    strerror(Errno, String).

:- pred strerror(int::in, string::uo) is det.

:- pragma foreign_proc("C",
    strerror(Errno::in, Str::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    char errbuf[MR_STRERROR_BUF_SIZE];

    MR_make_aligned_string_copy(Str,
        MR_strerror(Errno, errbuf, sizeof(errbuf)));
").

%-----------------------------------------------------------------------------%
:- end_module sockets.
%-----------------------------------------------------------------------------%
