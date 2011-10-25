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

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

:- type sockets.res(Type)
    --->    ok(Type)
    ;       error(string).

:- pred sockets.gethostbyname(string::in, string::out,
    io::di, io::uo) is det.

:- pred sockets.getservbyname(string::in, string::in, int::out,
    io::di, io::uo) is det.

:- pred sockets.socket(int::in, int::in, int::in, int::out, bool::out,
    io::di, io::uo) is det.

:- pred sockets.port_address(string::in, int::in, c_pointer::out, bool::out,
    io::di, io::uo) is det.

:- pred sockets.service_address(string::in, string::in, c_pointer::out, 
    bool::out, io::di, io::uo) is det.

:- pred sockets.connect(int::in, c_pointer::in, int::in, bool::out,
    io::di, io::uo) is det.

:- pred sockets.bind(int::in, c_pointer::in, int::in, bool::out,
    io::di, io::uo) is det.

:- pred sockets.listen(int::in, int::in, bool::out, io::di, io::uo) is det.

:- pred sockets.accept(int::in, c_pointer::in, int::out, bool::out,
    io::di, io::uo) is det.

:- pred sockets.close(int::in, io::di, io::uo) is det.

    % Why did the socket operation fail?
    %
:- pred sockets.error_message(string::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
#ifdef MR_WIN32
  #include <winsock.h>

  #define  error()      WSAGetLastError

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

  /*
  ** Save the errno into this variable if a function fails.
  */
  extern int socket_errno;

").

:- pragma foreign_code("C", "
    int socket_errno;
").

%-----------------------------------------------------------------------------%

:- initialise sockets.init/2.

:- pred sockets.init(io::di, io::uo) is det.

:- pragma foreign_proc(c,
    sockets.init(_IO0::di, _IO::uo),
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

    % XXX thread safe?
:- pragma foreign_proc(c,
    gethostbyname(Name::in, Host::out, _IO0::di, _IO::uo),
     [will_not_call_mercury, promise_pure, tabled_for_io],
"
    struct hostent  *host;
    host = gethostbyname(Name);
    Host = (MR_String) host->h_name;
").

    % XXX thread safe?
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

    % XXX thread safe?
:- pragma foreign_proc(c,
    socket(Domain::in, Type::in, Protocol::in, Socket::out, Success::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io], 
"
    Socket = socket(Domain, Type, Protocol);
    if (Socket == INVALID_SOCKET) {
        socket_errno = error();
        Success = MR_NO;
    } else {
        Success = MR_YES;
    }
").

    % XXX thread safe?
:- pragma foreign_proc("C",
    port_address(Host::in, Port::in, SA::out, Success::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    struct hostent *host;
    struct sockaddr_in *addr;

    host = gethostbyname(Host);
    if (host == NULL) {
        socket_errno = error();
        Success = MR_NO;
    } else {
        addr = MR_GC_NEW(struct sockaddr_in);

        MR_memcpy(&(addr->sin_addr), host->h_addr, host->h_length);
        addr->sin_family = host->h_addrtype;
        addr->sin_port = htons(Port);

        SA = (MR_Word) addr;
        Success = MR_YES;
    }
").

    % XXX thread safe?
:- pragma foreign_proc("C",
    service_address(Service::in, Host::in, SA::out, Success::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    struct hostent *host;
    struct servent *service;
    struct sockaddr_in *addr;

    host = gethostbyname(Host);
    if (host == NULL) {
        socket_errno = error();
        Success = MR_NO;
    } else {
        service = getservbyname(Service,""tcp"");

        if (service == NULL) {
            socket_errno = error();
            Success = MR_NO;
        } else {
            addr = MR_GC_NEW(struct sockaddr_in);
            MR_memcpy(&(addr->sin_addr), host->h_addr, host->h_length);
            addr->sin_family = host->h_addrtype;
            addr->sin_port = service->s_port;
            SA = (MR_Word) addr;
            Success = MR_YES;
        }
    }
").

    % XXX thread safe?
:- pragma foreign_proc("C",
    connect(Fd::in, Addr::in, AddrLen::in, Success::out, _IO0::di, _IO::uo), 
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    struct sockaddr *addr = (struct sockaddr *) Addr;
    if (connect(Fd, addr, AddrLen) == INVALID_SOCKET) {
        socket_errno = error();
        Success = MR_NO;
    } else {
        Success = MR_YES;
    }
").

    % XXX thread safe?
:- pragma foreign_proc("C",
    bind(Fd::in, Addr::in, AddrLen::in, Success::out, _IO0::di, _IO::uo), 
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    struct sockaddr *addr = (struct sockaddr *) Addr;
    if (bind(Fd, addr, AddrLen) == INVALID_SOCKET) {
        socket_errno = error();
        Success = MR_NO;
    } else {
        Success = MR_YES;
    }
").

    % XXX thread safe?
:- pragma foreign_proc("C",
    listen(Fd::in, BackLog::in, Success::out, _IO0::di, _IO::uo), 
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    if (listen(Fd, BackLog) == INVALID_SOCKET) {
        socket_errno = error();
        Success = MR_NO;
    } else {
        Success = MR_YES;
    }
").

    % This code can block, so we make it thread_safe
    % so as to avoid other code blocking on the global mutex.
:- pragma foreign_proc("C",
    accept(Fd::in, Addr::in, NewSocket::out, Success::out,
        _IO0::di, _IO::uo), 
    [thread_safe, will_not_call_mercury, promise_pure, tabled_for_io],
"
    struct sockaddr *addr = (struct sockaddr *) Addr;
    NewSocket = accept(Fd, addr, NULL);
    if (NewSocket == INVALID_SOCKET) {
        socket_errno = error();
        Success = MR_NO;
    } else {
        Success = MR_YES;
    }
").

    % XXX thread safe?
:- pragma foreign_proc("C",
    sockets.close(Fd::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    struct linger sockets_linger = { MR_TRUE, 2 };
    setsockopt(Fd, SOL_SOCKET, SO_LINGER,
        &sockets_linger, sizeof(sockets_linger));
    shutdown(Fd, 2);
").

    % XXX thread safe?
:- pragma foreign_proc("C",
    error_message(Err::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    MR_make_aligned_string_copy(Err, strerror(socket_errno));
").

%-----------------------------------------------------------------------------%
:- end_module sockets.
%-----------------------------------------------------------------------------%
