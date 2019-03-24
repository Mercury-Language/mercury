%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2000, 2004, 2007 The University of Melbourne.
% Copyright (C) 2018-2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module posix.socket.
% Main author: conway@cs.mu.oz.au
%
%-----------------------------------------------------------------------------%

:- module posix.socket.
:- interface.

%-----------------------------------------------------------------------------%

:- type posix.socket.domain
    --->    unix
    ;       inet.

    % OBSOLETE: this was the old name for socket_type/0.
    %
:- type posix.socket.(type) == socket_type.

:- type socket_type
    --->    stream
    ;       dgram
    ;       raw
    ;       seqpacket
    ;       rdm.

:- type protocol
    --->    protocol(int).

:- type sockaddr
    --->    inet(port, inet_addr).

:- type port
    --->    port(int).

:- type inet_addr
    --->    inet_addr(int).

%-----------------------------------------------------------------------------%

:- pred socket(domain::in, socket_type::in, protocol::in,
    posix.result(fd)::out, io::di, io::uo) is det.

:- pred accept(fd::in, posix.result(fd)::out, io::di, io::uo) is det.

:- pred bind(fd::in, sockaddr::in, posix.result::out, io::di, io::uo) is det.

:- pred connect(fd::in, sockaddr::in, posix.result::out, io::di, io::uo)
    is det.

:- pred listen(fd::in, int::in, posix.result::out, io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma foreign_decl("C", "
    #include <string.h>
    #include <sys/types.h>
    #include <sys/socket.h>
    #include <netinet/in.h>
    #include <arpa/inet.h>
").

%-----------------------------------------------------------------------------%

socket(Dom, Typ, protocol(Prot), Result, !IO) :-
    socket0(Dom, Typ, Prot, FdNo, !IO),
    ( if FdNo < 0 then
        errno(Err, !IO),
        Result = error(Err)
    else
        Result = ok(fd(FdNo))
    ).

:- pred socket0(domain::in, socket_type::in, int::in, int::out, io::di, io::uo)
    is det.
:- pragma foreign_proc("C",
    socket0(Dom::in, Typ::in, Prot::in, Fd::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    Fd = socket(Dom, Typ, Prot);
").

:- pragma foreign_enum("C", domain/0, [
    unix    - "AF_UNIX",
    inet    - "AF_INET"
]).

:- pragma foreign_enum("C", socket_type/0, [
    stream      - "SOCK_STREAM",
    dgram       - "SOCK_DGRAM",
    raw         - "SOCK_RAW",
    seqpacket   - "SOCK_SEQPACKET",
    rdm         - "SOCK_RDM"
]).

%-----------------------------------------------------------------------------%

:- type sockaddr_ptr.
:- pragma foreign_type("C", sockaddr_ptr, "struct sockaddr *",
    [can_pass_as_mercury_type]).

bind(Fd, SockAddr, Result, !IO) :-
    mksockaddr_struct(SockAddr, Ptr, Len),
    bind0(Fd, Ptr, Len, Res0, !IO),
    ( if Res0 = 0 then
        Result = ok
    else
        errno(Errno, !IO),
        Result = error(Errno)
    ).

:- pred bind0(fd::in, sockaddr_ptr::in, int::in, int::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("C",
    bind0(Fd::in, Addr::in, Len::in, Res::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    Res = bind(Fd, Addr, Len);
").

:- pred mksockaddr_struct(sockaddr::in, sockaddr_ptr::out, int::out) is det.

mksockaddr_struct(inet(Port, Addr), Ptr, Len) :-
    mkinet_addr(Addr, Port, Ptr, Len).

:- pred mkinet_addr(inet_addr::in, port::in, sockaddr_ptr::out, int::out)
    is det.

:- pragma foreign_proc("C",
    mkinet_addr(A::in, P::in, Ptr::out, Len::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    MR_Word Ptr0;
    struct sockaddr_in *ptr;

    MR_incr_hp(Ptr0, (1 + sizeof(struct sockaddr_in)/sizeof(MR_Word)));
    Ptr = (struct sockaddr *) Ptr0;
    ptr = (struct sockaddr_in *) Ptr;

    MR_memset(ptr, 0, sizeof(struct sockaddr_in));
    ptr->sin_family = AF_INET;
    ptr->sin_addr.s_addr = A;
    ptr->sin_port = htons(P);

    Len = sizeof(struct sockaddr_in);
").

%-----------------------------------------------------------------------------%

connect(Fd, SockAddr, Result, !IO) :-
    mksockaddr_struct(SockAddr, Ptr, Len),
    connect0(Fd, Ptr, Len, Res, !IO),
    ( if Res = 0 then
        Result = ok
    else
        errno(Err, !IO),
        Result = error(Err)
    ).

:- pred connect0(fd::in, sockaddr_ptr::in, int::in, int::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("C",
    connect0(Fd::in, Addr::in, Len::in, Res::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    do {
        Res = connect(Fd, Addr, Len);
    } while (Res == -1 && MR_is_eintr(errno));
").

%-----------------------------------------------------------------------------%

listen(Fd, N, Result, !IO) :-
    listen0(Fd, N, Res0, !IO),
    ( if Res0 = 0 then
        Result = ok
    else
        errno(Errno, !IO),
        Result = error(Errno)
    ).

:- pred listen0(fd::in, int::in, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    listen0(Fd::in, N::in, Res::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    Res = listen(Fd, N);
").

%-----------------------------------------------------------------------------%

accept(Fd, Result, !IO) :-
    accept0(Fd, _Ptr, NewFd, !IO),
    ( if NewFd < 0 then
        errno(Errno, !IO),
        Result = error(Errno)
    else
        Result = ok(fd(NewFd))
    ).

:- pred accept0(fd::in, sockaddr_ptr::out, int::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    accept0(Fd::in, Ptr::out, NewFd::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    socklen_t addrlen;

    Ptr = (struct sockaddr *) MR_GC_NEW_ATTRIB(struct sockaddr_in, MR_ALLOC_ID);
    do {
        addrlen = sizeof(struct sockaddr_in);
        NewFd = accept(Fd, Ptr, &addrlen);
    } while (NewFd == -1 && MR_is_eintr(errno));
").

%-----------------------------------------------------------------------------%
:- end_module posix.socket.
%-----------------------------------------------------------------------------%
