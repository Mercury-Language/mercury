%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2015, 2018, 2021, 2023 The Mercury Team
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module: netdb
% Main Author:  Paul Bone <paul@bone.id.au>
% Stability:    low
%
% Provide an interface to the POSIX C interface for network lookups.
%
% This interface uses the more modern getaddrinfo(2) interface rather than
% the old and not-thread-safe gethostbyname(2) interface.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module net.netdb.
:- interface.

:- import_module io.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module net.types.

%-----------------------------------------------------------------------------%

:- type protocol
    --->    protocol(
                p_name          :: string,
                p_aliases       :: list(string),
                p_num           :: protocol_num
            ).

    % Lookup a protocol entry by name.
    %
:- pred getprotobyname(string::in, maybe(protocol)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type service
    --->    numeric_service(int)
    ;       string_service(string).

%-----------------------------------------------------------------------------%

:- type lookup_result
    --->    lookup_result(
                hasr_family     :: family,
                hasr_socktype   :: socktype,
                hasr_protocol   :: protocol_num,
                hasr_sockaddr   :: sockaddr
            ).

:- pred lookup_host_and_service(string::in, service::in, maybe(family)::in,
    maybe(socktype)::in, maybe_error(list(lookup_result))::out)
    is det.

:- pred lookup_local_socket(service::in, maybe(family)::in,
    maybe(socktype)::in, maybe_error(list(lookup_result))::out) is det.

%:- pred gethostbyname(string::in, res(hostent)::out,
%    io::di, io::uo) is det.

%:- pred getservbyname(string::in, string::in, int::out,
%    io::di, io::uo) is det.

    % port_address(Host, Port, Result, !IO),
    %
    % Lookup a hostname and build an address structure with the resulting
    % address and the given port.
    %
:- pragma obsolete(pred(port_address/5)).
:- pred port_address(string::in, int::in, maybe_error(c_pointer)::out,
    io::di, io::uo) is det.

    % service_address(Host, Service, Result, !IO),
    %
:- pragma obsolete(pred(service_address/5)).
:- pred service_address(string::in, string::in,
    maybe_error(c_pointer)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module require.

:- import_module net.getaddrinfo.
:- import_module net.errno.

:- pragma foreign_decl("C",
"
#ifdef MR_WIN32
    #include ""mercury_windows.h""
    #include <winsock2.h>
    #include <ws2tcpip.h>
#else
    #include <netdb.h>
#endif
").

:- pragma foreign_decl("C", local,
"
#ifdef MR_WIN32
  #define  error()      WSAGetLastError()
#else
  #define  error()      errno
#endif

#if defined(MR_THREAD_SAFE) && !defined(__GNU_LIBRARY__)
  static MercuryLock    lookup_lock = PTHREAD_MUTEX_INITIALIZER;
#endif
").

%-----------------------------------------------------------------------------%

getprotobyname(Name, MaybeProtocol, !IO) :-
    getprotobyname_c(buffer_size, Name, CProtocol, Success, Found, !IO),
    (
        Success = yes,
        (
            Found = yes,
            c_protocol_to_protocol(CProtocol, Protocol),
            MaybeProtocol = yes(Protocol)
        ;
            Found = no,
            MaybeProtocol = no
        )
    ;
        Success = no,
        % A buffer size of 1024 bytes was insufficient for a protocol name,
        % protocols are usually 3 letters long like "tcp".
        unexpected($file, $pred, "Buffer too small")
    ).

:- pred getprotobyname_c(int::in, string::in, protocol_c::out, bool::out,
    bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    getprotobyname_c(BufferSize::in, Name::in, Protocol::out, Success::out,
        Found::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
/*
** getprotobyname_r is a GNU extension.
*/
#if defined(__GNU_LIBRARY__)

    int result;
    struct protoent *temp = MR_GC_NEW(struct protoent);
    char *buffer = MR_GC_malloc_atomic(BufferSize);

    result = getprotobyname_r(Name, temp, buffer, BufferSize, &Protocol);
    Success = result == 0 ? MR_YES : MR_NO;
    Found = Protocol != NULL ? MR_YES : MR_NO;

#else
    struct protoent *temp;
    int             num_aliases;
    int             i;

    #ifdef MR_THREAD_SAFE
      MR_LOCK(&lookup_lock, ""getprotobyname_r"");
    #endif

    temp = getprotobyname(Name);
    if (temp != NULL) {
        Protocol = MR_GC_NEW(struct protoent);
        MR_make_aligned_string_copy(Protocol->p_name, temp->p_name);
        for (num_aliases = 0; temp->p_aliases[num_aliases]; num_aliases++);
        Protocol->p_aliases = MR_GC_NEW_ARRAY(char*, num_aliases);
        for (i = 0; i < num_aliases; i++) {
            MR_make_aligned_string_copy(Protocol->p_aliases[i],
                temp->p_aliases[i]);
        }
        Protocol->p_proto = temp->p_proto;
        Found = MR_YES;
    } else {
        Found = MR_NO;
    }
    Success = MR_YES;

    #ifdef MR_THREAD_SAFE
      MR_UNLOCK(&lookup_lock, ""getprotobyname_r"");
    #endif

#endif /* ! __GNU_LIBRARY__ */
").

%-----------------------------------------------------------------------------%

:- type protocol_c.
:- pragma foreign_type("C", protocol_c, "struct protoent*",
    [can_pass_as_mercury_type]).

:- pred c_protocol_to_protocol(protocol_c::in, protocol::uo) is det.

c_protocol_to_protocol(CProto, Proto) :-
    c_protocol_get_name(CProto, OfficialName),
    c_protocol_get_aliases(CProto, Aliases),
    c_protocol_get_number(CProto, Number),
    Proto = protocol(OfficialName, Aliases, Number).

:- pred c_protocol_get_name(protocol_c::in, string::uo) is det.

:- pragma foreign_proc("C",
    c_protocol_get_name(Proto::in, Name::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
    "MR_make_aligned_string_copy_saved_hp(Name, Proto->p_name, NULL);").

:- pred c_protocol_get_aliases(protocol_c::in, list(string)::uo) is det.

:- pragma foreign_proc("C",
    c_protocol_get_aliases(Proto::in, List::uo),
    [may_call_mercury, promise_pure, thread_safe],
"
    int i = 0;

    List = MR_list_empty();

    while (Proto->p_aliases[i] != NULL) {
        MR_String str;
        MR_make_aligned_string_copy_saved_hp(str, Proto->p_aliases[i], NULL);
        List = MR_list_cons((MR_Word)str, List);
        i++;
    }
").

:- pred c_protocol_get_number(protocol_c::in, int::uo) is det.

:- pragma foreign_proc("C",
    c_protocol_get_number(Proto::in, Number::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Number = Proto->p_proto;").

%-----------------------------------------------------------------------------%

lookup_host_and_service(Host, Service, MaybeFamily, MaybeSocktype,
        MaybeResults) :-
    getaddrinfo(node_and_service(Host, Service), gai_flag_addrconfig,
        MaybeFamily, MaybeSocktype, no, MaybeResults0),
    (
        MaybeResults0 = ok(Results0),
        map(make_host_and_service_result, Results0, Results),
        MaybeResults = ok(Results)
    ;
        MaybeResults0 = error(Error),
        MaybeResults = error(Error)
    ).

lookup_local_socket(Service, MaybeFamily, MaybeSocktype, MaybeResults) :-
    getaddrinfo(service_only(Service),
        gai_flag_addrconfig \/ gai_flag_passive, MaybeFamily, MaybeSocktype,
        no, MaybeResults0),
    map_maybe_error(map(make_host_and_service_result),
        MaybeResults0, MaybeResults).

:- pred make_host_and_service_result(addrinfo::in,
    lookup_result::out) is det.

make_host_and_service_result(AI, lookup_result(Family, SockType,
        ProtocolNum, Sockaddr)) :-
    Family = AI ^ ai_family,
    MaybeSockType = AI ^ ai_socktype,
    (
        MaybeSockType = yes(SockType)
    ;
        MaybeSockType = no,
        unexpected($file, $pred, "No socktype")
    ),
    ProtocolNum = AI ^ ai_protocol,
    Sockaddr = AI ^ ai_sockaddr.

%-----------------------------------------------------------------------------%

:- pred map_maybe_error(pred(T, U), maybe_error(T, E), maybe_error(U, E)).
:- mode map_maybe_error(pred(in, out) is det, in, out) is det.

map_maybe_error(P, ok(X), ok(Y)) :-
    P(X, Y).
map_maybe_error(_, error(E), error(E)).

%-----------------------------------------------------------------------------%

%gethostbyname(Name, Result, !IO) :-
%    gethostbyname_c(Name, Hostent, Success, Error, !IO),
%    (
%        Success = yes,
%        Result = ok(Hostent)
%    ;
%        Success = no,
%        Result = error(Error)
%    ).
%
%:- pred gethostbyname(string::in, hostent::out, bool::out, string::out,
%    io::di, io::uo) is det.
%
%:- pragma foreign_proc(c,
%    gethostbyname_c(Name::in, Host::out, _IO0::di, _IO::uo),
%     [will_not_call_mercury, promise_pure, tabled_for_io],
%"
%    /*
%     * Not thread safe.
%     */
%    struct hostent  *host;
%    Host = gethostbyname(Name);
%    Host = (MR_String) host->h_name;
%").
%
%%-----------------------------------------------------------------------------%
%
%:- pragma foreign_proc(c,
%    getservbyname(Name::in, Protocol::in, Port::out, _IO0::di, _IO::uo),
%     [will_not_call_mercury, promise_pure, tabled_for_io],
%"
%    struct servent *service;
%    service = getservbyname(Name, Protocol);
%    if (service != NULL) {
%        Port = (MR_Integer) ntohs(service->s_port);
%    } else {
%        Port = -1;
%    }
%").

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

    % The initial length of buffers for strings (suggested by
    % getprotobyname_r(3).
    %
:- func buffer_size = int.

buffer_size = 1024.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
