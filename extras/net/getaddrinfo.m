%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2016, 2018 The Mercury Team
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module: getaddrinfo
% Main Author:  Paul Bone <paul@bone.id.au>
% Stability:    low
%
% Provide an interface to the getaddrinfo C function.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module net.getaddrinfo.
:- interface.

:- import_module int.
:- import_module list.
:- import_module maybe.

:- import_module net.netdb.
:- import_module net.types.

%-----------------------------------------------------------------------------%

:- type node_and_or_service
    --->    node_only(
                no_node         :: string
            )
    ;       service_only(
                so_service      :: service
            )
    ;       node_and_service(
                nas_node        :: string,
                nas_service     :: service
            ).

:- type addrinfo
    --->    addrinfo(
                ai_family       :: family,
                ai_socktype     :: maybe(socktype),
                ai_protocol     :: protocol_num,
                ai_sockaddr     :: sockaddr,
                ai_maybe_name   :: maybe(string)
            ).

%-----------------------------------------------------------------------------%

    % The address info flags bitfield.
    %
:- type gai_flags == int.

    % Return only addresses that make sense given the system's network
    % interface configuration.  For example, IPv6 addresses will only be
    % returned if at least one IPv6 interface is configured and is not the
    % loopback interface.
    %
:- func gai_flag_addrconfig = int.

    % Return addresses suitable for use with the bind() call.  Without this
    % flag returned addresses are suitable for use with the connect() call.
    %
:- func gai_flag_passive = int.

%-----------------------------------------------------------------------------%

:- pred getaddrinfo(node_and_or_service::in,
    gai_flags::in, maybe(family)::in, maybe(socktype)::in,
    maybe(protocol)::in, maybe_error(list(addrinfo))::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module string.

:- import_module net.errno.

:- pragma foreign_decl("C",
"
#ifdef MR_WIN32
    #include ""mercury_windows.h""
    #include <winsock2.h>
    #include <ws2tcpip.h>
#else
    #include <sys/types.h>
    #include <sys/socket.h>
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
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    gai_flag_addrconfig = (Flag::out),
    [will_not_call_mercury, promise_pure, thread_safe,
     will_not_throw_exception],
"
    Flag = AI_ADDRCONFIG;
").

:- pragma foreign_proc("C",
    gai_flag_passive = (Flag::out),
    [will_not_call_mercury, promise_pure, thread_safe,
     will_not_throw_exception],
"
    Flag = AI_PASSIVE;
").

:- pred flag_numericservice(gai_flags::out) is semidet.

:- pragma foreign_proc("C",
    flag_numericservice(Flag::out),
    [will_not_call_mercury, thread_safe, promise_pure,
        will_not_throw_exception],
"
    #ifdef AI_NUMERICSERV
        Flag = AI_NUMERICSERV;
        SUCCESS_INDICATOR = MR_YES;
    #else
        SUCCESS_INDICATOR = MR_NO;
    #endif
").

%-----------------------------------------------------------------------------%

getaddrinfo(NodeAndOrService, Flags0, MaybeFamily0, MaybeSocktype0,
        MaybeProtocol0, Result) :-
    make_node_and_service_c_strings(NodeAndOrService, Node, Service),
    ( if
        nas_service_is_numeric(NodeAndOrService),
        flag_numericservice(NumericServiceFlag)
    then
        Flags = Flags0 \/ NumericServiceFlag
    else
        Flags = Flags0
    ),
    map_maybe((pred(A::in, B::out) is det :-
            family_int(A, B)
        ), MaybeFamily0, MaybeFamily),
    maybe_default(0, MaybeFamily, Family),
    map_maybe((pred(A::in, B::out) is det :-
            socktype_int(A, B)
        ), MaybeSocktype0, MaybeSocktype),
    maybe_default(0, MaybeSocktype, Socktype),
    MaybeProtocol = map_maybe((func(P) = P ^ p_num), MaybeProtocol0),
    maybe_default(0, MaybeProtocol, Protocol),
    promise_pure (
        getaddrinfo_c(Node, Service, Flags, Family, Socktype, Protocol,
            AddrInfoList0, Result0),
        ( if Result0 = gai_ok then
            addrinfo_c_to_addrinfos(AddrInfoList0, AddrInfoList),
            impure free_addrinfo_c(AddrInfoList0),
            Result = ok(AddrInfoList)
        else if Result0 = gai_not_found then
            Result = ok([])
        else
            Result = error(gai_strerror(Result0))
        )
    ).

:- pred getaddrinfo_c(nullable_string::in, nullable_string::in,
    int::in, int::in, int::in, int::in, addrinfo_c::out, int::out) is det.

:- pragma foreign_proc("C",
    getaddrinfo_c(Node::in, Service::in, Flags::in, Family::in, Socktype::in,
        Protocol::in, AddrInfoList::out, Result::out),
    [will_not_call_mercury, promise_pure, thread_safe,
     will_not_throw_exception],
"
    struct addrinfo hints;
    memset(&hints, 0, sizeof(hints));
    hints.ai_flags = Flags;
    hints.ai_family = Family;
    hints.ai_socktype = Socktype;
    hints.ai_protocol = Protocol;

    Result = getaddrinfo(Node, Service, &hints, &AddrInfoList);
").

%-----------------------------------------------------------------------%

:- pred make_node_and_service_c_strings(node_and_or_service::in,
    nullable_string::out, nullable_string::out) is det.

make_node_and_service_c_strings(node_and_service(Node0, Service0), Node,
        Service) :-
    make_nullable_string(Node0, Node),
    make_service(Service0, Service).
make_node_and_service_c_strings(node_only(Node0), Node, null_string) :-
    make_nullable_string(Node0, Node).
make_node_and_service_c_strings(service_only(Service0), null_string,
        Service) :-
    make_service(Service0, Service).

:- pred make_service(service::in, nullable_string::out) is det.

make_service(numeric_service(ServiceNum), Service) :-
    make_nullable_string(string(ServiceNum), Service).
make_service(string_service(ServiceStr), Service) :-
    make_nullable_string(ServiceStr, Service).

:- pred nas_service_is_numeric(node_and_or_service::in) is semidet.

nas_service_is_numeric(NAS) :-
    ( NAS = service_only(Service)
    ; NAS = node_and_service(_, Service)
    ),
    service_is_numeric(Service).

:- pred service_is_numeric(service::in) is semidet.

service_is_numeric(numeric_service(_)).

%-----------------------------------------------------------------------------%

:- type addrinfo_c.

:- pragma foreign_type("C",
    addrinfo_c,
    "struct addrinfo*",
    [can_pass_as_mercury_type]).

:- pred addrinfo_c_to_addrinfos(addrinfo_c::in, list(addrinfo)::out) is det.

addrinfo_c_to_addrinfos(AddrInfoC, AddrInfoList) :-
    read_addrinfo(AddrInfoC, FamilyInt, SocktypeInt, ProtocolNum, Sockaddr),
    ( if read_addrinfo_name(AddrInfoC, Name) then
        MaybeName = yes(Name)
    else
        MaybeName = no
    ),
    ( if
        family_int(FamilyPrime, FamilyInt)
    then
        Family = FamilyPrime
    else
        unexpected($file, $pred,
            "getaddrinfo returned '0' for family")
    ),
    ( if
        socktype_int(SocktypePrime, SocktypeInt)
    then
        MaybeSocktype = yes(SocktypePrime)
    else
        MaybeSocktype = no
    ),
    AddrInfo = addrinfo(Family, MaybeSocktype, ProtocolNum, Sockaddr,
        MaybeName),
    ( if next_addrinfo_c(AddrInfoC, NextAddrInfoC) then
        addrinfo_c_to_addrinfos(NextAddrInfoC, AddrInfoList0),
        AddrInfoList = [AddrInfo | AddrInfoList0]
    else
        AddrInfoList = [AddrInfo]
    ).

:- pred read_addrinfo(addrinfo_c::in, int::out, int::out, int::out,
    sockaddr::out) is det.

:- pragma foreign_proc("C",
    read_addrinfo(AddrInfo::in, Family::out, Socktype::out, ProtocolNum::out,
        Sockaddr::out),
    [will_not_call_mercury, promise_pure, thread_safe,
     will_not_throw_exception],
"
    int len;

    Family = AddrInfo->ai_family;
    Socktype = AddrInfo->ai_socktype;
    ProtocolNum = AddrInfo->ai_protocol;
    Sockaddr = MR_GC_malloc(AddrInfo->ai_addrlen);
    memcpy(Sockaddr, AddrInfo->ai_addr, AddrInfo->ai_addrlen);

    len = sock_addr_size(Sockaddr);
    assert((len == -1) || (len == AddrInfo->ai_addrlen));
").

:- pred read_addrinfo_name(addrinfo_c::in, string::out) is semidet.

:- pragma foreign_proc("C",
    read_addrinfo_name(AddrInfo::in, Name::out),
    [will_not_call_mercury, promise_pure, thread_safe,
     will_not_throw_exception],
"
    SUCCESS_INDICATOR = AddrInfo->ai_canonname != NULL;
    if (SUCCESS_INDICATOR) {
        MR_make_aligned_string_copy(Name, AddrInfo->ai_canonname);
    }
").

:- pred next_addrinfo_c(addrinfo_c::in, addrinfo_c::out) is semidet.

:- pragma foreign_proc("C",
    next_addrinfo_c(AddrInfo::in, NextAddrInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe,
     will_not_throw_exception],
"
    NextAddrInfo = AddrInfo->ai_next;
    SUCCESS_INDICATOR = NextAddrInfo != NULL;
").

:- impure pred free_addrinfo_c(addrinfo_c::in) is det.

:- pragma foreign_proc("C",
    free_addrinfo_c(AddrInfo::in),
    [will_not_call_mercury, thread_safe, will_not_throw_exception],
"
    freeaddrinfo(AddrInfo);
").

%-----------------------------------------------------------------------------%

:- func gai_ok = int.
gai_ok = 0.

:- func gai_not_found = int.
:- pragma foreign_proc("C",
    gai_not_found = (Num::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "
        Num = EAI_NONAME;
    ").

:- func gai_strerror(int) = string.

:- pragma foreign_proc("C",
    gai_strerror(Num::in) = (String::out),
    [will_not_call_mercury, thread_safe, promise_pure],
    "
        MR_make_aligned_string_copy(String, gai_strerror(Num));
    ").

%-----------------------------------------------------------------------------%

:- pred maybe_default(T::in, maybe(T)::in, T::out) is det.

maybe_default(Default, no, Default).
maybe_default(_, yes(X), X).

%-----------------------------------------------------------------------%

:- type nullable_string.
:- pragma foreign_type("C",
    nullable_string,
    "char*").

:- func null_string = nullable_string.

:- pragma foreign_proc("C",
    null_string = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe,
    will_not_throw_exception],
"
    X = NULL;
").

:- pred make_nullable_string(string::in, nullable_string::out) is det.

:- pragma foreign_proc("C",
    make_nullable_string(Str0::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe,
    will_not_throw_exception],
"
    Str = Str0;
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
