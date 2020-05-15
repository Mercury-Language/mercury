%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2016, 2018 The Mercury Team
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module: types
% Main Author:  Paul Bone <paul@bone.id.au>
% Stability:    low
%
% Networking datatypes and conversion predicates.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module net.types.
:- interface.

:- import_module int.
:- import_module string.

%-----------------------------------------------------------------------------%

    % A port number.
    %
:- type port == int.

    % Protocol number.
    %
    % See getprotobyname/4.
    %
:- type protocol_num == int.

%-----------------------------------------------------------------------------%

    % The socket family.  This type is incomplete, support for socket
    % families such as IPX or appletalk will probably never be added.
    % However Unix domain sockets may be added in the future.
    %
:- type family
    --->    fam_inet
    ;       fam_inet6.

    % Convert to and from the integer representation of a family.  This is
    % sometimes required, for example when '0' indicates unspecified in the
    % underlying foreign code.
    %
:- pred family_int(family, int).
:- mode family_int(in, out) is det.
:- mode family_int(out, in) is semidet.

%-----------------------------------------------------------------------------%

    % The socket type.  Informally (for fam_inet and fam_inet6) these
    % correspond to TCP and UDP respectively.  More precisely these specify
    % the socket's behaviour, the protocol is optionally specified
    % separately.
    %
:- type socktype
    --->    sock_stream
    ;       sock_dgram.

    % Convert socktypes to and from integers.
    %
:- pred socktype_int(socktype, int).
:- mode socktype_int(in, out) is det.
:- mode socktype_int(out, in) is semidet.

%-----------------------------------------------------------------------------%

    % An address type can be converted to and from strings.
    %
    % The to and from string predicates are not guaranteed to be reciprocal.
    % Therefore they should not be reverse modes of one-another.
    %
:- typeclass addr(A) where [
    pred from_string(string::in, A::uo) is semidet,

    pred to_string(A::in, string::uo) is det
].

:- func to_string(A) = string <= addr(A).

    % Existentially typed from_string predicate.
    %
    % This will try to to recognise the address. It tries IPv4 then IPv6.
    %
:- some [A] pred exist_from_string(string::in, A::uo) is semidet => addr(A).

%-----------------------------------------------------------------------------%

    % An IPv4 Address.
    %
:- type in_addr.

    % The conversion code in this typeclass instance will convert an in_addr
    % into a dotted-decimal format.
    %
    % The dotted-decimal format is the typical format with four decimal
    % numbers separated by dots.
    %
:- instance addr(in_addr).

    % Constant (special) IP addresses:
    %  + the wildcard address:  0.0.0.0
    %  + the loopback address:  127.0.0.1
    %  + the broadcast address: 255.255.255.255
    %
    % See also ip(7).
    %
:- func in_addr_any = in_addr.
:- func in_addr_loopback = in_addr.
:- func in_addr_broadcast = in_addr.

    % Convert an address from numbers-and-dots format into an in_addr
    % structure.
    %
    % The numbers-and-dots format is general and allows up to four numbers
    % separated by dots, the numbers may be decimal, octal or hexadecimal,
    % See inet_aton(3).
    %
:- pred in_addr_from_string(string::in, in_addr::uo) is semidet.

%-----------------------------------------------------------------------------%

    % An IPv6 address.
    %
:- type in6_addr.

:- instance addr(in6_addr).

    % Constant IPv6 addresses
    %  + the wildcard address:  ::0
    %  + the loopback address:  ::1
    %
    % See also ipv6(7).
    %
:- func in6_addr_any = in6_addr.
:- func in6_addr_loopback = in6_addr.

:- pred in6_addr_from_string(string::in, in6_addr::uo) is semidet.

%-----------------------------------------------------------------------------%

    % A socket address, for example in ipv4 this is an IP address and a port
    % number pair.
    %
:- type sockaddr.

:- func family(sockaddr) = family.

    % Construct and deconstruct ipv4 sockaddrs.  Deconstruction fails if
    % this is not an ipv4 socket address.
    %
:- pred ipv4_sockaddr(in_addr, port, sockaddr).
:- mode ipv4_sockaddr(in, in, uo) is det.
:- mode ipv4_sockaddr(out, out, in) is semidet.

:- func ipv4_sockaddr(in_addr, port) = sockaddr.

    % Construct and deconstruct ipv6 sockaddrs.  Deconstruction fails if
    % this is not an ipv6 socket address.
    %
:- pred ipv6_sockaddr(in6_addr, port, sockaddr).
:- mode ipv6_sockaddr(in, in, uo) is det.
:- mode ipv6_sockaddr(out, out, in) is semidet.

:- func ipv6_sockaddr(in6_addr, port) = sockaddr.

:- some [A] pred sockaddr_get_addr_port(sockaddr::in, A::out, port::out)
        is semidet
    => addr(A).

    % Get the node address from a socket address.
    %
    % If the node address type is unknown or unsupported this call will
    % fail.
    %
:- some [A] pred sockaddr_get_addr(sockaddr::in, A::out) is semidet
    => addr(A).

    % Retrieve the port number from the socket address.  Not all socket
    % addresses have port numbers so this call may fail.
    %
:- pred sockaddr_get_port(sockaddr::in, port::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module require.

:- import_module net.errno.

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C",
"
#ifdef MR_WIN32
    #include ""mercury_windows.h""
    #include <winsock2.h>
    #include <ws2tcpip.h>
#else
    #include <netinet/in.h>
    #include <arpa/inet.h>
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

:- pragma foreign_proc("C",
    family_int(Family::in, Int::out),
    [will_not_call_mercury, promise_pure, thread_safe,
     will_not_throw_exception],
"
    Int = Family;
").

:- pragma foreign_proc("C",
    family_int(Family::out, Int::in),
    [will_not_call_mercury, promise_pure, thread_safe,
     will_not_throw_exception],
"
    Family = Int;
    switch (Family) {
        case AF_INET:
        case AF_INET6:
            SUCCESS_INDICATOR = MR_YES;
            break;
        default:
            SUCCESS_INDICATOR = MR_NO;
            break;
    }
").

:- pragma foreign_enum("C", socktype/0,
    [sock_stream    - "SOCK_STREAM",
     sock_dgram     - "SOCK_DGRAM"]).
% See socket(2) for the meaning of these values.
%     sock_seqpacket - "SOCK_SEQPACKET",
%     sock_raw       - "SOCK_RAW",
%     sock_rdm       - "SOCK_RDM",
    % Note: sock_packet is obsolete.
    % Note: We deliberately do not support the non-portable SOCK_NONBLOCK
    % and SOCK_CLOEXEC values, this functionality should be accessed via
    % setsocketopt.

:- pragma foreign_proc("C",
    socktype_int(Socktype::in, Int::out),
    [will_not_call_mercury, promise_pure, thread_safe,
     will_not_throw_exception],
"
    Int = Socktype;
").

:- pragma foreign_proc("C",
    socktype_int(Socktype::out, Int::in),
    [will_not_call_mercury, promise_pure, thread_safe,
     will_not_throw_exception],
"
    Socktype = Int;
    switch (Socktype) {
        case SOCK_STREAM:
        case SOCK_DGRAM:
            SUCCESS_INDICATOR = MR_YES;
            break;
        default:
            SUCCESS_INDICATOR = MR_NO;
            break;
    }
").

%-----------------------------------------------------------------------------%

to_string(Addr) = String :-
    to_string(Addr, String).

    % This type allows code in this module to return the existentially
    % quantified address.
    %
:- type univ_address
    --->    some [A] (univ_address(A) => addr(A)).

exist_from_string(String, Addr) :-
    ( if in_addr_from_string(String, AddrPrime) then
        UAddr = 'new univ_address'(AddrPrime)
    else if in6_addr_from_string(String, AddrPrime) then
        UAddr = 'new univ_address'(AddrPrime)
    else
        false
    ),
    univ_address(Addr) = UAddr.

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C",
    in_addr,
    "struct in_addr*",
    [can_pass_as_mercury_type]).

:- instance addr(in_addr) where [
    pred(from_string/2) is in_addr_from_string,
    pred(to_string/2) is in_addr_to_string
].

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    in_addr_any = (Addr::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Addr = MR_GC_NEW(struct in_addr);
    Addr->s_addr = htonl(INADDR_ANY);
").

:- pragma foreign_proc("C",
    in_addr_loopback = (Addr::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Addr = MR_GC_NEW(struct in_addr);
    Addr->s_addr = htonl(INADDR_LOOPBACK);
").

:- pragma foreign_proc("C",
    in_addr_broadcast = (Addr::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Addr = MR_GC_NEW(struct in_addr);
    Addr->s_addr = htonl(INADDR_BROADCAST);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    in_addr_from_string(String::in, Addr::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Addr = MR_GC_NEW(struct in_addr);

    SUCCESS_INDICATOR = inet_pton(AF_INET, String, Addr);
").

%-----------------------------------------------------------------------------%

:- pred in_addr_to_string(in_addr::in, string::uo) is det.
:- func in_addr_to_string(in_addr) = string.

in_addr_to_string(Addr, String) :-
    in_addr_to_string(Addr, String, Success, Errno),
    (
        Success = yes
    ;
        Success = no,
        unexpected($file, $pred,
            "Cannot convert address to string" ++ strerror(Errno))
    ).
in_addr_to_string(Addr) = String :-
    in_addr_to_string(Addr, String).

:- pred in_addr_to_string(in_addr::in, string::uo, bool::out, errno::out)
    is det.

:- pragma foreign_proc("C",
    in_addr_to_string(Addr::in, String::uo, Success::out, Errno::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    char *temp = MR_GC_malloc_atomic(INET_ADDRSTRLEN);

    String = (char*)inet_ntop(AF_INET, Addr, temp, INET_ADDRSTRLEN);
    if (String != NULL) {
        Success = MR_YES;
    } else {
        Success = MR_NO;
        Errno = error();
    }
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pragma foreign_type("C",
    in6_addr,
    "struct in6_addr*",
    [can_pass_as_mercury_type]).

:- instance addr(in6_addr) where [
    pred(from_string/2) is in6_addr_from_string,
    pred(to_string/2) is in6_addr_to_string
].

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    in6_addr_any = (Addr::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Addr = MR_GC_NEW(struct in6_addr);
    MR_memcpy(Addr, &in6addr_any, sizeof(in6addr_any));
").

:- pragma foreign_proc("C",
    in6_addr_loopback = (Addr::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Addr = MR_GC_NEW(struct in6_addr);
    MR_memcpy(Addr, &in6addr_loopback, sizeof(in6addr_loopback));
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    in6_addr_from_string(String::in, Addr::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Addr = MR_GC_NEW(struct in6_addr);

    SUCCESS_INDICATOR = inet_pton(AF_INET6, String, Addr);
").

%-----------------------------------------------------------------------------%

:- pred in6_addr_to_string(in6_addr::in, string::uo) is det.

in6_addr_to_string(Addr, String) :-
    in6_addr_to_string(Addr, String, Success, Errno),
    (
        Success = yes
    ;
        Success = no,
        unexpected($file, $pred,
            "Cannot convert address to string" ++ strerror(Errno))
    ).

:- pred in6_addr_to_string(in6_addr::in, string::uo, bool::out, errno::out)
    is det.

:- pragma foreign_proc("C",
    in6_addr_to_string(Addr::in, String::uo, Success::out, Errno::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    char *temp = MR_GC_malloc_atomic(INET6_ADDRSTRLEN);

    String = (char*)inet_ntop(AF_INET6, Addr, temp, INET6_ADDRSTRLEN);
    if (String != NULL) {
        Success = MR_YES;
    } else {
        Success = MR_NO;
        Errno = error();
    }
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C",
"
    union my_sockaddr {
        struct sockaddr         raw;
        struct sockaddr_in      in;
        struct sockaddr_in6     in6;
    };

    size_t sock_addr_size(union my_sockaddr *addr);
").

:- pragma foreign_code("C",
"
    size_t sock_addr_size(union my_sockaddr *addr) {
        switch (addr->raw.sa_family) {
            case AF_INET:
                return sizeof(struct sockaddr_in);
            case AF_INET6:
                return sizeof(struct sockaddr_in6);
            default:
                fprintf(stderr, ""Unhandled family\\n"");
                abort();
                return -1; /* MSVC doesn't understand abort(); */
        }
    }
").

:- pragma foreign_type("C",
    sockaddr,
    "union my_sockaddr *",
    [can_pass_as_mercury_type]).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    family(Addr::in) = (Family::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "Family = Addr->raw.sa_family;").

%-----------------------------------------------------------------------------%

ipv4_sockaddr(InAddr, Port) = Sockaddr :-
    ipv4_sockaddr(InAddr, Port, Sockaddr).

:- pragma foreign_proc("C",
    ipv4_sockaddr(InAddr::in, Port::in, Sockaddr::uo),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Sockaddr = (union my_sockaddr*)MR_GC_NEW(struct sockaddr_in);
    Sockaddr->in.sin_family = AF_INET;
    Sockaddr->in.sin_port = htons(Port);
    Sockaddr->in.sin_addr = *InAddr;
").

:- pragma foreign_proc("C",
    ipv4_sockaddr(InAddr::out, Port::out, Sockaddr::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    if (Sockaddr->in.sin_family == AF_INET) {
        Port = ntohs(Sockaddr->in.sin_port);
        InAddr = &(Sockaddr->in.sin_addr);
        SUCCESS_INDICATOR = MR_YES;
    } else {
        SUCCESS_INDICATOR = MR_NO;
    }
").

%-----------------------------------------------------------------------%

ipv6_sockaddr(InAddr, Port) = Sockaddr :-
    ipv6_sockaddr(InAddr, Port, Sockaddr).

:- pragma foreign_proc("C",
    ipv6_sockaddr(In6Addr::in, Port::in, Sockaddr::uo),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Sockaddr = (union my_sockaddr*)MR_GC_NEW(struct sockaddr_in6);
    Sockaddr->in6.sin6_family = AF_INET6;
    Sockaddr->in6.sin6_port = htons(Port);
    Sockaddr->in6.sin6_addr = *In6Addr;
").

:- pragma foreign_proc("C",
    ipv6_sockaddr(In6Addr::out, Port::out, Sockaddr::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    if (Sockaddr->in6.sin6_family == AF_INET6) {
        Port = ntohs(Sockaddr->in6.sin6_port);
        In6Addr = &(Sockaddr->in6.sin6_addr);
        SUCCESS_INDICATOR = MR_YES;
    } else {
        SUCCESS_INDICATOR = MR_NO;
    }
").

%-----------------------------------------------------------------------------%

sockaddr_get_addr_port(SockAddr, Addr, Port) :-
    sockaddr_get_addr(SockAddr, Addr),
    sockaddr_get_port(SockAddr, Port).

%-----------------------------------------------------------------------------%

sockaddr_get_addr(SockAddr, Addr) :-
    ( if ipv4_sockaddr(AddrPrime, _, SockAddr) then
        UAddr = 'new univ_address'(AddrPrime)
    else if ipv6_sockaddr(AddrPrime, _, SockAddr) then
        UAddr = 'new univ_address'(AddrPrime)
    else
        false
    ),
    univ_address(Addr) = UAddr.

%-----------------------------------------------------------------------------%

sockaddr_get_port(Sockaddr, Port) :-
    ( if ipv4_sockaddr(_, PortPrime, Sockaddr) then
        Port = PortPrime
    else if ipv6_sockaddr(_, PortPrime, Sockaddr) then
        Port = PortPrime
    else
        false
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
