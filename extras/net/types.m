%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury Team
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB
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

    % The socket type.  Informally (for fam_inet and fam_inet6) these
    % correspond to TCP and UDP respectively.  More precicely these specify
    % the socket's behavour, the protocol is optionally specified
    % seperately.
    %
:- type socktype
    --->    sock_stream
    ;       sock_dgram.

	% An IPv4 Address.
	%
:- type in_addr.

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
    % seperated by dots, the numbers may be decimal, octal or hexadecimal, 
    % See inet_aton(3).
	%
:- pred from_string(string::in, in_addr::uo) is semidet.

    % Convert an in_addr into a dotted-decimal format.  This predicate and
    % inet_aton are not reciprical, therefore this should not be a reverse
    % mode of the above.
    %
    % The dotted-decimal format is the typical format with four decimal
    % numbers seperated by dots.
	%
:- pred to_string(in_addr::in, string::uo) is det.
:- func to_string(in_addr) = string.

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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- implementation.

:- import_module bool.
:- import_module int.
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

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C",
    in_addr,
    "struct in_addr*",
    [can_pass_as_mercury_type]).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    in_addr_any = (Addr::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Addr = MR_GC_NEW(struct in_addr);
    Addr->s_addr = INADDR_ANY;
").

:- pragma foreign_proc("C",
    in_addr_loopback = (Addr::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Addr = MR_GC_NEW(struct in_addr);
    Addr->s_addr = INADDR_LOOPBACK;
").

:- pragma foreign_proc("C",
    in_addr_broadcast = (Addr::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Addr = MR_GC_NEW(struct in_addr);
    Addr->s_addr = INADDR_BROADCAST;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
	from_string(String::in, Addr::uo),
	[will_not_call_mercury, promise_pure, thread_safe],
"
    Addr = MR_GC_NEW(struct in_addr);

    SUCCESS_INDICATOR = inet_pton(AF_INET, String, Addr);
").

%-----------------------------------------------------------------------------%

to_string(Addr, String) :-
    to_string(Addr, String, Success, Errno),
    (
        Success = yes
    ;
        Success = no,
        unexpected($file, $pred,
            "Cannot convert address to string" ++ strerror(Errno))
    ).
to_string(Addr) = String :-
    to_string(Addr, String).

:- pred to_string(in_addr::in, string::uo, bool::out, errno::out) is det.

:- pragma foreign_proc("C",
    to_string(Addr::in, String::uo, Success::out, Errno::out),
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

:- pragma foreign_decl("C",
"
    union my_sockaddr {
        struct sockaddr     raw;
        struct sockaddr_in  in;
    };

    size_t sock_addr_size(union my_sockaddr *addr);
").

:- pragma foreign_code("C",
"
    size_t sock_addr_size(union my_sockaddr *addr) {
        switch (addr->raw.sa_family) {
            case AF_INET:
                return sizeof(struct sockaddr_in);
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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
