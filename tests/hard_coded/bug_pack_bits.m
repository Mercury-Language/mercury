%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bug_pack_bits.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.

main(!IO) :-
    getaddrinfo(AddrInfos),
    io.write_string("getaddrinfo for www.google.com:\n", !IO),
    foldl(write_addrinfo, AddrInfos, !IO).

:- pred write_addrinfo(addrinfo::in, io::di, io::uo) is det.

write_addrinfo(addrinfo(Family, Socktype, ProtoNum), !IO) :-
    io.format("Family: %-10s Socktype: %-12s Protocol: %s\n",
        [s(string(Family) ++ ","), s(string(Socktype) ++ ","), s(ProtoName)],
        !IO),
    ProtoName = string(ProtoNum).

:- type addrinfo
    --->    addrinfo(
                ai_family       :: family,
                ai_socktype     :: socktype,
                ai_protocol     :: protocol_num
            ).

:- pred getaddrinfo(list(addrinfo)::out) is det.

getaddrinfo([addrinfo(fam_inet, sock_stream, 89),
             addrinfo(fam_inet, sock_dgram, 80),
             addrinfo(fam_inet6, sock_stream, 80),
             addrinfo(fam_inet6, sock_dgram, 80)]).

    % Protocol number.
    %
    % See getprotobyname/4.
    %
:- type protocol_num == int.

    % The socket family. This type is incomplete, support for socket
    % families such as IPX or appletalk will probably never be added.
    % However Unix domain sockets may be added in the future.
    %
:- type family
    --->    fam_inet
    ;       fam_inet6.

%---------------------------------------------------------------------------%

    % The socket type. Informally (for fam_inet and fam_inet6) these
    % correspond to TCP and UDP respectively. More precicely these specify
    % the socket's behavour, the protocol is optionally specified
    % seperately.
    %
:- type socktype
    --->    sock_stream
    ;       sock_dgram.

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

%---------------------------------------------------------------------------%

    % This list of address families is from socket(2) on linux.
    %
:- pragma foreign_enum("C", family/0, [
    fam_inet       - "AF_INET",
    fam_inet6      - "AF_INET6"
]).

:- pragma foreign_enum("C", socktype/0, [
    sock_stream    - "SOCK_STREAM",
    sock_dgram     - "SOCK_DGRAM"
]).

%---------------------------------------------------------------------------%
