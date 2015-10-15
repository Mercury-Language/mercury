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

:- type protocol_num == int.

:- type family
    --->    fam_inet
    ;       fam_inet6.

:- type socktype
    --->    sock_stream
    ;       sock_dgram.

:- pragma foreign_enum("C", family/0, [
    fam_inet       - "1",
    fam_inet6      - "2"
]).

:- pragma foreign_enum("C", socktype/0, [
    sock_stream    - "3",
    sock_dgram     - "4"
]).

%---------------------------------------------------------------------------%
