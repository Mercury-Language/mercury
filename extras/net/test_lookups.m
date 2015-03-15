
:- module test_lookups.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module string.

:- import_module net.
:- import_module net.netdb.
:- import_module net.types.

main(!IO) :-
    TCPS = "tcp",
    getprotobyname(TCPS, TCP, !IO),
    io.format("get_proto_by_name(""%s"", %s, !IO).\n",
        [s(TCPS), s(string(TCP))], !IO),
    lookup_host_and_service("www.google.com", string_service("http"),
        yes(fam_inet), no, GAIResultHostService),
    (
        GAIResultHostService = ok(HostServiceResults),
        io.write_string("www.google.com:\n", !IO),
        foldl(write_lookup_result, HostServiceResults, !IO)
    ;
        GAIResultHostService = error(ErrorA),
        io.format("Lookup error for www.google.com: %s", [s(ErrorA)], !IO)
    ),
    lookup_local_socket(string_service("http"), yes(fam_inet),
        yes(sock_stream), ResultLocalSocket),
    (
        ResultLocalSocket = ok(LocalSockets),
        io.write_string("local sockets:\n", !IO),
        foldl(write_lookup_result, LocalSockets, !IO)
    ;
        ResultLocalSocket = error(ErrorB),
        io.format("Lookup error for local sockets: %s", [s(ErrorB)], !IO)
    ).

:- pred write_lookup_result(lookup_result::in,
    io::di, io::uo) is det.

write_lookup_result(lookup_result(Family, Socktype, ProtoNum, SockAddr),
        !IO) :-
    io.format("Family: %s, Socktype: %s, Protocol: %s, Addr: %s\n",
        [s(string(Family)), s(string(Socktype)), s(ProtoName),
            s(SockStr)],
        !IO),
    ProtoName = string(ProtoNum),
    ( ipv4_sockaddr(InAddr, Port, SockAddr) ->
        SockStr = format("%s:%d", [s(to_string(InAddr)), i(Port)])
    ;
        SockStr = "unknown"
    ).

