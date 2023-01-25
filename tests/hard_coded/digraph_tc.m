%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test digraph.tc.
%
%---------------------------------------------------------------------------%

:- module digraph_tc.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module benchmarking.
:- import_module bool.
:- import_module digraph.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module random.
:- import_module random.sfc64.
:- import_module random.system_rng.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if
        Args = ["benchmark", SizeStr, RepeatStr],
        string.to_int(SizeStr, Size),
        Size > 1,
        string.to_int(RepeatStr, Repeat),
        Repeat > 0
    then
        init_random(MaybeRNG, !IO),
        (
            MaybeRNG = ok(R0),
            generate_graph(Size, G, R0, _R),
            run_benchmark(Size, G, Repeat, !IO)
        ;
            MaybeRNG = error(Error),
            io.write_string(Error, !IO),
            io.nl(!IO),
            io.set_exit_status(1, !IO)
        )
    else
        main_2(Args, !IO)
    ).

:- pred main_2(list(string)::in, io::di, io::uo) is det.

main_2(Args, !IO) :-
    ( if Args = [] then
        load_graph("digraph_tc.inp", LoadRes, !IO),
        Verbose = yes
    else if Args = [FileName] then
        load_graph(FileName, LoadRes, !IO),
        Verbose = yes
    else if
        Args = ["random", SizeStr],
        string.to_int(SizeStr, Size),
        Size > 1
    then
        Verbose = no,
        init_random(MaybeRNG, !IO),
        (
            MaybeRNG = ok(R0),
            generate_graph(Size, G0, R0, _R),
            LoadRes = ok(G0)
        ;
            MaybeRNG = error(RNGError),
            LoadRes = error(RNGError)
        )
    else
        LoadRes = error("wrong arguments"),
        Verbose = no
    ),
    (
        LoadRes = ok(G),
        test_graph(G, Verbose, !IO)
    ;
        LoadRes = error(Error),
        io.write_string(Error, !IO),
        io.nl(!IO),
        io.set_exit_status(1, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred load_graph(string::in, maybe_error(digraph(string))::out,
    io::di, io::uo) is det.

load_graph(FileName, Res, !IO) :-
    io.open_input(FileName, OpenRes, !IO),
    (
        OpenRes = ok(Stream),
        io.read(Stream, ReadRes, !IO),
        (
            ReadRes = ok(AssocList),
            digraph.from_assoc_list(AssocList, G),
            Res = ok(G)
        ;
            ReadRes = eof,
            Res = error("unexpected eof")
        ;
            ReadRes = error(Error, _),
            Res = error(Error)
        ),
        io.close_input(Stream, !IO)
    ;
        OpenRes = error(Error),
        Res = error(io.error_message(Error))
    ).

%---------------------------------------------------------------------------%

:- some [R] pred init_random(maybe_error(R)::out, io::di, io::uo) is det
    => random(R).

init_random(Res, !IO) :-
    open_system_rng(MaybeRNG, !IO),
    (
        MaybeRNG = ok(SystemRNG),
        system_rng.generate_uint64(SystemRNG, A, !IO),
        system_rng.generate_uint64(SystemRNG, B, !IO),
        system_rng.generate_uint64(SystemRNG, C, !IO),
        close_system_rng(SystemRNG, !IO),

        sfc64.seed(A, B, C, Params, UState),
        R = make_shared_random(Params, UState),
        Res = ok(R)
    ;
        MaybeRNG = error(Error),
        Res = error(Error)
    ).

:- pred generate_graph(int::in, digraph(string)::out,
    R::in, R::out) is det <= random(R).

generate_graph(Size, !:G, !R) :-
    !:G = digraph.init,
    generate_vertices(0, Size, [], RevKeys, !G),
    array.from_reverse_list(RevKeys, KeysArray),

    random.uniform_int_in_range(10, 30, Factor, !R),
    NumEdges = Size * Factor / 10,

    generate_edges(KeysArray, NumEdges, !G, !R).

:- pred generate_vertices(int::in, int::in,
    list(digraph_key(string))::in, list(digraph_key(string))::out,
    digraph(string)::in, digraph(string)::out) is det.

generate_vertices(I, Size, !RevKeys, !G) :-
    ( if I >= Size then
        true
    else
        generate_vertex(I, Key, !G),
        !:RevKeys = [Key | !.RevKeys],
        generate_vertices(I + 1, Size, !RevKeys, !G)
    ).

:- pred generate_vertex(int::in, digraph_key(string)::out,
    digraph(string)::in, digraph(string)::out) is det.

generate_vertex(N, Key, !G) :-
    Name = "N" ++ string.from_int(N),
    add_vertex(Name, Key, !G).

:- pred generate_edges(array(digraph_key(string))::array_ui, int::in,
    digraph(string)::in, digraph(string)::out,
    R::in, R::out) is det <= random(R).

generate_edges(KeysArray, RemEdges, !G, !R) :-
    ( if RemEdges =< 0 then
        true
    else
        generate_edge(KeysArray, !G, !R),
        generate_edges(KeysArray, RemEdges - 1, !G, !R)
    ).

:- pred generate_edge(array(digraph_key(string))::array_ui,
    digraph(string)::in, digraph(string)::out,
    R::in, R::out) is det <= random(R).

generate_edge(KeysArray, !G, !R) :-
    array.size(KeysArray, NumKeys),
    random.uniform_int_in_range(0, NumKeys, I, !R),
    random.uniform_int_in_range(0, NumKeys, J, !R),
    array.lookup(KeysArray, I, KeyI),
    array.lookup(KeysArray, J, KeyJ),
    digraph.add_edge(KeyI, KeyJ, !G).

%---------------------------------------------------------------------------%

:- pred test_graph(digraph(string)::in, bool::in, io::di, io::uo) is det.

test_graph(G, Verbose, !IO) :-
    tc(G, TC),
    slow_tc(G, SlowTC),

    io.print_line("---- G ----", !IO),
    write_graph(G, !IO),
    (
        Verbose = yes,
        io.print_line("---- tc(G) ----", !IO),
        write_graph(TC, !IO),
        io.print_line("---- slow_tc(G) ----", !IO),
        write_graph(SlowTC, !IO)
    ;
        Verbose = no
    ),

    ( if same_graph(TC, SlowTC) then
        true
    else
        io.write_string("** TC != SlowTC\n\n", !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred same_graph(digraph(T)::in, digraph(T)::in) is semidet.

same_graph(A, B) :-
    same_graph_2(A, B),

    digraph.inverse(A, InvA),
    digraph.inverse(B, InvB),
    same_graph_2(InvA, InvB).

:- pred same_graph_2(digraph(T)::in, digraph(T)::in) is semidet.

same_graph_2(A, B) :-
    digraph.to_assoc_list(A, PairsA),
    digraph.to_assoc_list(B, PairsB),
    sort(PairsA, SortedPairsA),
    sort(PairsB, SortedPairsB),
    SortedPairsA = SortedPairsB.

%---------------------------------------------------------------------------%

:- pred write_graph(digraph(string)::in, io::di, io::uo) is det.

write_graph(G, !IO) :-
    digraph.to_assoc_list(G, Edges),
    io.write_string("digraph {\n", !IO),
    io.format("  /* %d edges */\n", [i(length(Edges))], !IO),
    list.foldl(write_edge, Edges, !IO),
    io.write_string("}\n\n", !IO).

:- pred write_edge(pair(string)::in, io::di, io::uo) is det.

write_edge(A - B, !IO) :-
    io.format("  %s -> %s;\n", [s(A), s(B)], !IO).

%---------------------------------------------------------------------------%

:- pred run_benchmark(int::in, digraph(string)::in, int::in, io::di, io::uo)
    is cc_multi.

run_benchmark(Size, G, Repeat, !IO) :-
    NumEdges = length(to_assoc_list(G)),
    io.format("vertices:   %d\n", [i(Size)], !IO),
    io.format("edges:      %d\n", [i(NumEdges)], !IO),

    benchmark_det(tc, G, _BasicTC, Repeat, TimeBasicTC),
    AvgTimeBasicTC = float(TimeBasicTC) / float(Repeat),
    io.format("tc avg:     %f ms\n", [f(AvgTimeBasicTC)], !IO).

%---------------------------------------------------------------------------%
