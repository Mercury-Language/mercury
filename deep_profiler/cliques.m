%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2002, 2004-2006, 2008, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Authors: conway, zs.
%
% This module allows you build a description of a directed graph (represented
% as a set of arcs between nodes identified by dense small integers) and then
% find the strongly connected components (cliques) of that graph.
%---------------------------------------------------------------------------%

:- module cliques.

:- interface.

:- type graph.

:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

    % Create a graph with no edges.
    %
:- pred init(graph::out) is det.

    % Add an arc from one node to another.
    %
:- pred add_arc(graph::in, int::in, int::in, graph::out) is det.

    % Perform a topological sort on the graph. Each set of integers in the
    % resulting list gives the ids of the nodes in a clique. The list contains
    % the cliques in top-down order: if there is an arc from node A to node B
    % and the two nodes are not in the same clique, then the clique containing
    % node A will be before the clique containing node B.
    %
:- pred topological_sort(graph::in, list(set(int))::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array_util.
:- import_module dense_bitset.

:- import_module array.
:- import_module int.
:- import_module io.
:- import_module string.

%---------------------------------------------------------------------------%

:- type graph
    --->    graph(
                int,
                array(set(int))
            ).

:- type visit == dense_bitset.

%---------------------------------------------------------------------------%

init(graph(1, Array)) :-
    % The initial array size doesn't really matter.
    array.init(16, set.init, Array).

add_arc(graph(Size0, Array0), From, To, Graph) :-
    ( if array.in_bounds(Array0, From) then
        array.lookup(Array0, From, Tos0),
        set.insert(To, Tos0, Tos),
        array.set(From, Tos, u(Array0), Array),
        Size = int.max(int.max(From, To), Size0),
        Graph = graph(Size, Array)
    else
        array.size(Array0, Size),
        array.resize(Size * 2, init, u(Array0), Array1),
        add_arc(graph(Size0, Array1), From, To, Graph)
    ).

:- pred successors(graph::in, int::in, set(int)::out) is det.

successors(graph(_Size, Array), From, Tos) :-
    ( if array.in_bounds(Array, From) then
        array.lookup(Array, From, Tos)
    else
        Tos = set.init
    ).

:- pred mklist(int::in, list(int)::in, list(int)::out) is det.

mklist(N, Acc0, Acc) :-
    ( if N < 0 then
        Acc = Acc0
    else
        Acc1 = [N | Acc0],
        mklist(N - 1, Acc1, Acc)
    ).

topological_sort(Graph, Cliques) :-
    trace [compiletime(flag("tsort")), io(!IO)] (
        io.nl(!IO),
        io.write_string("the graph:\n", !IO),
        write_graph(Graph, !IO),
        io.nl(!IO)
    ),

    dfs_graph(Graph, Dfs),

    trace [compiletime(flag("tsort")), io(!IO)] (
        io.nl(!IO),
        io.write_string("the dfs:\n", !IO),
        write_dfs(Dfs, !IO),
        io.nl(!IO)
    ),

    inverse(Graph, InvGraph),

    trace [compiletime(flag("tsort")), io(!IO)] (
        io.nl(!IO),
        io.write_string("the inverse graph:\n", !IO),
        write_graph(InvGraph, !IO),
        io.nl(!IO)
    ),

    Visit = dense_bitset.init,
    tsort(Dfs, InvGraph, Visit, [], Cliques0),
    reverse(Cliques0, [], Cliques),

    trace [compiletime(flag("tsort")), io(!IO)] (
        io.nl(!IO),
        io.write_string("the cliques:\n", !IO),
        write_cliques(Cliques, !IO),
        io.nl(!IO),
        io.nl(!IO)
    ).

    % This is a copy of list.reverse_2, we copy it here so that it can be
    % compiled with --trace minimum even when the version in the standard
    % library is compiled with --trace deep.
    %
:- pred reverse(list(T)::in, list(T)::in, list(T)::out) is det.

reverse([], L, L).
reverse([X | Xs], L0, L) :-
    reverse(Xs, [X | L0], L).

:- pred tsort(list(int)::in, graph::in, visit::array_di, list(set(int))::in,
    list(set(int))::out) is det.

tsort([], _InvGraph, _Visit, !Cliques).
tsort([Node | Nodes], InvGraph, !.Visited, !Cliques) :-
    trace [compiletime(flag("tsort_loop")), io(!IO)] (
        io.write_string("tsort check ", !IO),
        io.write_int(Node, !IO),
        io.nl(!IO)
    ),

    ( if dense_bitset.member(Node, !.Visited) then
        trace [compiletime(flag("tsort_old")), io(!IO)] (
            io.write_string("tsort old ", !IO),
            io.write_int(Node, !IO),
            io.nl(!IO)
        )
    else
        trace [compiletime(flag("tsort_new")), io(!IO)] (
            io.write_string("tsort new ", !IO),
            io.write_int(Node, !IO),
            io.nl(!IO)
        ),

        dfs([Node], InvGraph, !.Visited, [], !:Visited, CliqueList),

        trace [compiletime(flag("tsort_clique")), io(!IO)] (
            io.write_string("tsort clique ", !IO),
            io.write_int(Node, !IO),
            io.write_string(" -> ", !IO),
            write_clique(CliqueList, !IO),
            io.nl(!IO)
        ),

        set.list_to_set(CliqueList, Clique),
        !:Cliques = [Clique | !.Cliques]
    ),
    tsort(Nodes, InvGraph, !.Visited, !Cliques).

    % Return a list containing all the nodes of the graph. The list is
    % effectively computed by randomly breaking all cycles, doing a pre-order
    % traversal of the resulting trees, and concatenating the resulting lists
    % in a random order.
    %
:- pred dfs_graph(graph::in, list(int)::out) is det.

dfs_graph(Graph, Dfs) :-
    Graph = graph(Size, _Array),
    mklist(Size, [], NodeList),
    Visit = dense_bitset.init,
    dfs_graph_2(NodeList, Graph, Visit, [], Dfs).

:- pred dfs_graph_2(list(int)::in, graph::in, visit::array_di,
    list(int)::in, list(int)::out) is det.

dfs_graph_2([], _Graph, _Visit, Dfs, Dfs).
dfs_graph_2([Node | Nodes], Graph, Visit0, Dfs0, Dfs) :-
    dfs([Node], Graph, Visit0, Dfs0, Visit, Dfs1),
    dfs_graph_2(Nodes, Graph, Visit, Dfs1, Dfs).

    % dfs(NodeList, Graph, Visit0, Dfs0, Visit, Dfs):
    % For every node in NodeList, add the node and all its successors to the
    % front of Dfs0, giving Dfs. The descendants of a node will in general be
    % after that node in Dfs. The only situation where that may not be the
    % case is when two nodes are descendants of each other. We detect such
    % situations by passing along the set of nodes that have been visited
    % already.
    %
:- pred dfs(list(int)::in, graph::in, visit::array_di, list(int)::in,
    visit::array_uo, list(int)::out) is det.

dfs([], _Graph, Visit, Dfs, Visit, Dfs).
dfs([Node | Nodes], Graph, Visit0, Dfs0, Visit, Dfs) :-
    ( if dense_bitset.member(Node, Visit0) then
        trace [compiletime(flag("dfs_old")), io(!IO)] (
            io.write_string("dfs old ", !IO),
            io.write_int(Node, !IO),
            io.nl(!IO)
        ),

        dfs(Nodes, Graph, Visit0, Dfs0, Visit, Dfs)
    else
        trace [compiletime(flag("dfs_new")), io(!IO)] (
            io.write_string("dfs new ", !IO),
            io.write_int(Node, !IO),
            io.nl(!IO)
        ),

        Visit1 = dense_bitset.insert(Visit0, Node),
        successors(Graph, Node, Succ),
        set.to_sorted_list(Succ, SuccList),
        dfs(SuccList, Graph, Visit1, Dfs0, Visit2, Dfs1),
        Dfs2 = [Node | Dfs1],
        dfs(Nodes, Graph, Visit2, Dfs2, Visit, Dfs)
    ).

:- pred inverse(graph::in, graph::out) is det.

inverse(Graph, InvGraph) :-
    init(InvGraph0),
    Graph = graph(Size, _Array),
    inverse_2(Size, Graph, InvGraph0, InvGraph).

:- pred inverse_2(int::in, graph::in, graph::in, graph::out) is det.

inverse_2(To, Graph, InvGraph0, InvGraph) :-
    ( if To >= 0 then
        successors(Graph, To, Froms),
        set.to_sorted_list(Froms, FromList),
        add_arcs_to(FromList, To, InvGraph0, InvGraph1),
        inverse_2(To - 1, Graph, InvGraph1, InvGraph)
    else
        InvGraph = InvGraph0
    ).

:- pred add_arcs_to(list(int)::in, int::in, graph::in, graph::out) is det.

add_arcs_to([], _, Graph, Graph).
add_arcs_to([From | FromList], To, Graph0, Graph) :-
    add_arc(Graph0, From, To, Graph1),
    add_arcs_to(FromList, To, Graph1, Graph).

%---------------------------------------------------------------------------%

% Predicates to use in debugging.

:- pred write_graph(graph::in, io::di, io::uo) is det.

write_graph(Graph, !IO) :-
    Graph = graph(Size, Array),
    io.format("graph size: %d\n", [i(Size)], !IO),
    write_graph_nodes(0, Size, Array, !IO).

:- pred write_graph_nodes(int::in, int::in, array(set(int))::in,
    io::di, io::uo) is det.

write_graph_nodes(Cur, Max, Array, !IO) :-
    ( if Cur =< Max then
        io.format("%d -> ", [i(Cur)], !IO),
        array.lookup(Array, Cur, SuccSet),
        set.to_sorted_list(SuccSet, Succs),
        io.write_list(Succs, ", ", io.write_int, !IO),
        io.nl(!IO),
        write_graph_nodes(Cur + 1, Max, Array, !IO)
    else
        true
    ).

:- pred write_dfs(list(int)::in, io::di, io::uo) is det.

write_dfs(Dfs, !IO) :-
    io.write_list(Dfs, "\n", io.write_int, !IO).

:- pred write_cliques(list(set(int))::in, io::di, io::uo) is det.

write_cliques(Cliques, !IO) :-
    io.write_list(Cliques, "\n", io.write, !IO).

:- pred write_clique(list(int)::in, io::di, io::uo) is det.

write_clique(Nodes, !IO) :-
    io.write_list(Nodes, "\n", io.write_int, !IO).

%---------------------------------------------------------------------------%
:- end_module cliques.
%---------------------------------------------------------------------------%
