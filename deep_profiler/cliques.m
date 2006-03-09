%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002, 2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Authors: conway, zs.
%
% This module allows you build a description of a directed graph (represented
% as a set of arcs between nodes identified by dense small integers) and then
% find the strongly connected components (cliques) of that graph.
%-----------------------------------------------------------------------------%

:- module cliques.

:- interface.

:- type graph.

:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array_util.
:- import_module dense_bitset.

:- import_module array.
:- import_module int.

% :- import_module io.
% :- import_module string.
% :- import_module unsafe.

%-----------------------------------------------------------------------------%

:- type graph
    --->    graph(
                int,
                array(set(int))
            ).

:- type visit == dense_bitset.

%-----------------------------------------------------------------------------%

init(graph(1, Array)) :-
    % The initial array size doesn't really matter.
    array.init(16, set.init, Array).

add_arc(graph(Size0, Array0), From, To, Graph) :-
    ( array.in_bounds(Array0, From) ->
        array.lookup(Array0, From, Tos0),
        set.insert(Tos0, To, Tos),
        array.set(u(Array0), From, Tos, Array),
        Size = int.max(int.max(From, To), Size0),
        Graph = graph(Size, Array)
    ;
        array.size(Array0, Size),
        array.resize(u(Array0), Size * 2, init, Array1),
        add_arc(graph(Size0, Array1), From, To, Graph)
    ).

:- pred successors(graph::in, int::in, set(int)::out) is det.

successors(graph(_Size, Array), From, Tos) :-
    ( array.in_bounds(Array, From) ->
        array.lookup(Array, From, Tos)
    ;
        Tos = set.init
    ).

:- pred mklist(int::in, list(int)::in, list(int)::out) is det.

mklist(N, Acc0, Acc) :-
    ( N < 0 ->
        Acc = Acc0
    ;
        Acc1 = [N | Acc0],
        mklist(N - 1, Acc1, Acc)
    ).

% :- pragma promise_pure(topological_sort/2).

topological_sort(Graph, TSort) :-
    % impure unsafe_perform_io(io.nl),
    % impure unsafe_perform_io(io.write_string("the graph:\n")),
    % impure unsafe_perform_io(write_graph(Graph)),
    % impure unsafe_perform_io(io.nl),

    dfs_graph(Graph, Dfs),

    % impure unsafe_perform_io(io.nl),
    % impure unsafe_perform_io(io.write_string("the dfs:\n")),
    % impure unsafe_perform_io(write_dfs(Dfs)),
    % impure unsafe_perform_io(io.nl),

    inverse(Graph, InvGraph),

    % impure unsafe_perform_io(io.nl),
    % impure unsafe_perform_io(io.write_string("the inverse graph:\n")),
    % impure unsafe_perform_io(write_graph(InvGraph)),
    % impure unsafe_perform_io(io.nl),

    Visit = dense_bitset.init,
    tsort(Dfs, InvGraph, Visit, [], TSort0),
    reverse(TSort0, TSort).

    % impure unsafe_perform_io(io.nl),
    % impure unsafe_perform_io(io.write_string("the cliques:\n")),
    % impure unsafe_perform_io(write_cliques(TSort)),
    % impure unsafe_perform_io(io.nl),
    % impure unsafe_perform_io(io.nl).

:- pred tsort(list(int)::in, graph::in, visit::array_di, list(set(int))::in,
    list(set(int))::out) is det.

% :- pragma promise_pure(tsort/5).

tsort([], _InvGraph, _Visit, TSort, TSort).
tsort([Node | Nodes], InvGraph, Visit0, TSort0, TSort) :-
    % impure unsafe_perform_io(io.write_string("tsort check ")),
    % impure unsafe_perform_io(io.write_int(Node)),
    % impure unsafe_perform_io(io.nl),

    ( dense_bitset.member(Node, Visit0) ->
        % impure unsafe_perform_io(io.write_string("tsort old ")),
        % impure unsafe_perform_io(io.write_int(Node)),
        % impure unsafe_perform_io(io.nl),
        Visit1 = Visit0,
        TSort1 = TSort0
    ;
        % impure unsafe_perform_io(io.write_string("tsort new ")),
        % impure unsafe_perform_io(io.write_int(Node)),
        % impure unsafe_perform_io(io.nl),

        dfs([Node], InvGraph, Visit0, [], Visit1, CliqueList),

        % impure unsafe_perform_io(io.write_string("tsort clique ")),
        % impure unsafe_perform_io(io.write_int(Node)),
        % impure unsafe_perform_io(io.write_string(" -> ")),
        % impure unsafe_perform_io(write_clique(CliqueList)),
        % impure unsafe_perform_io(io.nl),

        set.list_to_set(CliqueList, Clique),
        TSort1 = [Clique | TSort0]
    ),
    tsort(Nodes, InvGraph, Visit1, TSort1, TSort).

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

% :- pragma promise_pure(dfs/6).

dfs([], _Graph, Visit, Dfs, Visit, Dfs).
dfs([Node | Nodes], Graph, Visit0, Dfs0, Visit, Dfs) :-
    ( dense_bitset.member(Node, Visit0) ->
        % impure unsafe_perform_io(io.write_string("dfs old ")),
        % impure unsafe_perform_io(io.write_int(Node)),
        % impure unsafe_perform_io(io.nl),

        dfs(Nodes, Graph, Visit0, Dfs0, Visit, Dfs)
    ;
        % impure unsafe_perform_io(io.write_string("dfs new ")),
        % impure unsafe_perform_io(io.write_int(Node)),
        % impure unsafe_perform_io(io.nl),

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
    ( To >= 0 ->
        successors(Graph, To, Froms),
        set.to_sorted_list(Froms, FromList),
        add_arcs_to(FromList, To, InvGraph0, InvGraph1),
        inverse_2(To - 1, Graph, InvGraph1, InvGraph)
    ;
        InvGraph = InvGraph0
    ).

:- pred add_arcs_to(list(int)::in, int::in, graph::in, graph::out) is det.

add_arcs_to([], _, Graph, Graph).
add_arcs_to([From | FromList], To, Graph0, Graph) :-
    add_arc(Graph0, From, To, Graph1),
    add_arcs_to(FromList, To, Graph1, Graph).

%-----------------------------------------------------------------------------%

% Predicates to use in debugging.

% :- pred write_graph(graph::in, io::di, io::uo)
%   is det.
%
% write_graph(Graph, !IO) :-
%   Graph = graph(Size, Array),
%   io.format("graph size: %d\n", [i(Size)], !IO),
%   write_graph_nodes(0, Size, Array, !IO).
%
% :- pred write_graph_nodes(int::in, int::in, array(set(int))::in,
%   io::di, io::uo) is det.
%
% write_graph_nodes(Cur, Max, Array, !IO) :-
%   ( Cur =< Max ->
%       io.format("%d -> ", [i(Cur)], !IO),
%       array.lookup(Array, Cur, SuccSet),
%       set.to_sorted_list(SuccSet, Succs),
%       io.write_list(Succs, ", ", io.write_int, !IO),
%       io.nl(!IO),
%       write_graph_nodes(Cur + 1, Max, Array, !IO)
%   ;
%       true
%   ).
%
% :- pred write_dfs(list(int)::in, io::di, io::uo)
%   is det.
%
% write_dfs(Dfs, !IO) :-
%   io.write_list(Dfs, "\n", io.write_int, !IO).
%
% :- pred write_cliques(list(set(int))::in, io::di, io::uo)
%   is det.
%
% write_cliques(Cliques, !IO) :-
%   io.write_list(Cliques, "\n", io.write, !IO).
%
% :- pred write_clique(list(int)::in, io::di, io::uo)
%   is det.
%
% write_clique(Nodes, !IO) :-
%   io.write_list(Nodes, "\n", io.write_int, !IO).
%
%----------------------------------------------------------------------------%
:- end_module cliques.
%----------------------------------------------------------------------------%
