%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1999, 2003, 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% File: mc_graph.m.
% Main author: conway.
% Stability: low.
%
% This module defines a directed graph data type. The type graph(N, A)
% stores information of type N in the nodes, and information of type A
% in the arcs.
% This is a modified copy of the standard library module graph, and provides
% a reasonably large test case for the propagation solver approach to
% constraints based mode analysis to be run on.
%
%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- module mc_graph.

:- interface.

:- import_module list.
:- import_module set.
:- import_module std_util.

    % mc_graph(Node, Arc) represents a directed mc_graph with information of
    % type Node associated with each node, and information of type Arc
    % associated with each arc.
    %
:- type mc_graph(N, A).

:- type node(N).

:- type arc(A).

    % Lots of mc_graphs don't need to store anything in the arcs so here's
    % a type equivalence that only has `real' information in the nodes.
    %
:- type mc_graph(N)    == mc_graph(N, unit).

:- type arc     == arc(unit).

    % mc_graph__init(Graph) binds Graph to an empty mc_graph containing no nodes
    % and no arcs. (The mc_graph contains a counter of the number of nodes
    % allocated in it, so it is possible for a mc_graph to contain no nodes
    % or arcs and still fail to unify with the binding of Graph from
    % mc_graph__init.)
    %
:- pred mc_graph__init(mc_graph(N, A)::out) is det.
:- func mc_graph__init = mc_graph(N, A).

    % mc_graph__set_node(OldGraph, NodeInfo, Node, NewGraph) takes
    % OldGraph and NodeInfo which is the information to be stored
    % in a new node, and returns a key "Node" which refers to that
    % node, and the new mc_graph NewGraph containing all of the nodes
    % and arcs in OldGraph as well as the new node.
    % It is possible to have two nodes in the mc_graph with the
    % same information stored in them.
    %
    % This operation is O(lgN) for a mc_graph containing N nodes.
    %
:- pred mc_graph__set_node(mc_graph(N, A)::in, N::in, node(N)::out,
    mc_graph(N, A)::out) is det.

    % mc_graph__insert_node/4 is the same as mc_graph__set_node/4 except
    % that if the information to be stored in the node is stored
    % in another node, then the mc_graph__insert_node/4 fails.
    %
    % This operation is O(N) for a mc_graph containing N nodes since
    % this predicate has to check that the node data isn't in an
    % existing node.
    %
:- pred mc_graph__insert_node(mc_graph(N, A)::in, N::in, node(N)::out,
    mc_graph(N, A)::out) is semidet.

    % mc_graph__det_insert_node/4 is like mc_graph__insert_node, except
    % that if the insertion would fail, it calls error/1.
    %
:- pred mc_graph__det_insert_node(mc_graph(N, A)::in, N::in, node(N)::out,
    mc_graph(N, A)::out) is det.

    % mc_graph__search_node(Graph, NodeInfo, Node) nondeterministically
    % produces bindings of Node such that Node is a node in Graph
    % that has the information NodeInfo attatched to it.
    %
    % This operation is O(lgN) for the first solution for a mc_graph
    % containing N nodes.
    %
:- pred mc_graph__search_node(mc_graph(N, A)::in, N::in, node(N)::out) is nondet.

    % mc_graph__find_matching_nodes(Graph, NodeInfo, Nodes) takes a mc_graph
    % Graph and the information NodeInfo and returns the set of nodes
    % Nodes which have the information NodeInfo stored in them. (The set
    % Nodes will of course be empty if there are no matching nodes.)
    %
    % This operation is O(NlgN) for a mc_graph containing N nodes.
    %
:- pred mc_graph__find_matching_nodes(mc_graph(N, A)::in, N::in, set(node(N))::out)
    is det.
:- func mc_graph__find_matching_nodes(mc_graph(N, A), N) = set(node(N)).

    % mc_graph__node_contents(Graph, Node, NodeInfo) takes Graph and
    % Node and returns the information NodeInfo stored in Node.
    %
    % This operation is O(lgN) for a mc_graph containing N nodes.
    %
:- pred mc_graph__node_contents(mc_graph(N, A)::in, node(N)::in, N::out) is det.
:- func mc_graph__node_contents(mc_graph(N, A), node(N)) = N.

    % mc_graph__successors(Graph, Node, Nodes) takes a mc_graph Graph and
    % a node Node and returns the set of nodes Nodes that are reachable
    % (directly - not transitively) from Node.
    %
    % This operation is O(NlgN) for a mc_graph containing N nodes.
    %
:- pred mc_graph__successors(mc_graph(N, A)::in, node(N)::in, set(node(N))::out)
    is det.
:- func mc_graph__successors(mc_graph(N, A), node(N)) = set(node(N)).

    % mc_graph__nodes(Graph, Nodes) binds Nodes to the set of nodes in Graph.
    %
:- pred mc_graph__nodes(mc_graph(N, A)::in, set(node(N))::out) is det.
:- func mc_graph__nodes(mc_graph(N, A)) = set(node(N)).

    % mc_graph__set_edge(OldGraph, Start, End, ArcInfo, Arc, NewGraph)
    % takes a mc_graph OldGraph and adds an arc from Start to End with
    % the information ArcInfo stored in it, and returns a key for
    % that arc Arc, and the new mc_graph NewGraph.
    % If an identical arc already exists then this operation has
    % no effect.
    %
    % This operation is O(lgN+lgM) for a mc_graph with N nodes and M arcs.
    %
:- pred mc_graph__set_edge(mc_graph(N, A)::in, node(N)::in, node(N)::in, A::in,
    arc(A)::out, mc_graph(N, A)::out) is det.

    % mc_graph__insert_edge/6 is the same as mc_graph__set_edge/6 except that
    % if an identical arc already exists in the mc_graph the operation fails.
    % This is O(N) for a mc_graph with N edges between the two nodes.
    %
:- pred mc_graph__insert_edge(mc_graph(N, A)::in, node(N)::in, node(N)::in, A::in,
    arc(A)::out, mc_graph(N, A)::out) is semidet.

    % mc_graph__det_insert_edge/6 is like mc_graph__insert_edge except
    % than instead of failing, it calls error/1.
    %
:- pred mc_graph__det_insert_edge(mc_graph(N, A)::in, node(N)::in, node(N)::in,
    A::in, arc(A)::out, mc_graph(N, A)::out) is det.

    % mc_graph__arc_contents(Graph, Arc, Start, End, ArcInfo) takes a
    % mc_graph Graph and an arc Arc and returns the start and end nodes
    % and the information stored in that arc.
    %
:- pred mc_graph__arc_contents(mc_graph(N, A)::in, arc(A)::in,
    node(N)::out, node(N)::out, A::out) is det.

    % mc_graph__path(Graph, Start, End, Path) is true iff there is a path
    % from the node Start to the node End in Graph that goes through
    % the sequence of arcs Arcs.
    % The algorithm will return paths containing at most one cycle.
    %
:- pred mc_graph__path(mc_graph(N, A), node(N), node(N), list(arc(A))).
:- mode mc_graph__path(in, in, in, out) is nondet.
:- mode mc_graph__path(in, in, out, out) is nondet.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module counter.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module std_util.

:- type mc_graph(N, A)
    --->    mc_graph(
                node_supply     :: counter,
                arc_supply      :: counter,
                node_map        :: map(node(N), N),
                arc_map         :: map(arc(A), arc_info(N, A)),
                edge_map        :: map(node(N), map(arc(A), node(N)))
            ).

:- type node(N)
    --->    node(int).

:- type arc(A)
    --->    arc(int).

:- type arc_info(N, A)
    --->    arc_info(node(N), node(N), A).

%------------------------------------------------------------------------------%

mc_graph__init(Graph) :-
    Graph = mc_graph(counter__init(0), counter__init(0), Nodes, Arcs, Edges),
    map__init(Nodes),
    map__init(Arcs),
    map__init(Edges).

%------------------------------------------------------------------------------%

mc_graph__set_node(!.G, NInfo, node(N), !:G) :-
    NS0 = !.G ^ node_supply,
    counter__allocate(N, NS0, NS),
    !:G = !.G ^ node_supply := NS,

    Nodes0 = !.G ^ node_map,
    map__set(Nodes0, node(N), NInfo, Nodes),
    !:G = !.G ^ node_map := Nodes,

    Edges0 = !.G ^ edge_map,
    map__init(EdgeMap),
    map__set(Edges0, node(N), EdgeMap, Edges),
    !:G = !.G ^ edge_map := Edges.

mc_graph__det_insert_node(!.G, NInfo, N, !:G) :-
    ( mc_graph__insert_node(!.G, NInfo, NPrime, !:G) ->
        N = NPrime
    ;
        error("mc_graph__det_insert_node: node already exists.")
    ).

mc_graph__insert_node(!.G, NInfo, node(N), !:G) :-
    % Make sure that the mc_graph doesn't contain NInfo already.
    \+ map__member(!.G ^ node_map, _, NInfo),

    NS0 = !.G ^ node_supply,
    counter__allocate(N, NS0, NS),
    !:G = !.G ^ node_supply := NS,

    Nodes0 = !.G ^ node_map,
    map__set(Nodes0, node(N), NInfo, Nodes),
    !:G = !.G ^ node_map := Nodes,

    Edges0 = !.G ^ edge_map,
    map__init(EdgeSet),
    map__set(Edges0, node(N), EdgeSet, Edges),
    !:G = !.G ^ edge_map := Edges.

%------------------------------------------------------------------------------%

mc_graph__search_node(Graph, NodeInfo, Node) :-
    NodeTable = Graph ^ node_map,
    map__member(NodeTable, Node, NodeInfo).

%------------------------------------------------------------------------------%

mc_graph__find_matching_nodes(Graph, NodeInfo, NodeSet) :-
    NodeTable = Graph ^ node_map,
%   Higher order code removed here
%   solutions(mc_graph__select_node(NodeTable, NodeInfo), NodeList),
    map.sorted_keys(NodeTable, AllNodes),
    filter_node_info(NodeTable, NodeInfo, AllNodes, MatchingNodes),
    set__sorted_list_to_set(MatchingNodes, NodeSet).

    % filter_node_info(NodeTable, NodeInfo, Nodes, FilteredNodes)
    %
    % Succeeds where FilterNodes are those elements of Nodes for
    % which the node info for them in NodeTable matches NodeInfo.
    %
:- pred filter_node_info(map(node(N), N)::in, N::in, list(node(N))::in,
    list(node(N))::out) is det.

filter_node_info(_, _, [], []).
filter_node_info(NodeTable, NodeInfo, [Node | Nodes], FilteredNodes) :-
    filter_node_info(NodeTable, NodeInfo, Nodes, FilteredNodes0),
    ( map.search(NodeTable, Node, NodeInfo) ->
        FilteredNodes = [Node | FilteredNodes0]
    ;
        FilteredNodes = FilteredNodes0
    ).

:- pred mc_graph__select_node(map(node(N), N)::in, N::in, node(N)::out) is nondet.

mc_graph__select_node(NodeTable, NodeInfo, Node) :-
    map__member(NodeTable, Node, NodeInfo).

%------------------------------------------------------------------------------%

mc_graph__node_contents(G, N, I) :-
    map__lookup(G ^ node_map, N, I).

%------------------------------------------------------------------------------%

mc_graph__successors(G, N, Ss) :-
    map__lookup(G ^ edge_map, N, E),
    map__values(E, SsList),
    set__list_to_set(SsList, Ss).

%------------------------------------------------------------------------------%

mc_graph__nodes(G, Ns) :-
    map__keys(G ^ node_map, Ns1),
    set__list_to_set(Ns1, Ns).

%------------------------------------------------------------------------------%

mc_graph__set_edge(!.G, Start, End, Info, Arc, !:G) :-
    AS0 = !.G ^ arc_supply,
    counter__allocate(A, AS0, AS),
    Arc = arc(A),
    !:G = !.G ^ arc_supply := AS,

    Arcs0 = !.G ^ arc_map,
    map__set(Arcs0, Arc, arc_info(Start, End, Info), Arcs),
    !:G = !.G ^ arc_map := Arcs,

    Es0 = !.G ^ edge_map,
    map__lookup(Es0, Start, EdgeMap0),
    map__set(EdgeMap0, Arc, End, EdgeMap),
    map__set(Es0, Start, EdgeMap, Es),
    !:G = !.G ^ edge_map := Es.

%------------------------------------------------------------------------------%

mc_graph__det_insert_edge(!.G, Start, End, Info, Arc, !:G) :-
    ( mc_graph__insert_edge(!.G, Start, End, Info, ArcPrime, !:G) ->
        Arc = ArcPrime
    ;
        error("mc_graph__det_insert_edge: this edge is already in the mc_graph.")
    ).

mc_graph__insert_edge(!.G, Start, End, Info, Arc, !:G) :-
    AS0 = !.G ^ arc_supply,
    counter__allocate(A, AS0, AS),
    Arc = arc(A),
    !:G = !.G ^ arc_supply := AS,

    Arcs0 = !.G ^ arc_map,
    map__insert(Arcs0, Arc, arc_info(Start, End, Info), Arcs),
    !:G = !.G ^ arc_map := Arcs,

    Es0 = !.G ^ edge_map,
    map__lookup(Es0, Start, EdgeMap0),
    map__set(EdgeMap0, Arc, End, EdgeMap),
    map__set(Es0, Start, EdgeMap, Es),
    !:G = !.G ^ edge_map := Es.

%------------------------------------------------------------------------------%

mc_graph__arc_contents(G, N, S, E, A) :-
    map__lookup(G ^ arc_map, N, I),
    I = arc_info(S, E, A).

%------------------------------------------------------------------------------%

mc_graph__path(G, S, E, Path) :-
    mc_graph__path_2(G, S, E, [], Path).

:- pred mc_graph__path_2(mc_graph(N, A), node(N), node(N),
    list(node(N)), list(arc(A))).
:- mode mc_graph__path_2(in, in, in, in, out) is nondet.
:- mode mc_graph__path_2(in, in, out, in, out) is nondet.

mc_graph__path_2(G, S, E, Nodes0, Path) :-
    Es = G ^ edge_map,
    map__lookup(Es, S, Arcs),
    (
        map__member(Arcs, A, E),
        \+ list__member(E, Nodes0),
        Path = [A]
    ;
        map__member(Arcs, A, N),
        \+ list__member(N, Nodes0),
        mc_graph__path_2(G, N, E, [N | Nodes0], Path0),
        Path = [A | Path0]
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
%       Functional forms added.

mc_graph__init = G :-
    mc_graph__init(G).

mc_graph__find_matching_nodes(G, N) = S :-
    mc_graph__find_matching_nodes(G, N, S).

mc_graph__node_contents(G, N) = NI :-
    mc_graph__node_contents(G, N, NI).

mc_graph__successors(G, N) = S :-
    mc_graph__successors(G, N, S).

mc_graph__nodes(G) = S :-
    mc_graph__nodes(G,S).
