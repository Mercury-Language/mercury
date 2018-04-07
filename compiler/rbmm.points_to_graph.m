%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007, 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: rbmm.points_to_graph.m.
% Main author: Quan Phan.
%
% This module defines the region points-to graph data structure and the
% operations on it.

:- module transform_hlds.rbmm.points_to_graph.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module set.

%-----------------------------------------------------------------------------%
% The region points-to graph.
%
:- type rpt_graph.
:- type rptg_nodes == map(rptg_node, rptg_node_content).
:- type rptg_edges == map(rptg_edge, rptg_edge_info).
:- type rptg_outedges == map(rptg_node, map(rptg_edge, rptg_node)).

:- func rptg_get_nodes(rpt_graph) = rptg_nodes.
:- func rptg_get_nodes_as_list(rpt_graph) = list(rptg_node).
:- func rptg_get_edges(rpt_graph) = rptg_edges.
:- func rptg_get_outedges(rpt_graph) = rptg_outedges.

:- func rptg_get_next_node_id(rpt_graph) = int.

    % rpt_graph_init(Graph) binds Graph to an empty graph containing
    % no nodes and no edges.
    %
:- func rpt_graph_init = rpt_graph.

    % rptg_get_node_content(Graph, Node) takes Graph and Node
    % and returns the information NodeContent associated with Node.
    %
    % This operation is O(lgN) for a graph containing N nodes.
    %
:- func rptg_get_node_content(rpt_graph, rptg_node) = rptg_node_content.

    % Overwrite the content of a node.
    %
:- pred rptg_set_node_content(rptg_node::in, rptg_node_content::in,
    rpt_graph::in, rpt_graph::out) is det.

    % Overwrite the allocated status of a node.
    %
:- pred rptg_set_node_is_allocated(rptg_node::in, bool::in,
    rpt_graph::in, rpt_graph::out) is det.

    % rptg_add_node(NodeInfo, Node, OldGraph, NewGraph) takes OldGraph and
    % NodeInfo that is the information to be stored in a new node, and
    % returns a key "Node" which refers to that node, and the new graph
    % NewGraph containing all of the nodes and edges in OldGraph as well as
    % the new node. It is possible to have two nodes in the graph with the
    % same content.
    %
    % This operation is O(lgN) for a graph containing N nodes.
    %
:- pred rptg_add_node(rptg_node_content::in, rptg_node::out,
    rpt_graph::in, rpt_graph::out) is det.

    % rptg_get_edge_contents(Graph, Edge, Start, End, EdgeInfo) takes
    % Graph and Edge and returns the start and end nodes and the
    % content associated with Edge.
    %
:- pred rptg_get_edge_contents(rpt_graph::in, rptg_edge::in, rptg_node::out,
    rptg_node::out, rptg_edge_content::out) is det.

    % rptg_set_edge(Start, End, EdgeContent, Edge, OldGraph, NewGraph)
    % takes a graph OldGraph and adds an edge from Start to End with
    % the information EdgeContent, and returns the edge as Edge
    % and the new graph as NewGraph.
    % If an identical edge already exists then this operation has no effect.
    %
    % This operation is O(lgN+lgM) for a graph with N nodes and M edges.
    %
:- pred rptg_set_edge(rptg_node::in, rptg_node::in, rptg_edge_content::in,
    rptg_edge::out, rpt_graph::in, rpt_graph::out) is det.

    % rptg_successors(Graph, Node) takes a graph Graph and a node Node
    % and returns the set of nodes Nodes that are *directly* reachable
    % (not transitively) from Node.
    %
    % This operation is O(NlgN) for a graph containing N nodes.
    %
:- func rptg_successors(rpt_graph, rptg_node) = set(rptg_node).

    % rptg_path(Graph, Start, End, Path) is true iff there is a path
    % from the node Start to the node End in Graph.
    % When succeed it returns the path as Path, a list of edges.
    %
    % The algorithm will return paths containing at most one cycle.
    %
:- pred rptg_path(rpt_graph, rptg_node, rptg_node, list(rptg_edge)).
:- mode rptg_path(in, in, in, out) is nondet.
:- mode rptg_path(in, in, out, out) is nondet.

    % rptg_reachable_and_having_type(Graph, Start, EType, Node)
    % finds a node that is reachable from Start and has type EType.
    % If not found, fails.
    %
:- pred rptg_reachable_and_having_type(rpt_graph::in, rptg_node::in,
    mer_type::in, rptg_node::out) is semidet.

    % Get a node given the region name (region variable) assigned to it.
    % There is one and only one node with a given region name.
    % Therefore the predicate returns the node as soon as it finds.
    %
:- pred rptg_get_node_by_region_name(rpt_graph::in, string::in,
    rptg_node::out) is det.

    % Get a node given a set of Mercury variables assigned to it.
    % There is one and only one node corresponding to a set of variables.
    % Therefore the predicate returns the node as soon as it finds.
    %
:- pred rptg_get_node_by_vars(rpt_graph::in, set(prog_var)::in,
    rptg_node::out) is det.

    % Get a node given a Mercury variable assigned to it.
    % There is one and only one node of a variable.
    % Therefore the predicate returns the node as soon as it finds.
    %
:- pred rptg_get_node_by_variable(rpt_graph::in, prog_var::in,
    rptg_node::out) is det.

    % Get a node given a node that has been merged into the first one.
    %
:- pred rptg_get_node_by_node(rpt_graph::in, rptg_node::in, rptg_node::out)
    is det.

    % Compare two graphs.
    %
:- pred rptg_equal(rpt_graph::in, rpt_graph::in) is semidet.

    % This predicate finds all regions that are reachable from X.
    % The regions must be reached by edges with labels (type selectors)
    % which are valid with the type of X.
    %
:- pred rptg_reach_from_a_variable(rpt_graph::in, module_info::in,
    proc_info::in, prog_var::in, set(rptg_node)::in, set(rptg_node)::out)
    is det.

    % rptg_find_edge_from_node_with_same_content(N, EdgeContent, Graph, M)
    % finds in Graph, an edge that has the given EdgeContent.
    % If found, it returns the node which the edge points to as M.
    % Fails if no such an edge exists.
    %
    % Note: this predicate is used when we do not know the end node. If we
    % know the start node, the label and the end node, we may want to use
    % rptg_edge_in_graph instead.
    %
:- pred rptg_find_edge_from_node_with_same_content(rptg_node::in,
    rptg_edge_content::in, rpt_graph::in, rptg_node::out) is semidet.

    % Check if an edge (Start, Label, End) is in the Graph or not.
    %
:- pred rptg_edge_in_graph(rptg_node::in, rptg_edge_content::in, rptg_node::in,
    rpt_graph::in) is semidet.

:- func rptg_lookup_region_name(rpt_graph, rptg_node) = string.
:- func rptg_lookup_node_type(rpt_graph, rptg_node) = mer_type.
:- func rptg_lookup_node_vars(rpt_graph, rptg_node) = set(prog_var).
:- func rptg_lookup_node_is_allocated(rpt_graph, rptg_node) = bool.

:- pred rptg_is_allocated_node(rpt_graph::in, rptg_node::in) is semidet.

    % Return the list of edges (edge id's).
    %
:- func rptg_lookup_list_outedges(rpt_graph, rptg_node) = list(rptg_edge).

    % Return the list of nodes reached directly from a node.
    %
:- func rptg_lookup_list_endnodes(rpt_graph, rptg_node) = list(rptg_node).

    % Return the outedges map.
    %
:- func rptg_lookup_map_outedges(rpt_graph, rptg_node) =
    map(rptg_edge, rptg_node).

    % The unify operator.
    % We merge the second node into the first one.
    %
:- pred unify_operator(rptg_node::in, rptg_node::in,
    rpt_graph::in, rpt_graph::out) is det.

    % The edge operator.
    %
:- pred edge_operator(rptg_node::in, rptg_node::in, rptg_edge_content::in,
    rpt_graph::in, rpt_graph::out) is det.

%-----------------------------------------------------------------------------%
% A node in region points-to graphs.
%
:- type rptg_node
        --->    rptg_node(int).

:- type rptg_node_content
        --->    rptg_node_content(
                    % The set of procedure variables assigned to this node.
                    rptg_nc_vars            :: set(prog_var),

                    % The region variable that names this node.
                    rptg_nc_reg_var_name    :: string,

                    rptg_nc_merged_from     :: set(rptg_node),
                    rptg_nc_node_type       :: mer_type,
                    rptg_nc_is_allocated    :: bool
                ).

:- func rptg_node_content_get_vars(rptg_node_content) = set(prog_var).
:- func rptg_node_content_get_region_name(rptg_node_content) = string.
:- func rptg_node_content_get_merged_from(rptg_node_content) = set(rptg_node).
:- func rptg_node_content_get_node_type(rptg_node_content) = mer_type.
:- func rptg_node_content_get_is_allocated(rptg_node_content) = bool.

:- pred rptg_node_content_set_vars(set(prog_var)::in,
    rptg_node_content::in, rptg_node_content::out) is det.
:- pred rptg_node_content_set_region_name(string::in,
    rptg_node_content::in, rptg_node_content::out) is det.
:- pred rptg_node_content_set_merged_from(set(rptg_node)::in,
    rptg_node_content::in, rptg_node_content::out) is det.
:- pred rptg_node_content_set_node_type(mer_type::in,
    rptg_node_content::in, rptg_node_content::out) is det.
:- pred rptg_node_content_set_is_allocated(bool::in,
    rptg_node_content::in, rptg_node_content::out) is det.

%-----------------------------------------------------------------------------%
% An edge in region points-to graphs.
%
:- type rptg_edge
        --->    rptg_edge(int).

:- type rptg_edge_content
        --->    rptg_edge_content(
                    rptg_ec_label     :: selector
                    % The label of an edge.
                ).

:- func rptg_edge_content_get_label(rptg_edge_content) = selector.
:- pred rptg_edge_content_set_label(selector::in,
    rptg_edge_content::in, rptg_edge_content::out) is det.

:- type rptg_edge_info
        --->    rptg_edge_info(
                    rptg_edge_from_node     :: rptg_node,
                    rptg_edge_to_node       :: rptg_node,
                    rptg_edge_label         :: rptg_edge_content
                ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.vartypes.
:- import_module transform_hlds.smm_common.

:- import_module assoc_list.
:- import_module counter.
:- import_module pair.
:- import_module require.
:- import_module solutions.
:- import_module term.

    % A region points-to graph (rpt_graph) is a directed graph in which
    % 1. Each node is associated with some pieces of info represented by
    %    rptg_node_content. The set of nodes is presented as a map
    %    from rptg_node, which is the node id, to rptg_node_content.
    % 2. Each edge is a tuple (start node, label, end node) that is represented
    %    by rtpg_edge_info. The set of edges is presented as a map
    %    from rptg_edge, which is the edge id, to rptg_edge_info.
    %    We refer to label as some information associated with an edge. We
    %    represent it by rptg_edge_content.
    %
    % Two above maps (for sets of nodes and edges) store enough *information*
    % about an rpt graph. But in terms of *accessibility* it is unconvenient.
    % We often access an rpt graph from a node, and then follow
    % the node's outedges to other nodes, then from those nodes through their
    % outedges to other nodes, and so on. To facilitate this, we maintain
    % (redundantly) the outedges of a node. This is represented by
    % a map from rptg_node to all of the node's outedges, which are represented
    % by a map from rptg_edge to rptg_node (end node).
    %
:- type rpt_graph
        --->    rpt_graph(
                    % The source of node ids.
                    rptg_node_supply    :: counter,

                    % The source of edge ids.
                    rptg_edge_supply    :: counter,

                    rptg_nodes          :: rptg_nodes,
                    rptg_edges          :: rptg_edges,
                    rptg_outedges       :: rptg_outedges
                ).

rpt_graph_init = Graph :-
    counter.init(1, NodeSupply),
    counter.init(1, EdgeSupply),
    map.init(Nodes),
    map.init(Edges),
    map.init(OutEdges),
    Graph = rpt_graph(NodeSupply, EdgeSupply, Nodes, Edges, OutEdges).

:- func rptg_get_node_supply(rpt_graph) = counter.

rptg_get_node_supply(G) = G ^ rptg_node_supply.

:- pred rptg_set_node_supply(counter::in, rpt_graph::in, rpt_graph::out)is det.

rptg_set_node_supply(NS, !G) :-
    !G ^ rptg_node_supply := NS.

:- func rptg_get_edge_supply(rpt_graph) = counter.

rptg_get_edge_supply(G) = G ^ rptg_edge_supply.

:- pred rptg_set_edge_supply(counter::in,
    rpt_graph::in, rpt_graph::out) is det.

rptg_set_edge_supply(ES, !G) :-
    !G ^ rptg_edge_supply := ES.

rptg_get_nodes(G) = G ^ rptg_nodes.
rptg_get_edges(G) = G ^ rptg_edges.
rptg_get_outedges(G) = G ^ rptg_outedges.

rptg_get_next_node_id(G) = NextNodeId :-
    NodeSupply = rptg_get_node_supply(G),
    counter.allocate(NextNodeId, NodeSupply, _).

:- pred rptg_set_nodes(rptg_nodes::in, rpt_graph::in, rpt_graph::out) is det.

rptg_set_nodes(Nodes, !G) :-
    !G ^ rptg_nodes := Nodes.

:- pred rptg_set_edges(rptg_edges::in, rpt_graph::in, rpt_graph::out) is det.

rptg_set_edges(Edges, !G) :-
    !G ^ rptg_edges := Edges.

:- pred rptg_set_outedges(rptg_outedges::in, rpt_graph::in, rpt_graph::out)
    is det.

rptg_set_outedges(OutEdges, !G) :-
    !G ^ rptg_outedges := OutEdges.

    % After adding a node, we need to update Content0 so that the merged_from
    % set of the node contains itself.
    % Doing it this way is not completely satisfied because we are adding a
    % node with the given content but we change the content after all.
    % But without adding the node first, the node is nonexistant and we
    % cannot add it to the merged_from set.
    %
rptg_add_node(Content0, rptg_node(NodeId), !G) :-
    NS0 = rptg_get_node_supply(!.G),
    counter.allocate(NodeId, NS0, NS),
    rptg_set_node_supply(NS, !G),
    Node = rptg_node(NodeId),

    % Make the merged_from set contain this node.
    MergedFrom = set.make_singleton_set(Node),
    rptg_node_content_set_merged_from(MergedFrom, Content0, Content),

    % Add the node.
    NodeMap0 = !.G ^ rptg_nodes,
    map.set(Node, Content, NodeMap0, NodeMap),
    rptg_set_nodes(NodeMap, !G),

    % We can assume there is no outedge for this node yet.
    OutEdges0 = rptg_get_outedges(!.G),
    map.set(Node, map.init, OutEdges0, OutEdges),
    rptg_set_outedges(OutEdges, !G).

rptg_get_node_content(Graph, Node) = NodeContent :-
    map.lookup(rptg_get_nodes(Graph), Node, NodeContent).

rptg_get_nodes_as_list(Graph) = NodeList :-
    map.keys(rptg_get_nodes(Graph), NodeList).

rptg_successors(Graph, Node) = Successors :-
    SuccessorList = rptg_lookup_list_endnodes(Graph, Node),
    set.list_to_set(SuccessorList, Successors).

rptg_set_edge(Start, End, EdgeContent, Edge, !G) :-
    ES0 = rptg_get_edge_supply(!.G),
    counter.allocate(EdgeId, ES0, ES),
    rptg_set_edge_supply(ES, !G),

    Edge = rptg_edge(EdgeId),

    Edges0 = rptg_get_edges(!.G),
    map.set(Edge, rptg_edge_info(Start, End, EdgeContent), Edges0, Edges),
    rptg_set_edges(Edges, !G),

    % Update the outedges of the Start node.
    OutEdges0 = rptg_get_outedges(!.G),
    map.lookup(OutEdges0, Start, StartOutEdges0),
    map.set(Edge, End, StartOutEdges0, StartOutEdges),
    map.set(Start, StartOutEdges, OutEdges0, OutEdges),
    rptg_set_outedges(OutEdges, !G).

rptg_get_edge_contents(G, Edge, Start, End, Content) :-
    Edges = rptg_get_edges(G),
    map.lookup(Edges, Edge, EdgeInfo),
    EdgeInfo = rptg_edge_info(Start, End, Content).

rptg_path(G, S, E, Path) :-
    rptg_path_2(G, S, E, [], Path).

:- pred rptg_path_2(rpt_graph, rptg_node, rptg_node, list(rptg_node),
    list(rptg_edge)).
:- mode rptg_path_2(in, in, in, in, out) is nondet.
:- mode rptg_path_2(in, in, out, in, out) is nondet.

rptg_path_2(G, S, E, Nodes0, Path) :-
    OutEdges = rptg_get_outedges(G),
    map.lookup(OutEdges, S, OutEdgesOfS),
    (
        map.member(OutEdgesOfS, Edge, E),
        \+ list.member(E, Nodes0),
        Path = [Edge]
    ;
        map.member(OutEdgesOfS, Edge, N),
        \+ list.member(N, Nodes0),
        rptg_path_2(G, N, E, [N | Nodes0], Path0),
        Path = [Edge | Path0]
    ).

rptg_reachable_and_having_type(Graph, Start, EType, E) :-
    rptg_lookup_node_type(Graph, Start) = Type,
    ( if Type = EType then
        E = Start
    else
        reachable_and_having_type_2(Graph, [Start], [Start], EType, E)
    ).

    % This implementation uses breath-first search. It ensures that each node
    % becomes "Start" node at most once, therefore it will terminate.
    %
:- pred reachable_and_having_type_2(rpt_graph::in, list(rptg_node)::in,
    list(rptg_node)::in, mer_type::in, rptg_node::out) is semidet.

reachable_and_having_type_2(Graph, [StartNode | StartNodes0], VisitedNodes0,
        EType, E) :-
    Ends = rptg_lookup_list_endnodes(Graph, StartNode),
    ( if find_node_with_same_type(Ends, Graph, EType, E1) then
        % Find such a node, return it.
        E = E1
    else
        % Still not find, do breath-first search, with nodes that we have
        % never started from.
        StartNodes1 = StartNodes0 ++ Ends,
        list.remove_dups([StartNode | VisitedNodes0], VisitedNodes),
        list.delete_elems(StartNodes1, VisitedNodes, StartNodes),

        reachable_and_having_type_2(Graph, StartNodes, VisitedNodes, EType, E)
    ).

    % Find a node with the given type in the list of nodes.
    % If not found, fails.
    %
:- pred find_node_with_same_type(list(rptg_node)::in, rpt_graph::in,
    mer_type::in, rptg_node::out) is semidet.

find_node_with_same_type([N | Ns], Graph, Type, End) :-
    rptg_lookup_node_type(Graph, N) = NType,
    ( if NType = Type then
        End = N
    else
        find_node_with_same_type(Ns, Graph, Type, End)
    ).

rptg_get_node_by_region_name(Graph, RegionName, Node) :-
    AllNodes = rptg_get_nodes_as_list(Graph),
    ( if
        get_node_by_region_name_from_list(Graph, AllNodes, RegionName,
            NodePrime)
    then
        Node = NodePrime
    else
        unexpected($pred, "node not found")
    ).

:- pred get_node_by_region_name_from_list(rpt_graph::in, list(rptg_node)::in,
    string::in, rptg_node::out) is semidet.

get_node_by_region_name_from_list(Graph, NodeList, RegName, Node) :-
    NodeList = [ANode | Rest],
    RegionANode = rptg_lookup_region_name(Graph, ANode),
    ( if RegionANode = RegName then
        Node = ANode
    else
        get_node_by_region_name_from_list(Graph, Rest, RegName, Node)
    ).

rptg_get_node_by_vars(Graph, Vars, Node) :-
    Nodes = rptg_get_nodes_as_list(Graph),
    ( if get_node_by_vars_from_list(Graph, Nodes, Vars, NodePrime) then
        Node = NodePrime
    else
        unexpected($pred, "node not found")
    ).

:- pred get_node_by_vars_from_list(rpt_graph::in, list(rptg_node)::in,
    set(prog_var)::in, rptg_node::out) is semidet.

get_node_by_vars_from_list(Graph, List, Vars, Node) :-
    List = [ANode | Rest],
    NodeContent = rptg_get_node_content(Graph, ANode),
    ( set.subset(Vars, NodeContent ^ rptg_nc_vars) ->
        Node = ANode
    ;
        get_node_by_vars_from_list(Graph, Rest, Vars, Node)
    ).

    % Find a node in the graph using a variable assigned to it.
    %
rptg_get_node_by_variable(Graph, Var, Node) :-
    Vars = set.make_singleton_set(Var),
    rptg_get_node_by_vars(Graph, Vars, Node).

rptg_get_node_by_node(Graph, Node, MergedNode) :-
    NodeMap = rptg_get_nodes(Graph),
    ( map.search(NodeMap, Node, _NodeContent) ->
        MergedNode = Node
    ;
        % Not directly in the NodeMap, checked if it has been merged.
        AllNodes = rptg_get_nodes_as_list(Graph),
        ( get_node_by_node_from_list(Graph, AllNodes, Node, MergedNode0) ->
            MergedNode = MergedNode0
        ;
            unexpected($pred, "node not found")
        )
    ).

:- pred get_node_by_node_from_list(rpt_graph::in, list(rptg_node)::in,
    rptg_node::in, rptg_node::out) is semidet.

get_node_by_node_from_list(Graph, [N | Ns], Node, MergedNode) :-
    NodeContent = rptg_get_node_content(Graph, N),
    ( set.member(Node, NodeContent ^ rptg_nc_merged_from) ->
        MergedNode = N
    ;
        get_node_by_node_from_list(Graph, Ns, Node, MergedNode)
    ).

rptg_lookup_region_name(Graph, Node) = RegionName :-
    NodeContent = rptg_get_node_content(Graph, Node),
    RegionName = rptg_node_content_get_region_name(NodeContent).

rptg_lookup_node_type(Graph, Node) = NodeType :-
    NodeContent = rptg_get_node_content(Graph, Node),
    NodeType = rptg_node_content_get_node_type(NodeContent).

rptg_lookup_node_vars(Graph, Node) = Vars :-
    NodeContent = rptg_get_node_content(Graph, Node),
    Vars = rptg_node_content_get_vars(NodeContent).

rptg_lookup_node_is_allocated(Graph, Node) = IsAllocated :-
    NodeContent = rptg_get_node_content(Graph, Node),
    IsAllocated = rptg_node_content_get_is_allocated(NodeContent).

rptg_is_allocated_node(Graph, Region) :-
    IsAlloc = rptg_lookup_node_is_allocated(Graph, Region),
    IsAlloc = bool.yes.

rptg_lookup_list_outedges(Graph, Node) = EdgeList :-
    OutEdgesOfNode = rptg_lookup_map_outedges(Graph, Node),
    map.keys(OutEdgesOfNode, EdgeList).

rptg_lookup_map_outedges(Graph, Node) = OutEdgesOfNode :-
    OutEdges = rptg_get_outedges(Graph),
    map.lookup(OutEdges, Node, OutEdgesOfNode).

rptg_lookup_list_endnodes(Graph, Node) = EndNodeList :-
    OutEdgesOfNode = rptg_lookup_map_outedges(Graph, Node),
    map.values(OutEdgesOfNode, EndNodeList).

rptg_set_node_content(Node, NodeContent, !Graph) :-
    Nodes0 = rptg_get_nodes(!.Graph),
    map.det_update(Node, NodeContent, Nodes0, Nodes),
    rptg_set_nodes(Nodes, !Graph).

rptg_set_node_is_allocated(Node, IsAlloc, !Graph) :-
    NodeContent0 = rptg_get_node_content(!.Graph, Node),
    rptg_node_content_set_is_allocated(IsAlloc, NodeContent0, NodeContent),
    rptg_set_node_content(Node, NodeContent, !Graph).

rptg_node_content_get_vars(NC) = NC ^ rptg_nc_vars.
rptg_node_content_get_region_name(NC) = NC ^ rptg_nc_reg_var_name.
rptg_node_content_get_merged_from(NC) = NC ^ rptg_nc_merged_from.
rptg_node_content_get_node_type(NC) = NC ^ rptg_nc_node_type.
rptg_node_content_get_is_allocated(NC) = NC ^ rptg_nc_is_allocated.

rptg_node_content_set_vars(Vars, !NC) :-
    !NC ^ rptg_nc_vars := Vars.
rptg_node_content_set_region_name(Name, !NC) :-
    !NC ^ rptg_nc_reg_var_name := Name.
rptg_node_content_set_merged_from(Nodes, !NC) :-
    !NC ^ rptg_nc_merged_from := Nodes.
rptg_node_content_set_node_type(NodeType, !NC) :-
    !NC ^ rptg_nc_node_type := NodeType.
rptg_node_content_set_is_allocated(IsAllocated, !NC) :-
    !NC ^ rptg_nc_is_allocated := IsAllocated.

rptg_edge_content_get_label(AC) = AC ^ rptg_ec_label.
rptg_edge_content_set_label(Label, !AC) :-
    !AC ^ rptg_ec_label := Label.

%-----------------------------------------------------------------------------%
%
% The two graph-manipulating operators, i.e., unify and edge.
%

unify_operator(Node1, Node2, !Graph) :-
    Nodes0 = rptg_get_nodes(!.Graph),
    NodeContent1 = rptg_get_node_content(!.Graph, Node1),
    NodeContent2 = rptg_get_node_content(!.Graph, Node2),

    % The vars need to be unioned.
    set.union(NodeContent1 ^ rptg_nc_vars, NodeContent2 ^ rptg_nc_vars,
        UnionVars),
    rptg_node_content_set_vars(UnionVars, NodeContent1, NewContent0),

    % Union the merged_from sets.
    set.union(NodeContent1 ^ rptg_nc_merged_from,
        NodeContent2 ^ rptg_nc_merged_from, UnionMergedFrom),
    rptg_node_content_set_merged_from(UnionMergedFrom,
        NewContent0, NewContent1),

    % The unified node is marked allocated if
    % at least one of them is allocated.
    IsAllocated = bool.or(NodeContent1 ^ rptg_nc_is_allocated,
        NodeContent2 ^ rptg_nc_is_allocated),
    rptg_node_content_set_is_allocated(IsAllocated,
        NewContent1, NewContent),

    map.det_update(Node1, NewContent, Nodes0, Nodes),

    !Graph ^ rptg_nodes := Nodes,

    % Copy all out-edges of node 2 to node 1.
    transfer_out_edges(Node1, Node2, !Graph),

    % Copy all in-edges of node 2 to node 1.
    transfer_in_edges(Node1, Node2, !Graph),

    % Remove node 2.
    delete_node(Node2, !Graph).

    % This predicate receives a graph and returns a new graph in which
    % for all the out-edges of node2 in the first graph are copied to
    % node1, as out-edges of node1.
    %
:- pred transfer_out_edges(rptg_node::in, rptg_node::in, rpt_graph::in,
    rpt_graph::out) is det.

transfer_out_edges(Node1, Node2, !Graph) :-
    % Out-edges from node 2.
    EdgeList = rptg_lookup_list_outedges(!.Graph, Node2),

    % Transfer them to node 1.
    transfer_out_edges_2(EdgeList, Node1, !Graph).

    % This predicate receives a list of out-edges of node2 and returns a
    % graph with all the edges in the list copied to Node1, but it
    % maintains the invariant that "there is only one edge with a
    % specific label from a specific node to another specific node".
    % The algorithm is as follows.
    % for each edge (Node2, Content, Node) in EdgeList:
    %   if (Node1, Content, Node) exists
    %       ignore the edge.
    %   else
    %       copy the edge to Node1.
    %
:- pred transfer_out_edges_2(list(rptg_edge)::in, rptg_node::in,
    rpt_graph::in, rpt_graph::out) is det.

transfer_out_edges_2([], _, !Graph).
transfer_out_edges_2([Edge | Edges], Node1, !Graph) :-
    rptg_get_edge_contents(!.Graph, Edge, _Node2, Node, EdgeContent),
    ( rptg_edge_in_graph(Node1, EdgeContent, Node, !.Graph) ->
        true
    ;
        % Not existed, copy the Edge as an out-edge of Node1.
        rptg_set_edge(Node1, Node, EdgeContent, _Edge, !Graph)
    ),
    transfer_out_edges_2(Edges, Node1, !Graph).

    % This predicate receives a graph and returns a new graph in which
    % all the in-edges of node2 in the first graph are copied as in-edges
    % of node1.
    %
:- pred transfer_in_edges(rptg_node::in, rptg_node::in, rpt_graph::in,
    rpt_graph::out) is det.

transfer_in_edges(Node1, Node2, !Graph) :-
    % In-edges of node 2.
    rptg_get_in_edges(!.Graph, Node2, InEdges),

    % Copy them to node 1.
    transfer_in_edges_2(InEdges, Node1, !Graph).

    % Finding incoming edges to a node is not direct as finding outcoming
    % ones, we have to scan all the edges in the graph and explicitly
    % check their ending node.
    % XXX This potentially is very inefficient. We might consider storing
    % InEdges explicitly like OutEdges.
    %
:- pred rptg_get_in_edges(rpt_graph::in, rptg_node::in, list(rptg_edge)::out)
    is det.

rptg_get_in_edges(Graph, Node, InEdges) :-
    Edges = rptg_get_edges(Graph),
    map.foldl(edge_points_to_node(Node), Edges, [], InEdges).

:- pred edge_points_to_node(rptg_node::in, rptg_edge::in, rptg_edge_info::in,
    list(rptg_edge)::in, list(rptg_edge)::out) is det.

edge_points_to_node(End, Edge, EdgeInfo, !L) :-
    EdgeInfo = rptg_edge_info(_S, E, _C),
    ( E = End ->
        !:L = [Edge | !.L]
    ;
        true
    ).

    % This predicate is very similar to transfer_out_edges_2, except that
    % the edges now point to Node1, instead of going out from it.
    %
:- pred transfer_in_edges_2(list(rptg_edge)::in, rptg_node::in,
    rpt_graph::in, rpt_graph::out) is det.

transfer_in_edges_2([], _, !Graph).
transfer_in_edges_2([Edge | Edges], Node1, !Graph) :-
    rptg_get_edge_contents(!.Graph, Edge, Node, _Node2, EdgeContent),
    ( rptg_edge_in_graph(Node, EdgeContent, Node1, !.Graph) ->
        true
    ;
        % No, copy the Edge as an in-edge of Node1.
        rptg_set_edge(Node, Node1, EdgeContent, _Edge, !Graph)
    ),
    transfer_in_edges_2(Edges, Node1, !Graph).

    % Delete a node from the graph.
    % We also need to delete all the edges from and to the Node.
    %
:- pred delete_node(rptg_node::in, rpt_graph::in, rpt_graph::out) is det.

delete_node(Node,
    rpt_graph(NS, AS, !.Nodes, !.Edges, !.OutEdges),
    rpt_graph(NS, AS, !:Nodes, !:Edges, !:OutEdges)) :-
    map.delete(Node, !Nodes),
    delete_all_outedges_and_edges(Node, !Edges, !OutEdges),
    delete_all_inedges_and_edges(Node, !Edges, !OutEdges).

    % This predicate deletes all the outedges of Node.
    % Note: it works as a helper for delete_node so it does not proceed
    % on a graph but on the edges map and outedges map.
    %
:- pred delete_all_outedges_and_edges(rptg_node::in,
    rptg_edges::in, rptg_edges::out,
    rptg_outedges::in, rptg_outedges::out) is det.

delete_all_outedges_and_edges(Node, !Edges, !OutEdges) :-
    % Delete the edges themselves.
    map.lookup(!.OutEdges, Node, OutEdgesOfNode),
    map.keys(OutEdgesOfNode, TheEdges),
    map.delete_list(TheEdges, !Edges),

    % Delete the info about outcoming edges.
    map.delete(Node, !OutEdges).

    % This predicate deletes all the incoming edges of the input node (Node).
    % We only store outcoming edges therefore to remove incoming ones of Node
    % we need to check all the outcoming edges and remove those point to Node.
    %
:- pred delete_all_inedges_and_edges(rptg_node::in,
    rptg_edges::in, rptg_edges::out, rptg_outedges::in, rptg_outedges::out)
    is det.

delete_all_inedges_and_edges(Node, !Edges, !OutEdges) :-
    map.keys(!.OutEdges, StartNodes),

    % For each node: find the outcoming edges from it
    % and delete ones pointing to Node.
    delete_all_inedges_and_edges_2(StartNodes, Node, !Edges, !OutEdges).

    % This predicate receives a node (Node) and a list of (start) nodes.
    % It deletes all the start nodes' outcoming edges and corresponding edges
    % which point to the Node.
    %
:- pred delete_all_inedges_and_edges_2(list(rptg_node)::in, rptg_node::in,
    rptg_edges::in, rptg_edges::out, rptg_outedges::in, rptg_outedges::out)
    is det.

delete_all_inedges_and_edges_2([], _, !Edges, !OutEdges).
delete_all_inedges_and_edges_2([N | Ns], Node, !Edges, !OutEdges) :-
    map.lookup(!.OutEdges, N, OutEdgesOfN0),

    % Find the edges that point to Node.
    solutions(map.inverse_search(OutEdgesOfN0, Node), EdgesFromNPointToNode),

    % Delete the edges themselves.
    map.delete_list(EdgesFromNPointToNode, !Edges),

    % Delete the info about outedges.
    map.delete_list(EdgesFromNPointToNode, OutEdgesOfN0, OutEdgesOfN),
    map.set(N, OutEdgesOfN, !OutEdges),
    delete_all_inedges_and_edges_2(Ns, Node, !Edges, !OutEdges).

edge_operator(Start, End, Info, !G) :-
    rptg_set_edge(Start, End, Info, _Edge, !G).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% For finding and checking edges in graph.
%

rptg_find_edge_from_node_with_same_content(N, EdgeContent, G, M) :-
    EdgeList = rptg_lookup_list_outedges(G, N),
    find_edge_with_same_content(EdgeContent, EdgeList, G, M).

:- pred find_edge_with_same_content(rptg_edge_content::in,
    list(rptg_edge)::in, rpt_graph::in, rptg_node::out) is semidet.

find_edge_with_same_content(EdgeContent, [Edge | Edges], G, M) :-
    rptg_get_edge_contents(G, Edge, _N, M0, EdgeContent0),
    ( EdgeContent0 = EdgeContent ->
        M = M0
    ;
        find_edge_with_same_content(EdgeContent, Edges, G, M)
    ).

rptg_edge_in_graph(Start, Label, End, Graph) :-
    OutEdgesOfStart = rptg_lookup_map_outedges(Graph, Start),

    % Out of the above, find those that point to End.
    solutions(map.inverse_search(OutEdgesOfStart, End), EdgePointToEndList),

    find_edge_with_same_content(Label, EdgePointToEndList, Graph, _).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Equality of region points-to graphs.
%

rptg_equal(Graph1, Graph2) :-
    Graph1 = rpt_graph(NS1, AS1, Nodes1, Edges1, OutEdges1),
    Graph2 = rpt_graph(NS2, AS2, Nodes2, Edges2, OutEdges2),
    NS1 = NS2,
    AS1 = AS2,
    simple_map_equal(Nodes1, Nodes2),
    simple_map_equal(Edges1, Edges2),
    complex_map_equal(OutEdges1, OutEdges2).

% The comparisons below may not be necessary, unification can help if it is
% sure that the elements are added to the maps in the same order.
%
    % The values of the maps are required to be comparable using
    % unification, i.e., values of type V1 can be compared using
    % unification.
    %
:- pred simple_map_equal(map(K1, V1)::in, map(K1, V1)::in) is semidet.

simple_map_equal(Map1, Map2) :-
    % Check if they have the same number of entries?
    map.count(Map1, C1),
    map.count(Map2, C2),
    C1 = C2,

    % If yes, check if all the entries are equal.
    map.keys(Map1, Ks1),
    simple_map_equal_2(Ks1, Map1, Map2).

    % With the condition that the two maps have the same number of entries,
    % verify that all keys in map 1 are also in map 2 and that their
    % corresponding values are equal.
    %
:- pred simple_map_equal_2(list(K1)::in,
    map(K1, V1)::in, map(K1, V1)::in) is semidet.

simple_map_equal_2([], _, _).
simple_map_equal_2([K | Ks], Map1, Map2) :-
    % K is also in map 2?
    map.search(Map2, K, V2),

    % Yes, so check whether the values are equal.
    map.lookup(Map1, K, V1),
    V1 = V2,
    simple_map_equal_2(Ks, Map1, Map2).

    % The maps need to be of map-in-map structure, namely
    % map(k1, map(k2, v)) and values of type V can be compared by unifying
    % (i.e., in our notion here map(k2, v) is a "simple" map).
    %
:- pred complex_map_equal(map(K1, map(K2, V))::in, map(K1, map(K2, V))::in)
    is semidet.

complex_map_equal(Map1, Map2) :-
    map.count(Map1, C1),
    map.count(Map2, C2),
    C1 = C2,
    map.keys(Map1, Ks1),
    complex_map_equal_2(Ks1, Map1, Map2).

:- pred complex_map_equal_2(list(K1)::in, map(K1, map(K2, V))::in,
    map(K1, map(K2, V))::in) is semidet.

complex_map_equal_2([], _, _).
complex_map_equal_2([K | Ks], Map1, Map2) :-
    map.search(Map2, K, V2),

    % V2 is "simple" map, so compare it with V1.
    map.lookup(Map1, K, V1),
    simple_map_equal(V1, V2),
    complex_map_equal_2(Ks, Map1, Map2).

rptg_reach_from_a_variable(Graph, HLDS, ProcInfo, X, !Reach_X) :-
    rptg_get_node_by_variable(Graph, X, N_X),
    Node_Selector = pair(N_X, []),

    proc_info_get_vartypes(ProcInfo, VarTypes),
    lookup_var_type(VarTypes, X, TypeX),

    % Find regions reached from X.
    reach_from_a_variable_2([Node_Selector], Graph, HLDS,
        TypeX, [], !Reach_X).

    % This predicate receives a (remembered) list of nodes that are
    % reached from X, along with the valid selectors to those nodes
    % from the node of X.
    % Algorithm:
    %   1. each node is recorded into the reach_from_x set,
    %   2. if an target of a node's out-edge can be reached by a valid
    %   selector, we "remember" the target as reachable from X but not
    %   record it yet,
    %   3. do until the remembered list is empty.
    %
:- pred reach_from_a_variable_2(assoc_list(rptg_node, selector)::in,
    rpt_graph::in, module_info::in, mer_type::in, list(rptg_node)::in,
    set(rptg_node)::in, set(rptg_node)::out) is det.

reach_from_a_variable_2([], _, _, _, _, !Reach_X).
reach_from_a_variable_2([Node_Selector | Node_Selectors0],
        Graph, HLDS, TypeX, Processed0, !Reach_X) :-
    Node_Selector = Node - Selector,

    % Add the "remembered" Node to reach_from_x set
    set.insert(Node, !Reach_X),

    % Add the Node to processed list so that we do not have to deal with
    % it more than once. (Node is not yet in Processed0 because if it
    % is in there it will not be in the to-be-processed list.
    Processed = [Node | Processed0],

    % Take out-edges of the Node and update the remembered list.
    EdgeList = rptg_lookup_list_outedges(Graph, Node),
    list.foldl(
        update_remembered_list(Selector, HLDS, TypeX, Graph, Processed),
        EdgeList, Node_Selectors0, Node_Selectors),

    reach_from_a_variable_2(Node_Selectors, Graph, HLDS, TypeX,
        Processed, !Reach_X).

    % A target is remembered as reachable from X if its edge's selector
    % is valid. The remembered list is appended, so it is a breadth-first
    % process.
    %
:- pred update_remembered_list(selector::in, module_info::in,
    mer_type::in, rpt_graph::in, list(rptg_node)::in, rptg_edge::in,
    assoc_list(rptg_node, selector)::in,
    assoc_list(rptg_node, selector)::out) is det.
update_remembered_list(Selector0, HLDS, TypeX, Graph, Processed, OutEdge,
        !List) :-
    rptg_get_edge_contents(Graph, OutEdge, _Start, End, EdgeContent),
    EdgeSelector = rptg_edge_content_get_label(EdgeContent),
    Selector = Selector0 ++ EdgeSelector,
    ( check_type_of_node(HLDS, TypeX, Selector) ->
        % The edge's selector is a valid one.
        ( list.member(End, Processed) ->
            % Already processed, ignore.
            true
        ;
            % A non-processed node and can be reached from X by a
            % valid selector, so it is remembered.
            !:List = !.List ++ [pair(End, Selector)]
        )
    ;
        % Selector is not valid, ignore.
        true
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.rbmm.points_to_graph.
%-----------------------------------------------------------------------------%
