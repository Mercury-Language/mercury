%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 The University of Melbourne.
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

:- import_module int.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module string.

:- type rpt_graph.

:- func rptg_get_nodemap(rpt_graph) = map(rptg_node, rptg_node_content).
:- func rptg_get_arcmap(rpt_graph) = map(rptg_arc, rptg_arc_info).
:- func rptg_get_edgemap(rpt_graph) = map(rptg_node, map(rptg_arc, rptg_node)).

:- pred rptg_set_nodemap(map(rptg_node, rptg_node_content)::in,
    rpt_graph::in, rpt_graph::out) is det.
:- pred rptg_set_arcmap(map(rptg_arc, rptg_arc_info)::in,
    rpt_graph::in, rpt_graph::out) is det.
:- pred rptg_set_edgemap(map(rptg_node, map(rptg_arc, rptg_node))::in,
    rpt_graph::in, rpt_graph::out) is det.

    % XXX: This function should be unnecessary.
:- func rptg_get_next_node_number(rpt_graph) = int.

:- type rptg_node
        --->    rptg_node(int).

:- type rptg_node_content
        --->    rptg_node_content(
                    % The set of procedure variables assigned to this node.
                    rptg_nc_vars            :: set(prog_var),

                    % The region variable that names this node.
                    rptg_nc_reg_var_name    :: string,

                    rptg_nc_merged_from     :: set(rptg_node),
                    rptg_nc_node_type       :: mer_type
                ).

:- func rptg_node_content_get_vars(rptg_node_content) = set(prog_var).
:- func rptg_node_content_get_region_name(rptg_node_content) = string.
:- func rptg_node_content_get_merged_from(rptg_node_content) = set(rptg_node).
:- func rptg_node_content_get_node_type(rptg_node_content) = mer_type.

:- pred rptg_node_content_set_vars(set(prog_var)::in,
    rptg_node_content::in, rptg_node_content::out) is det.
:- pred rptg_node_content_set_region_name(string::in,
    rptg_node_content::in, rptg_node_content::out) is det.
:- pred rptg_node_content_set_merged_from(set(rptg_node)::in,
    rptg_node_content::in, rptg_node_content::out) is det.
:- pred rptg_node_content_set_node_type(mer_type::in,
    rptg_node_content::in, rptg_node_content::out) is det.

:- type rptg_arc
        --->    rptg_arc(int).

:- type rptg_arc_content
        --->    rptg_arc_content(
                    rptg_ac_label       :: selector % the label of an edge
                ).

:- func rptg_arc_content_get_label(rptg_arc_content) = selector.
:- pred rptg_arc_content_set_label(selector::in,
    rptg_arc_content::in, rptg_arc_content::out) is det.

:- type rptg_arc_info
        --->    rptg_arc_info(
                    rptg_ai_from_node   :: rptg_node,
                    rptg_ai_to_node     :: rptg_node,
                    rptg_ai_label       :: rptg_arc_content
                ).

    % rpt_graph_init(Graph) binds Graph to an empty graph containing
    % no nodes and no arcs. (The graph contains a counter of the number
    % of nodes allocated in it, so it is possible for a graph to contain
    % no nodes or arcs and still fail to unify with the binding of Graph from
    % rpt_graph_init.)
    %
:- func rpt_graph_init = rpt_graph.

    % rptg_set_node(NodeInfo, Node, OldGraph, NewGraph) takes OldGraph and
    % NodeInfo which is the information to be stored in a new node, and
    % returns a key "Node" which refers to that node, and the new graph
    % NewGraph containing all of the nodes and arcs in OldGraph as well as
    % the new node. It is possible to have two nodes in the graph with the
    % same information stored in them.
    %
    % This operation is O(lgN) for a graph containing N nodes.
    %
:- pred rptg_set_node(rptg_node_content::in, rptg_node::out,
    rpt_graph::in, rpt_graph::out) is det.

    % rptg_node_contents(Graph, Node, NodeContent) takes Graph and Node
    % and returns the information NodeContent stored in Node.
    %
    % This operation is O(lgN) for a graph containing N nodes.
    %
:- func rptg_node_contents(rpt_graph, rptg_node) = rptg_node_content.

    % rptg_successors(Graph, Node, Nodes) takes a graph Graph and a node Node
    % and returns the set of nodes Nodes that are reachable (directly -
    % not transitively) from Node.
    %
    % This operation is O(NlgN) for a graph containing N nodes.
    %
:- func rptg_successors(rpt_graph, rptg_node) = set(rptg_node).

    % rptg_get_nodes(Graph, Nodes) binds Nodes to the set of nodes in Graph.
    %
:- func rptg_get_nodes(rpt_graph) = list(rptg_node).

    % rptg_set_edge(Start, End, ArcInfo, Arc, OldGraph, NewGraph)
    % takes a graph OldGraph and adds an arc from Start to End with
    % the information ArcInfo stored in it, and returns a key for that arc Arc,
    % and the new graph NewGraph.
    % If an identical arc already exists then this operation has no effect.
    %
    % This operation is O(lgN+lgM) for a graph with N nodes and M arcs.
    %
:- pred rptg_set_edge(rptg_node::in, rptg_node::in, rptg_arc_content::in,
    rptg_arc::out, rpt_graph::in, rpt_graph::out) is det.

    % rptg_arc_contents(Graph, Arc, Start, End, ArcInfo) takes a graph Graph
    % and an arc Arc and returns the start and end nodes and the content
    % stored in that arc.
    %
:- pred rptg_arc_contents(rpt_graph::in, rptg_arc::in, rptg_node::out,
    rptg_node::out, rptg_arc_content::out) is det.

    % rptg_path(Graph, Start, End, Path) is true iff there is a path
    % from the node Start to the node End in Graph that goes through
    % the sequence of arcs Arcs.
    % The algorithm will return paths containing at most one cycle.
    %
:- pred rptg_path(rpt_graph, rptg_node, rptg_node, list(rptg_arc)).
:- mode rptg_path(in, in, in, out) is nondet.
:- mode rptg_path(in, in, out, out) is nondet.

:- pred reachable_and_having_type(rpt_graph::in, rptg_node::in, mer_type::in,
    rptg_node::out) is semidet.

    % Get a node given the region name (region variable) assigned to it.
    % There is one and only one node with a given region name.
    % Therefore the predicate returns the node as soon as it finds.
    %
:- pred get_node_by_region_name(rpt_graph::in, string::in,
    rptg_node::out) is det.

    % Get a node given a set of Mercury variables assigned to it.
    % There is one and only one node corresponding to a set of variables.
    % Therefore the predicate returns the node as soon as it finds.
    %
:- pred get_node_by_vars(rpt_graph::in, set(prog_var)::in,
    rptg_node::out) is det.

    % Get a node given a Mercury variable assigned to it.
    % There is one and only one node of a variable.
    % Therefore the predicate returns the node as soon as it finds.
    %
:- pred get_node_by_variable(rpt_graph::in, prog_var::in,
    rptg_node::out) is det.

    % Get a node given a node that has been merged into the first one.
    %
:- pred get_node_by_node(rpt_graph::in, rptg_node::in, rptg_node::out) is det.

    % Compare two graphs.
    %
:- pred rptg_equal(rpt_graph::in, rpt_graph::in) is semidet.

    % This predicate returns a set of nodes (regions) reachable from a
    % variable X.
    %
:- pred reach_from_a_variable(rpt_graph::in, module_info::in, proc_info::in,
    prog_var::in, set(rptg_node)::in, set(rptg_node)::out) is det.

:- func rptg_lookup_region_name(rpt_graph, rptg_node) = string.
:- func rptg_lookup_node_type(rpt_graph, rptg_node) = mer_type.

    % The unify operator.
    %
:- pred unify_operator(rptg_node::in, rptg_node::in,
    rpt_graph::in, rpt_graph::out) is det.

    % The edge operator.
    %
:- pred edge_operator(rptg_node::in, rptg_node::in, rptg_arc_content::in,
    rpt_graph::in, rpt_graph::out) is det.

    % This predicate finds, in the graph, an edge which has the given label.
    % If found, it returns the node which the edge points to.
    % Fails if no such an edge exists.
    %
    % Note: this predicate is used when we do not know the end node. If we
    % know the start node, the label and the end node, we may want to use
    % edge_in_graph instead.
    %
:- pred find_arc_from_node_with_same_label(rptg_node::in,
    rptg_arc_content::in, rpt_graph::in, rptg_node::out) is semidet.

    % Check if an edge (Start, Label, End) is in the Graph or not.
    %
:- pred edge_in_graph(rptg_node::in, rptg_arc_content::in, rptg_node::in,
    rpt_graph::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.compiler_util.
:- import_module transform_hlds.smm_common.

:- import_module counter.
:- import_module assoc_list.
:- import_module pair.
:- import_module solutions.
:- import_module svmap.
:- import_module svset.
:- import_module term.

    % A region points-to graph rpt_graph(Node, Arc) represents a directed
    % graph with information type Node associated with each node, and
    % information of type Arc associated with each arc.
    %
:- type rpt_graph
        --->    rpt_graph(
                    % The source of node ids.
                    rptg_node_supply    :: counter,

                    % The source of arc ids.
                    rptg_arc_supply     :: counter,

                    rptg_nodemap        :: map(rptg_node, rptg_node_content),
                    rptg_arcmap         :: map(rptg_arc, rptg_arc_info),
                    rptg_edgemap        :: map(rptg_node,
                                                map(rptg_arc, rptg_node))
                ).

rpt_graph_init = Graph :-
    counter.init(1, NodeSupply),
    counter.init(1, ArcSupply),
    Graph = rpt_graph(NodeSupply, ArcSupply, Nodes, Arcs, Edges),
    map.init(Nodes),
    map.init(Arcs),
    map.init(Edges).

:- func rptg_get_node_supply(rpt_graph) = counter.
:- func rptg_get_arc_supply(rpt_graph) = counter.

:- pred rptg_set_node_supply(counter::in,
    rpt_graph::in, rpt_graph::out)is det.
:- pred rptg_set_arc_supply(counter::in,
    rpt_graph::in, rpt_graph::out) is det.

rptg_get_node_supply(G) = G ^ rptg_node_supply.
rptg_get_arc_supply(G) = G ^ rptg_arc_supply.
rptg_get_nodemap(G) = G ^ rptg_nodemap.
rptg_get_arcmap(G) = G ^ rptg_arcmap.
rptg_get_edgemap(G) = G ^ rptg_edgemap.

rptg_set_node_supply(NS, !G) :-
    !:G = !.G ^ rptg_node_supply := NS.
rptg_set_arc_supply(AS, !G) :-
    !:G = !.G ^ rptg_arc_supply := AS.
rptg_set_nodemap(N, !G) :-
    !:G = !.G ^ rptg_nodemap := N.
rptg_set_arcmap(A, !G) :-
    !:G = !.G ^ rptg_arcmap := A.
rptg_set_edgemap(E, !G) :-
    !:G = !.G ^ rptg_edgemap := E.

rptg_node_content_get_vars(NC) = NC ^ rptg_nc_vars.
rptg_node_content_get_region_name(NC) = NC ^ rptg_nc_reg_var_name.
rptg_node_content_get_merged_from(NC) = NC ^ rptg_nc_merged_from.
rptg_node_content_get_node_type(NC) = NC ^ rptg_nc_node_type.

rptg_get_next_node_number(G) = NextNodeNumber :-
    NodeSupply = rptg_get_node_supply(G),
    counter.allocate(NextNodeNumber, NodeSupply, _).

    % After adding a node with Content into the graph, we need to update the
    % Content so that the merged_from set of the node contains itself.
    % Doing it this way is not completely satisfied because we are adding a
    % node with the given content but we change the content after all.
    % But without adding the node first, the node is nonexistant and we
    % cannot make add it to the merged_from set.
    %
rptg_set_node(Content0, rptg_node(N), !G) :-
    NS0 = rptg_get_node_supply(!.G),
    counter.allocate(N, NS0, NS),
    rptg_set_node_supply(NS, !G),

    % make the merged_from set contain this node
    set.init(MergedFrom0),
    svset.insert(rptg_node(N), MergedFrom0, MergedFrom1),
    rptg_node_content_set_merged_from(MergedFrom1, Content0, Content),

    NodeMap0 = rptg_get_nodemap(!.G),
    svmap.set(rptg_node(N), Content, NodeMap0, NodeMap),
    rptg_set_nodemap(NodeMap, !G),

    % set edge
    EdgeMap0 = rptg_get_edgemap(!.G),
    svmap.set(rptg_node(N), map.init, EdgeMap0, EdgeMap),
    rptg_set_edgemap(EdgeMap, !G).

rptg_node_contents(G, N) = NodeContents :-
    map.lookup(rptg_get_nodemap(G), N, NodeContents).

rptg_get_nodes(G) = Nodes :-
    map.keys(rptg_get_nodemap(G), Nodes).

rptg_successors(G, N) = Successors :-
    EdgeMap = rptg_get_edgemap(G),
    map.lookup(EdgeMap, N, OutEdges),
    map.values(OutEdges, SuccessorList),
    set.list_to_set(SuccessorList, Successors).

rptg_set_edge(Start, End, Info, Arc, !G) :-
    AS0 = rptg_get_arc_supply(!.G),
    counter.allocate(A, AS0, AS),
    rptg_set_arc_supply(AS, !G),

    Arc = rptg_arc(A),

    ArcMap0 = rptg_get_arcmap(!.G),
    map.set(ArcMap0, Arc, rptg_arc_info(Start, End, Info), ArcMap),
    rptg_set_arcmap(ArcMap, !G),

    % register into edge set of the Start node
    EdgeMap0 = rptg_get_edgemap(!.G),
    map.lookup(EdgeMap0, Start, OutEdges0),
    map.set(OutEdges0, Arc, End, OutEdges),
    map.set(EdgeMap0, Start, OutEdges, EdgeMap),
    rptg_set_edgemap(EdgeMap, !G).

rptg_arc_contents(G, Arc, S, E, Content) :-
    ArcMap = rptg_get_arcmap(G),
    map.lookup(ArcMap, Arc, I),
    I = rptg_arc_info(S, E, Content).

rptg_path(G, S, E, Path) :-
    rptg_path_2(G, S, E, [], Path).

:- pred rptg_path_2(rpt_graph, rptg_node, rptg_node, list(rptg_node),
    list(rptg_arc)).
:- mode rptg_path_2(in, in, in, in, out) is nondet.
:- mode rptg_path_2(in, in, out, in, out) is nondet.

rptg_path_2(G, S, E, Nodes0, Path) :-
    EdgeMap = rptg_get_edgemap(G),
    map.lookup(EdgeMap, S, OutEdgesOfS),
    (
        map.member(OutEdgesOfS, A, E),
        \+ list.member(E, Nodes0),
        Path = [A]
    ;
        map.member(OutEdgesOfS, A, N),
        \+ list.member(N, Nodes0),
        rptg_path_2(G, N, E, [N | Nodes0], Path0),
        Path = [A | Path0]
    ).

    % Find a node that is reachable from Start and has type EType.
    % If not found, fails.
    %
reachable_and_having_type(Graph, Start, EType, E) :-
    rptg_lookup_node_type(Graph, Start) = Type,
    ( if
        Type = EType
      then
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
    EdgeMap = rptg_get_edgemap(Graph),
    map.lookup(EdgeMap, StartNode, OutArcs),
    map.values(OutArcs, Ends),
    ( if
        find_node_with_same_type(Ends, Graph, EType, E1)
      then
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
    ( if
        NType = Type
      then
        End = N
      else
        find_node_with_same_type(Ns, Graph, Type, End)
    ).

rptg_node_content_set_vars(Vars, !NC) :-
    !:NC = !.NC ^ rptg_nc_vars := Vars.
rptg_node_content_set_region_name(Name, !NC) :-
    !:NC = !.NC ^ rptg_nc_reg_var_name := Name.
rptg_node_content_set_merged_from(Nodes, !NC) :-
    !:NC = !.NC ^ rptg_nc_merged_from := Nodes.
rptg_node_content_set_node_type(NodeType, !NC) :-
    !:NC = !.NC ^ rptg_nc_node_type := NodeType.

rptg_arc_content_get_label(AC) = AC ^ rptg_ac_label.
rptg_arc_content_set_label(Label, !AC) :-
    !.AC ^ rptg_ac_label := Label = !:AC.

get_node_by_region_name(Graph, RegName, Node) :-
    % from all nodes in the graph find a node corresponding to the
    % region name
    AllNodes = rptg_get_nodes(Graph),
    ( if
        get_node_by_region_name_from_list(Graph, AllNodes, RegName, NodePrime)
      then
        Node = NodePrime
      else
        unexpected(this_file, "get_node_by_region_name: No such a node exists")
    ).

:- pred get_node_by_region_name_from_list(rpt_graph::in, list(rptg_node)::in,
    string::in, rptg_node::out) is semidet.

get_node_by_region_name_from_list(Graph, List, RegName, Node) :-
    List = [ANode | Rest],
    NodeContents = rptg_node_contents(Graph, ANode),
    ( if
        NodeContents ^ rptg_nc_reg_var_name = RegName
      then
        Node = ANode
      else
        get_node_by_region_name_from_list(Graph, Rest, RegName, Node)
    ).

    % find a node in the graph using a set of variables.
    % Because a variable is assigned to only one node.
    %
get_node_by_vars(Graph, Vars, Node) :-
    AllNodes = rptg_get_nodes(Graph),
    ( if
        get_node_by_vars_from_list(Graph, AllNodes, Vars, NodePrime)
      then
        Node = NodePrime
      else
        unexpected(this_file, "get_node_by_vars: No such a node exists")
    ).

:- pred get_node_by_vars_from_list(rpt_graph::in, list(rptg_node)::in,
    set(prog_var)::in, rptg_node::out) is semidet.

get_node_by_vars_from_list(Graph, List, Vars, Node) :-
    List = [ANode | Rest],
    NodeContents = rptg_node_contents(Graph, ANode),
    ( set.subset(Vars, NodeContents ^ rptg_nc_vars) ->
        Node = ANode
    ;
        get_node_by_vars_from_list(Graph, Rest, Vars, Node)
    ).

    % find a node in the graph using a variable assigned to it.
    % simply call get_node_by_vars.
    %
get_node_by_variable(Graph, Var, Node) :-
    % make a set(prog_var) containing the variable
    set.init(Vars0),
    set.insert(Vars0, Var, Vars),
    % find the node
    get_node_by_vars(Graph, Vars, Node).

get_node_by_node(Graph, Node, MergedNode) :-
    % first the node in the NodeMap
    NodeMap = rptg_get_nodemap(Graph),
    ( map.search(NodeMap, Node, _NodeContent) ->
        MergedNode = Node
    ;
        % not directly in the NodeMap, checked if it has been merged
        AllNodes = rptg_get_nodes(Graph),
        ( get_node_by_node_from_list(Graph, AllNodes, Node, MergedNode0) ->
            MergedNode = MergedNode0
        ;
            unexpected(this_file, "get_node_by_node: No such a node exists")
        )
    ).

:- pred get_node_by_node_from_list(rpt_graph::in, list(rptg_node)::in,
    rptg_node::in, rptg_node::out) is semidet.

get_node_by_node_from_list(Graph, [N | Ns], Node, MergedNode) :-
    NodeContent = rptg_node_contents(Graph, N),
    ( set.member(Node, NodeContent ^ rptg_nc_merged_from) ->
        MergedNode = N
    ;
        get_node_by_node_from_list(Graph, Ns, Node, MergedNode)
    ).

rptg_lookup_region_name(Graph, Node) =
    rptg_node_contents(Graph, Node) ^ rptg_nc_reg_var_name.

rptg_lookup_node_type(Graph, Node) =
    rptg_node_contents(Graph, Node) ^ rptg_nc_node_type.

%-----------------------------------------------------------------------------%
%
% The two graph-manipulating operators, i.e., unify and edge.
%

unify_operator(Node1, Node2, !Graph) :-
    % The vars need to be unioned.
    NodeMap0 = rptg_get_nodemap(!.Graph),
    NodeContent1 = rptg_node_contents(!.Graph, Node1),
    NodeContent2 = rptg_node_contents(!.Graph, Node2),

    set.union(NodeContent1 ^ rptg_nc_vars, NodeContent2 ^ rptg_nc_vars,
        UnionVars),
    rptg_node_content_set_vars(UnionVars, NodeContent1, NewContent0),
    set.union(NodeContent1 ^ rptg_nc_merged_from,
        NodeContent2 ^ rptg_nc_merged_from, UnionMergedFrom),
    rptg_node_content_set_merged_from(UnionMergedFrom,
        NewContent0, NewContent1),
    map.det_update(NodeMap0, Node1, NewContent1, NodeMap1),

    rptg_set_nodemap(NodeMap1, !Graph),

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
    EdgeMap = rptg_get_edgemap(!.Graph),
    map.lookup(EdgeMap, Node2, OutEdges2Map),
    map.keys(OutEdges2Map, OutArcs),
    % Transfer them to node 1
    transfer_out_edges_2(OutArcs, Node1, !Graph).

    % This predicate receives a list of out-edges of node2 and returns a
    % graph with all the edges in the list copied to Node1, but it
    % maintains the invariant that "there is only one arc with a
    % specific label from a specific node to another specific node".
    % The algorithm is as follows.
    % for each arc (Node2, Content, Node) in OutArcs2 list:
    %   if (Node1, Content, Node) exists
    %       ignore the arc
    %   else
    %       copy the arc to Node1.
    %
:- pred transfer_out_edges_2(list(rptg_arc)::in, rptg_node::in,
    rpt_graph::in, rpt_graph::out) is det.

transfer_out_edges_2([], _, !Graph).
transfer_out_edges_2([Arc | Arcs], Node1, !Graph) :-
    rptg_arc_contents(!.Graph, Arc, _Node2, Node, ArcContent),
    ( edge_in_graph(Node1, ArcContent, Node, !.Graph) ->
        true
    ;
        % not existed, copy the Arc as an out-edge of Node1
        rptg_set_edge(Node1, Node, ArcContent, _Arc, !Graph)
    ),
    transfer_out_edges_2(Arcs, Node1, !Graph).

    % This predicate receives a graph and returns a new graph in which
    % all the in-edges of node2 in the first graph are copied as in-edges
    % of node1.
    %
:- pred transfer_in_edges(rptg_node::in, rptg_node::in, rpt_graph::in,
    rpt_graph::out) is det.

transfer_in_edges(Node1, Node2, !Graph) :-
    % in-edges of node 2
    rptg_get_in_arcs(!.Graph, Node2, InArcs),
    % copy them to node 1
    transfer_in_edges_2(InArcs, Node1, !Graph).

    % Finding incoming arcs to a node is not direct as finding outcoming
    % ones, we have to scan all the arcs in the graph and explicitly
    % check their ending node.
    %
:- pred rptg_get_in_arcs(rpt_graph::in, rptg_node::in, list(rptg_arc)::out)
    is det.

rptg_get_in_arcs(Graph, Node, Arcs) :-
    ArcMap = rptg_get_arcmap(Graph),
    map.foldl(arc_points_to_node(Node), ArcMap, [], Arcs).

:- pred arc_points_to_node(rptg_node::in, rptg_arc::in, rptg_arc_info::in,
    list(rptg_arc)::in, list(rptg_arc)::out) is det.

arc_points_to_node(End, Arc, ArcInfo, !L) :-
    ArcInfo = rptg_arc_info(_S, E, _C),
    ( E = End ->
        !:L = [Arc | !.L]
    ;
        true
    ).

    % This predicate is very similar to transfer_out_edges_2, except that
    % the arcs now point to Node1, instead of going out from it.
    %
:- pred transfer_in_edges_2(list(rptg_arc)::in, rptg_node::in, rpt_graph::in,
    rpt_graph::out) is det.

transfer_in_edges_2([], _, !Graph).
transfer_in_edges_2([Arc | Arcs], Node1, !Graph) :-
    rptg_arc_contents(!.Graph, Arc, Node, _Node2, ArcContent),
    ( edge_in_graph(Node, ArcContent, Node1, !.Graph) ->
        true
    ;
        % No, copy the Arc as an in-edge of Node1
        rptg_set_edge(Node, Node1, ArcContent, _Arc, !Graph)
    ),
    transfer_in_edges_2(Arcs, Node1, !Graph).

    % Delete a node from the graph.
    % We also need to delete all the edges and arcs from the Node,
    % and delete all edges and arcs to the Node.
    %
:- pred delete_node(rptg_node::in, rpt_graph::in, rpt_graph::out) is det.

delete_node(Node, Graph0, Graph) :-
    Graph0 = rpt_graph(NS, AS, NodeMap0, ArcMap0, EdgeMap0),
    map.delete(NodeMap0, Node, NodeMap),
    delete_all_outedges_and_arcs(Node, ArcMap0, ArcMap1, EdgeMap0, EdgeMap1),
    delete_all_inedges_and_arcs(Node, ArcMap1, ArcMap, EdgeMap1, EdgeMap),
    Graph = rpt_graph(NS, AS, NodeMap, ArcMap, EdgeMap).

    % This predicate deletes all the edges which start from the input
    % node Node.
    % Note: it works as a helper for delete_node so it does not proceed
    % on a graph but on the edge map and the arc map.
    %
:- pred delete_all_outedges_and_arcs(rptg_node::in,
    map(rptg_arc, rptg_arc_info)::in,
    map(rptg_arc, rptg_arc_info)::out,
    map(rptg_node, map(rptg_arc, rptg_node))::in,
    map(rptg_node, map(rptg_arc, rptg_node))::out) is det.

delete_all_outedges_and_arcs(Node, !ArcMap, !EdgeMap) :-
    % Delete the corresponding arcs from arc map.
    map.lookup(!.EdgeMap, Node, EdgesFromNodeMap),
    map.keys(EdgesFromNodeMap, OutArcs),
    svmap.delete_list(OutArcs, !ArcMap),
    % Delete all outcoming edges of the Node from edge map.
    svmap.delete(Node, !EdgeMap).

    % This predicate deletes all the incoming edges of the input node (Node).
    % We only store outcoming edges therefore to remove incoming ones of Node
    % we need to check all the outcoming edges and remove those point to Node.
    %
:- pred delete_all_inedges_and_arcs(rptg_node::in,
    map(rptg_arc, rptg_arc_info)::in,
    map(rptg_arc, rptg_arc_info)::out,
    map(rptg_node, map(rptg_arc, rptg_node))::in,
    map(rptg_node, map(rptg_arc, rptg_node))::out) is det.

delete_all_inedges_and_arcs(Node, !ArcMap, !EdgeMap) :-
    map.keys(!.EdgeMap, StartNodes),

    % For each node: find the outcoming edges from it
    % and delete ones pointing to Node.
    delete_all_inedges_and_arcs_2(StartNodes, Node, !ArcMap, !EdgeMap).

    % This predicate receives a node (Node) and a list of (start) nodes.
    % It deletes all the start nodes' outcoming edges and corresponding arcs
    % which point to the Node.
    %
:- pred delete_all_inedges_and_arcs_2(list(rptg_node)::in, rptg_node::in,
    map(rptg_arc, rptg_arc_info)::in, map(rptg_arc, rptg_arc_info)::out,
    map(rptg_node, map(rptg_arc, rptg_node))::in,
    map(rptg_node, map(rptg_arc, rptg_node))::out) is det.

delete_all_inedges_and_arcs_2([], _, !ArcMap, !EdgeMap).
delete_all_inedges_and_arcs_2([N | Ns], Node, !ArcMap, !EdgeMap) :-
    % Find the mapping with end node = Node, there will be only one,
    % but we do as below because inverse_search is non-det.
    map.lookup(!.EdgeMap, N, EdgesFromN0),
    solutions(map.inverse_search(EdgesFromN0, Node), ArcsFromNPointToNode),

    svmap.delete_list(ArcsFromNPointToNode, !ArcMap),
    svmap.delete_list(ArcsFromNPointToNode, EdgesFromN0, EdgesFromN),
    svmap.set(N, EdgesFromN, !EdgeMap),
    delete_all_inedges_and_arcs_2(Ns, Node, !ArcMap, !EdgeMap).

edge_operator(Start, End, Info, !G) :-
    rptg_set_edge(Start, End, Info, _Arc, !G).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% For finding and checking edges in graph.
%

find_arc_from_node_with_same_label(N, ArcContent, G, M) :-
    EdgeMap = rptg_get_edgemap(G),
    map.lookup(EdgeMap, N, OutEdges),
    map.keys(OutEdges, OutArcs),
    find_arc_with_same_label(ArcContent, OutArcs, G, M).

:- pred find_arc_with_same_label(rptg_arc_content::in,
    list(rptg_arc)::in, rpt_graph::in, rptg_node::out) is semidet.

find_arc_with_same_label(ArcContent, [Arc | Arcs], G, M) :-
    rptg_arc_contents(G, Arc, _N, M0, ArcContent0),
    ( ArcContent0 = ArcContent ->
        M = M0
    ;
        find_arc_with_same_label(ArcContent, Arcs, G, M)
    ).

edge_in_graph(Start, Label, End, Graph) :-
    EdgeMap = rptg_get_edgemap(Graph),
    map.lookup(EdgeMap, Start, OutEdges),
    solutions(map.inverse_search(OutEdges, End), ArcList),
    find_arc_with_same_label(Label, ArcList, Graph, _).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Equality of region points-to graphs.
%

rptg_equal(Graph1, Graph2) :-
    Graph1 = rpt_graph(NS1, AS1, Nodes1, Arcs1, Edges1),
    Graph2 = rpt_graph(NS2, AS2, Nodes2, Arcs2, Edges2),
    NS1 = NS2,
    AS1 = AS2,
    simple_map_equal(Nodes1, Nodes2),
    simple_map_equal(Arcs1, Arcs2),
    complex_map_equal(Edges1, Edges2).

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

    % This predicate finds all regions that are reachable from X.
    % The regions must be reached by edges with labels (type selectors)
    % which are valid with the type of X.
    %
reach_from_a_variable(Graph, HLDS, ProcInfo, X, !Reach_X) :-
    get_node_by_variable(Graph, X, N_X),
    Node_Selector = pair(N_X, []),

    proc_info_get_vartypes(ProcInfo, VarTypes),
    map.lookup(VarTypes, X, TypeX),

    % Find regions reached from X.
    reach_from_a_variable_2([Node_Selector], Graph, HLDS,
        TypeX, [], !Reach_X).

    % This predicate receives a (remembered) list of nodes that are
    % reached from X, along with the valid selectors to those nodes
    % from the node of X.
    % Algorithm:
    %   1. each node is recorded into the reach_from_x set,
    %   2. if an target of a node's out-arc can be reached by a valid
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
    svset.insert(Node, !Reach_X),

    % Add the Node to processed list so that we do not have to deal with
    % it more than once. (Node is not yet in Processed0 because if it
    % is in there it will not be in the to-be-processed list.
    Processed = [Node | Processed0],

    % Take out-arcs of the Node and update the remembered list
    EdgeMap = rptg_get_edgemap(Graph),
    map.lookup(EdgeMap, Node, OutEdges),
    map.keys(OutEdges, OutArcs),
    list.foldl(
        update_remembered_list(Selector, HLDS, TypeX, Graph, Processed),
        OutArcs, Node_Selectors0, Node_Selectors),

    reach_from_a_variable_2(Node_Selectors, Graph, HLDS, TypeX,
        Processed, !Reach_X).

    % A target is remembered as reachable from X if its arc's selector
    % is valid. The remembered list is appended, so it is a breadth-first
    % process.
    %
:- pred update_remembered_list(selector::in, module_info::in,
    mer_type::in, rpt_graph::in, list(rptg_node)::in, rptg_arc::in,
    assoc_list(rptg_node, selector)::in,
    assoc_list(rptg_node, selector)::out) is det.
update_remembered_list(Selector0, HLDS, TypeX, Graph, Processed, OutArc,
        !List) :-
    rptg_arc_contents(Graph, OutArc, _Start, End, ArcContent),
    ArcSelector = ArcContent ^ rptg_ac_label,
    Selector = Selector0 ++ ArcSelector,
    ( check_type_of_node(HLDS, TypeX, Selector) ->
        % The arc's selector is a valid one.
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

:- func this_file = string.

this_file = "rbmm.points_to_graph.m".

%-----------------------------------------------------------------------------%
