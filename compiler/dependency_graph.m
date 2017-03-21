%-----------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------%
% Copyright (C) 2017 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------%
%
% File: dependency_graph.m.
%
% These are generic types and predicates for managing a dependency graph.
% hlds_dependency_graph.m uses it, and if we need an mlds_dependency_graph.m,
% that would use it too.
%
% A dependency_graph records which entities depend on which other entities.
% (For hlds_dependency_graph.m, the entities are HLDS procedures.)
% It is defined as a digraph R where the presence of an edge x -> y means that
% the definition of x depends on the definition of y.
%
% The reason why we build the dependency graph is because from it,
% we can compute the list of the SCCs (strongly-connected components)
% of this graph. This list of SCCs is the bottom_up_dependency_sccs, the other
% component in a dependency_graph itself.
%
%-----------------------------------------------------------------------%

:- module libs.dependency_graph.
:- interface.

:- import_module assoc_list.
:- import_module digraph.
:- import_module list.
:- import_module map.
:- import_module set.

%-----------------------------------------------------------------------%

    % A dependency_info contains
    %
    % - a dependency_graph,
    % - a list of the arcs in that graph, in which the same arc
    %   may appear more than once), and
    % - a dependency_ordering.
    %
    % Calling make_dependency_info on a graph fills in the dependency_ordering.
    %
:- type dependency_info(T).

:- type dependency_graph(T)     == digraph(T).
:- type dependency_graph_key(T) == digraph_key(T).
:- type dependency_arcs(T)      == assoc_list(digraph_key(T), digraph_key(T)).
:- type scc_map(T)              == map(T, set(T)).

    % A dependency ordering gives the list of SCCs of the analysed entities.
    % (For hlds_dependency_graph.m, this will be all the predicates or all
    % the procedure of the module being compiled.)
    %
    % The list is in ascending order: the lowest SCC is first,
    % the highest SCC is last.
    %
:- type bottom_up_dependency_sccs(T)  == list(set(T)).

%-----------------------------------------------------------------------%

:- func make_dependency_info(digraph(T), dependency_arcs(T))
    = dependency_info(T).

%-----------------------------------------------------------------------%

:- func dependency_info_get_graph(dependency_info(T))
    = dependency_graph(T).
:- func dependency_info_get_arcs(dependency_info(T))
    = dependency_arcs(T).
:- func dependency_info_get_bottom_up_sccs(dependency_info(T))
    = bottom_up_dependency_sccs(T).

    % This function does the same job as dependency_info_get_ordering,
    % except that it condenses all the nodes into a single list.
    % This is useful when the caller wants to process entities bottom up,
    % but the SCC boundaries are not relevant.
    %
:- func dependency_info_get_condensed_bottom_up_sccs(dependency_info(T))
    = list(T).

    % Build a map from each node to its SCC.
    %
:- func dependency_info_make_scc_map(dependency_info(T)) = scc_map(T).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------%

:- type dependency_info(T)
    --->    dependency_info(
                dep_graph           :: dependency_graph(T),
                dep_arcs            :: dependency_arcs(T),
                dep_bottom_up_sccs  :: bottom_up_dependency_sccs(T)
            ).

make_dependency_info(Graph, Arcs) = DepInfo :-
    % digraph.atsort puts the cliques of parents before the cliques
    % of their children. This is a top down order.
    digraph.atsort(Graph, TopDownOrdering),
    list.reverse(TopDownOrdering, BottomUpOrdering),
    DepInfo = dependency_info(Graph, Arcs, BottomUpOrdering).

%-----------------------------------------------------------------------%

dependency_info_get_graph(DepInfo) = DepInfo ^ dep_graph.
dependency_info_get_arcs(DepInfo) = DepInfo ^ dep_arcs.
dependency_info_get_bottom_up_sccs(DepInfo) = DepInfo ^ dep_bottom_up_sccs.

dependency_info_get_condensed_bottom_up_sccs(DepInfo) = CondensedOrder :-
    ListOfSets = dependency_info_get_bottom_up_sccs(DepInfo),
    list.map(set.to_sorted_list, ListOfSets, ListOfLists),
    list.condense(ListOfLists, CondensedOrder).

%-----------------------------------------------------------------------%

dependency_info_make_scc_map(DepInfo) = SCCMap :-
    SCCs = dependency_info_get_bottom_up_sccs(DepInfo),
    foldl(make_scc_map, SCCs, init, SCCMap).

:- pred make_scc_map(set(T)::in, map(T, set(T))::in, map(T, set(T))::out)
    is det.

make_scc_map(SCC, !Map) :-
    fold((pred(Node::in, Map0::in, Map::out) is det :-
            map.det_insert(Node, SCC, Map0, Map)
        ), SCC, !Map).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
