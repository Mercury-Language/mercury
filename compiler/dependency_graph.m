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
% of this graph. This list of SCCs is the dependency_ordering, the other
% component in a dependency_info.
%
%-----------------------------------------------------------------------%

:- module libs.dependency_graph.
:- interface.

:- import_module digraph.
:- import_module list.

%-----------------------------------------------------------------------%

    % A dependency_info contains a dependency_graph and a dependency_ordering.
    % Calling make_dependency_info on a graph fills in the dependency_ordering.
    %
:- type dependency_info(T).

:- type dependency_graph(T)     == digraph(T).
:- type dependency_graph_key(T) == digraph_key(T).

    % A dependency ordering gives the list of SCCs of the analysed entities.
    % (For hlds_dependency_graph.m, this will be all the predicates or all
    % the procedure of the module being compiled.)
    %
    % The list is in ascending order: the lowest SCC is first,
    % the highest SCC is last.
    %
:- type dependency_ordering(T)  == list(list(T)).

%-----------------------------------------------------------------------%

:- func make_dependency_info(digraph(T)) = dependency_info(T).

%-----------------------------------------------------------------------%

:- func dependency_info_get_graph(dependency_info(T))
    = dependency_graph(T). 
:- func dependency_info_get_ordering(dependency_info(T))
    = dependency_ordering(T).

    % This function does the same job as dependency_info_get_ordering,
    % except that it condenses all the nodes into a single list.
    % This is useful when the caller wants to process entities bottom up,
    % but the SCC boundaries are not relevant.
    %
:- func dependency_info_get_condensed_ordering(dependency_info(T)) = list(T).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module set.

%-----------------------------------------------------------------------%

:- type dependency_info(T)
    --->    dependency_info(
                dep_graph       :: dependency_graph(T),
                dep_ord         :: dependency_ordering(T)
            ).

make_dependency_info(Graph) = dependency_info(Graph, Ordering) :-
    digraph.atsort(Graph, Ordering0),
    sets_to_lists(Ordering0, [], Ordering).

:- pred sets_to_lists(list(set(T))::in, list(list(T))::in,
    list(list(T))::out) is det.

sets_to_lists([], Xs, Xs).
sets_to_lists([X | Xs], Ys, Zs) :-
    set.to_sorted_list(X, Y),
    sets_to_lists(Xs, [Y | Ys], Zs).

%-----------------------------------------------------------------------%

dependency_info_get_graph(DepInfo) = DepInfo ^ dep_graph.
dependency_info_get_ordering(DepInfo) = DepInfo ^ dep_ord.

dependency_info_get_condensed_ordering(DepInfo) =
    list.condense(dependency_info_get_ordering(DepInfo)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
