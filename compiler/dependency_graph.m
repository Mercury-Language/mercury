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
% hlds.dependency_graph.m and mlds.dependency_graph.m make use of them.
%
% A dependency_graph records which procedures depend on which other
% procedures. It is defined as a digraph (see hlds_module.m) R where
% edge x -> y means that the definition of x depends on the definition of y.
% Imported procedures are not normally included in a dependency_graph
% (although opt_imported procedures are included).
%
% The other important structure is the dependency_ordering which is
% a list of the cliques (strongly-connected components) of this graph,
% in topological order. This is very handy for doing fixpoint iterations.
%
%-----------------------------------------------------------------------%

:- module libs.dependency_graph.
:- interface.

:- import_module digraph.
:- import_module list.

%-----------------------------------------------------------------------%

    % A dependency ordering gives the list of SCCs of the module. The list
    % is in ascending order: the lowest SCC is first, the highest SCC is last.
:- type dependency_ordering(T)  == list(list(T)).

:- type dependency_graph(T)     == digraph(T).

:- type dependency_graph_key(T) == digraph_key(T).

:- type dependency_info(T).

%-----------------------------------------------------------------------%

:- func make_dependency_info(digraph(T)) = dependency_info(T).

%-----------------------------------------------------------------------%

:- func dependency_info_get_graph(dependency_info(T)) = dependency_graph(T).

:- func dependency_info_get_ordering(dependency_info(T)) =
    dependency_ordering(T).

    % Same as the above, except all the nodes are condensed into a single
    % list.  This is useful when processing them in order is important, but
    % processing whole SCCs or knowing the SCC boundaries is not.
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

    % TODO: These could be improved to reduce the amount of intermedediate
    % structures created.
    %
dependency_info_get_ordering(DepInfo) = DepInfo ^ dep_ord.

dependency_info_get_condensed_ordering(DepInfo) =
    list.condense(dependency_info_get_ordering(DepInfo)).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
